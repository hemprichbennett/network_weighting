# Setup -------------------------------------------------------------------

library(tidyverse)
library(bipartite)
library(here)

input_diet_data <- read_csv(here('data', 'raw_data', 'dummy_data.csv'))

individual_names <- colnames(input_diet_data)[-1]

# make a binary version
individual_binary_data <- input_diet_data %>%
  mutate(across(where(is.numeric), ~ as.integer(.x != 0)))

write_csv(individual_binary_data, file = here('results', 'table_2_binary_individuals.csv'))


# Species-level -----------------------------------------------------------

# calculate input species-level weighting
species_input <- input_diet_data %>%
  pivot_longer(all_of(individual_names), 
               names_to = 'individual', values_to = 'interaction') %>%
  mutate(consumer_sp = gsub('_.+', '', individual)) %>%
  group_by(resource_sp, consumer_sp) %>%
  summarise(interaction_weight = sum(interaction)) %>%
  pivot_wider(names_from = consumer_sp, values_from = interaction_weight) %>%
  select(resource_sp, specialist, intermediate, generalist)

write_csv(species_input, file = here('results', 'table_3_input_species.csv'))

# then do it for binary data
species_FOO_binary <- individual_binary_data %>%
  pivot_longer(all_of(individual_names), 
               names_to = 'individual', values_to = 'interaction') %>%
  mutate(consumer_sp = gsub('_.+', '', individual)) %>%
  group_by(resource_sp, consumer_sp) %>%
  summarise(interaction_weight = sum(interaction)) %>%
  pivot_wider(names_from = consumer_sp, values_from = interaction_weight) %>%
  select(resource_sp, specialist, intermediate, generalist)

write_csv(species_FOO_binary, file = here('results', 'table_4_FOO_species.csv'))


# Bipartite analyses ------------------------------------------------------


## convert datasets to networks --------------------------------------------



species_input_net <- species_input %>%
  ungroup() %>%
  column_to_rownames('resource_sp') %>%
  as.matrix()

species_FOO_net <- species_FOO_binary %>%
  ungroup() %>%
  column_to_rownames('resource_sp') %>%
  as.matrix()



## Do species-level analyses -----------------------------------------------

sp_metrics <- c('species.strength', 
                'weighted.closeness', 'weighted.betweenness')

input_sp_stats <- specieslevel(species_input_net, level = 'higher') %>%
  as_tibble(rownames = 'species') %>%
  mutate(weighting_method = '%FOO')


FOO_sp_stats <- specieslevel(species_FOO_net, level = 'higher') %>%
  as_tibble(rownames = 'species') %>%
  mutate(weighting_method = 'FOO')


all_sp_stats <- bind_rows(input_sp_stats, FOO_sp_stats)

sp_stats_forplot <- all_sp_stats %>%
  pivot_longer(cols = -c(species, weighting_method),
               names_to = 'metric_name',
               values_to = 'metric_result') %>%
  filter(metric_name %in% sp_metrics) %>%
  mutate(metric_name = metric_name %>%
           str_replace_all("\\.", " ") %>%
           str_to_sentence(),
         species = str_to_sentence(species))

sp_stats_plot <- ggplot(sp_stats_forplot, aes(x = species, y = metric_result, fill = weighting_method)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_point(position = position_dodge(width = 0.9), size = 2) +
  scale_fill_viridis_d(name = "Weighting method") +
  facet_grid(metric_name ~ ., scales = "free", switch = "y") +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    strip.placement = "outside",
    strip.background = element_blank(),
    # Specific text tweaks
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )+
  ylab(NULL)+
  xlab('Species')

sp_stats_plot

ggsave(plot = sp_stats_plot, file = here('figures', 'sp_stats_plot.png'), 
       height = 8)



## do network-level analyses -----------------------------------------------



input_net_stats <- networklevel(species_input_net) %>%
  as_tibble(rownames = 'metric') %>%
  mutate(weighting_method = '%FOO')

FOO_net_stats <- networklevel(species_FOO_net) %>%
  as_tibble(rownames = 'metric') %>%
  mutate(weighting_method = 'FOO')

net_stats_forplot <- bind_rows(input_net_stats, FOO_net_stats) %>%
  filter(metric %in% c('H2', 'interaction strength asymmetry', 'weighted NODF', 'modularity Q')) %>%
  mutate(metric = str_to_title(metric),
         metric = gsub('Nodf', 'NODF', metric),
         metric = gsub('Strength Asymmetry', 'Strength\nAsymmetry', metric))

# metric names
FOO_net_stats$metric[order(FOO_net_stats$metric)]

net_stats_plot <- ggplot(net_stats_forplot, aes(x = weighting_method, y = value)) +
  geom_bar(stat = 'identity', position = position_dodge(),colour = "black" ) +
  #geom_point(position = position_dodge(width = 0.9), size = 2) +
  #scale_fill_viridis_d(name = "Weighting method", option = 'B') +
  facet_grid(metric ~ ., scales = "free", switch = "y") +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    strip.placement = "outside",
    strip.background = element_blank(),
    # Specific text tweaks
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )+
  ylab(NULL)+
  xlab('Weighting method')

net_stats_plot

ggsave(plot = net_stats_plot, file = here('figures', 'net_stats_plot.png'),
       height = 8)

# convert the stats to a nice table for the manuscript
net_stats_forplot %>%
  pivot_wider(names_from = weighting_method, values_from = value) %>%
  arrange(metric) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  write_csv(here('results', 'table_5_network_level_stats.csv'))





# Attempt normalisation ---------------------------------------------------

mat_pFOO_norm <- species_input_net / sum(species_input_net)
mat_FOO_norm <- species_FOO_net / sum(species_FOO_net)

pFOO_norm_stats <- networklevel(mat_pFOO_norm) %>%
  as_tibble(rownames = 'metric') %>%
  mutate(weighting_method = 'pFOO')

FOO_norm_stats <- networklevel(mat_FOO_norm) %>%
  as_tibble(rownames = 'metric') %>%
  mutate(weighting_method = 'FOO')


net_stats_norm <- bind_rows(pFOO_norm_stats, FOO_norm_stats) %>%
  filter(metric %in% c('H2', 'interaction strength asymmetry', 'weighted NODF', 'modularity Q'))

net_stats_norm %>%
  pivot_wider(names_from = weighting_method, values_from = value) %>%
  arrange(metric) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  write_csv(here('results', 'table_6_normalised_network_level_stats.csv'))

pdf(file = here('figures', 'figure_3_networks.pdf'))
par(mfrow = c(2,2))
plotweb(species_input_net,
        higher_labels = F,
        lower_labels = F)
title('%FOO')
plotweb(species_FOO_net,
        higher_labels = F,
        lower_labels = F)
title('FOO')
plotweb(mat_pFOO_norm,
        higher_labels = F,
        lower_labels = F)
title('%FOO normalised')
plotweb(mat_FOO_norm,
        higher_labels = F,
        lower_labels = F)
title('FOO normalised ')
dev.off()

