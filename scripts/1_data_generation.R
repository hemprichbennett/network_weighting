
# Setup -------------------------------------------------------------------

library(bipartite)
library(dplyr)
library(tidyr)

# set the random seed so that while our analyses are random, 
# they're reproducible
set.seed(12345)

n_upper_individuals <- 100
n_upper_species <- 8
upper_species <- LETTERS[1:n_upper_species]

upper_individuals <- tibble(upper_id = seq(1,n_upper_individuals),
                            # with equal probability, randomly assign each of our individuals 
                            # to one of our species
                            upper_sp = sample(upper_species, 
                                        size = n_upper_individuals, 
                                        replace = T),
                            upper_full_name = paste(upper_id, upper_sp, sep = '_')
                            )


n_lower_individuals <- 3000
n_lower_species <- 300
# make all the possible combinations of two letters
lower_species <- expand_grid(l1 = letters, l2 = letters) %>%
  # turn them into strings
  mutate(lower_sp = paste0(l1, l2)) %>%
  # select only as many rows as we need
  top_n(n_upper_species) %>%
  # pull those strings into a vector
  pull(lower_sp)

lower_individuals <- tibble(lower_id = seq(1,n_lower_individuals),
                            # with equal probability, randomly assign each of our individuals 
                            # to one of our species
                            lower_sp = sample(lower_species, 
                                        size = n_lower_individuals, 
                                        replace = T),
                            lower_full_name = paste(lower_id, lower_sp, sep = '_')
                            )
# I'll probably need to improve the initial connection and weightings, but
# here's something rough for now
prop_realised_interactions <- 0.0005

possible_edges <- expand_grid(
  # all possible combinations of all possible interacting pairs
  upper_full_name = upper_individuals$upper_full_name,
  lower_full_name = lower_individuals$lower_full_name)

realised_edges <- possible_edges %>%
  # get a random subsample of all rows
  slice_sample(prop = prop_realised_interactions) %>%
  # give the interactions a random strength between 0 and 1
  mutate(interaction_strength = runif(nrow(.)))


# now combine it with the metadata for each individual ID, so we
# can construct networks with different normalisation techniques
full_dataset <- realised_edges %>%
  left_join(upper_individuals) %>%
  left_join(lower_individuals)

# make a tib for if it was FOO
FOO_tib <- full_dataset %>%
  group_by(upper_sp, lower_sp) %>%
  summarise(weighting = n())

FOO_tib %>% pivot_wider(names_from = upper_sp, 
                        values_from = weighting,
                        values_fill = 0)

# here we have if we'd just summed the interaction weights detected
cumulative_tib <- full_dataset %>%
  group_by(upper_sp, lower_sp) %>%
  summarise(weighting = sum(interaction_strength))


# now for FOO
# This codes broken
WFOO_tib <- full_dataset %>%
  group_by(upper_full_name) %>%
  summarise(predators_total_weight = sum(interaction_strength),
            weighting = interaction_strength / predators_total_weight)
