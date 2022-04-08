
# in this code I often talk about predators and prey as synonyms for the 
# upper and lower levels of the bipartite network, but thats just to 
# make it easier to read. Its a generic bipartite network

# Setup -------------------------------------------------------------------

library(dplyr)
library(tidyr)

# set the random seed so that while our analyses are random, 
# they're reproducible
set.seed(12345)

field_data_gen <- function(n_upper_individuals, n_upper_species,
                        n_lower_individuals, n_lower_species,
                        prop_realised_interactions){
  # function to generate edgelists based off of a small number of variables.
  # In its current incarnation, all species will be similar in number of individuals,
  # and all interactions will be similarly likely. It may be worth changing this at some point
  

  # Generate metadata for both levels -----------------------------------

  upper_species <- LETTERS[1:n_upper_species]
  
  upper_individuals <- tibble(upper_id = seq(1,n_upper_individuals),
                              # with equal probability, randomly assign each of our individuals 
                              # to one of our species
                              upper_sp = sample(upper_species, 
                                                size = n_upper_individuals, 
                                                replace = T),
                              upper_full_name = paste(upper_id, upper_sp, sep = '_')
  )
  
  
  # make all the possible combinations of two letters
  lower_species <- expand_grid(l1 = letters, l2 = letters) %>%
    # turn them into strings
    mutate(lower_sp = paste0(l1, l2)) %>%
    # select only as many rows as we need
    top_n(n_lower_species) %>%
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
  

  # Make an edgelist --------------------------------------------------------

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
  
  return(full_dataset)
}




data_transform <- function(input_tib){
  # this function turns our 'field' data into something resembling the data we 
  # may actually be faced with
  
  # Firstly, while we now have 'real' data (i.e. something that may actually occur
  # in reality in the field, but in metabarcoding data we'll never know if 
  # a detection of a species in an interaction sample (e.g. detecting
  # prey species inside a predators poo) is from multiple individuals 
  # of the same species, or one. We just know that we got x reads of 
  # taxa y. So we need our analyses to take place on that
  
  mbc_dataset <- input_tib %>%
    group_by(upper_full_name, upper_sp, lower_sp) %>%
    summarise(interaction_strength = sum(interaction_strength))
  
  # make a tib for if it was FOO
  FOO_tib <- mbc_dataset %>%
    group_by(upper_sp, lower_sp) %>%
    summarise(weighting = n())
  
  # now for if it was 'wfoo' in Deagle et als parlance (if an individual ate 5 things,
  # each of those interactions is 1/5 = 0.2)
  
  wFOO_tib <- mbc_dataset %>%
    group_by(upper_full_name) %>%
    # calculate wFOO for each upper individual
    mutate(upper_degree = n(),
           weighting = 1/upper_degree) %>%
    # then combine it by species
    group_by(upper_sp, lower_sp) %>%
    summarise(weighting = sum(weighting))
  
  # here we have if we'd just summed the interaction weights detected
  cumulative_tib <- mbc_dataset %>%
    group_by(upper_sp, lower_sp) %>%
    summarise(weighting = sum(interaction_strength))
  
  
  # now for RRA
  
  RRA_tib <- mbc_dataset %>%
    group_by(upper_full_name) %>%
    # here the weight of the individual predator's interactions
    # are the number of reads counted for that prey species, divided
    # by the total reads detected in the predator
    mutate(predators_total_reads = sum(interaction_strength),
           weighting = interaction_strength / predators_total_reads) %>%
    # we want to scale this to species, however. So we then sum that individual-level
    # weighting by the entire species
    group_by(upper_sp, lower_sp) %>%
    summarise(weighting = sum(weighting))
  
  out_list <- list(mbc = mbc_dataset, 
                   foo = FOO_tib, 
                   wfoo = wFOO_tib, 
                   cumulative = cumulative_tib, 
                   rra = RRA_tib)
  return(out_list)
}

tib_to_matrix <- function(input_tib){
  # make a tibble thats most of the way to being a matrix, but
  # still retains the tibble type and a column of the lower
  # species names, for use in a second
  almost_mat <- input_tib %>% 
    pivot_wider(names_from = upper_sp, 
                values_from = weighting,
                values_fill = 0) 
  
  # remove the lower_sp column and turn it into a matrix
  output_mat <- almost_mat %>%
    select(-lower_sp) %>%
    as.matrix(.)
  
  rownames(output_mat) <- almost_mat$lower_sp
  return(output_mat)
}

field_dataset <- field_data_gen(n_upper_individuals = 100,
                                n_upper_species = 8,
                                n_lower_individuals = 3000,
                                n_lower_species = 100,
                                prop_realised_interactions = 0.001)

trial <- data_transform(field_dataset)

nets <- lapply(trial[-1], tib_to_matrix)

indices <- c('connectance', 'ISA', 'weighted NODF', 'H2')
results_tib <- lapply(nets, function(x) bipartite::networklevel(x, index = indices)) %>%
  bind_rows(.id = 'normalisation_type')

