
# in this code I often talk about predators and prey as synonyms for the 
# upper and lower levels of the bipartite network, but thats just to 
# make it easier to read. Its a generic bipartite network

# Setup -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(here)

# load in functions for data generation
source(here('scripts', 'datagen_functions.R'))

# set the random seed so that while our analyses are random, 
# they're reproducible
set.seed(12345)

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

