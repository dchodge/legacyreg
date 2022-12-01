library(tidyverse)
library(cmdstanr)
library(tidybayes)
library(data.table)
library(ggh4x)
library(ggridges)
library(lemon)
library(R.utils)

# sourcing functions file
sourceDirectory("R/")

## Load model data
load(file = here::here("data", "df", "titre_all_naive.RData")) # titre_all_naive
load(file = here::here("data", "df", "titre_all_exposed.RData")) # titre_all_exposed

# the strain of SARS-CoV-2 antibodies in the blood are measured against
info2_levels <- c("Wild type", "Omicron BA1", "Omicron BA2", "Omicron BA5")
file_name_var <- c("wt", "ba1", "ba2", "ba5")

# event type - whether we've filtered individuals based on whether they've had a 3rd vaccine before infection,
# infection with BA1, BA2 or BA5 before any other infections
info3_levels <- c("Vac3", "BA1", "BA2", "BA5")
file_name_type <- c("vac3", "ba1", "ba2", "ba5")

# defining the possible exposure types - needed for the big loop to work
exposure_loop <- c("naive", "exposed")

# defining the population level parameters
pop_level_params <- c("t_p", "boost_i", "boost_s", "wane_s")

# defining the options for the various covariate categories
titre_type_options <- c("Wildtype", "BA.1", "BA.2", "BA.5")
event_type_options <- c("Vaccination", "BA.1 infection",
                        "BA.2 infection", "BA.5 infection")
exposure_type_options <- c("Naive", "Exposed")
