library(tidyverse)
library(cmdstanr)
library(tidybayes)
library(data.table)
library(ggh4x)
library(ggridges)
library(lemon)

# sourcing functions file
source("R/extract.R")
source("R/fit.R")
source("R/plot.R")
source("R/process.R")
source("R/simulate.R")
source("R/stan.R")
source("R/subset.R")
source("R/summarise.R")

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
