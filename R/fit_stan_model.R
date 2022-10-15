library(tidyverse)
library(cmdstanr)
## Load model data
load(file = here::here("data", "df", "titre_all_naive.RData")) # titre_all_naive
load(file = here::here("data", "df", "titre_all_exposed.RData")) # titre_all_exposed

info2_levels <- c("Wild type", "Omicron BA1", "Omicron BA2", "Omicron BA5")
file_name_var <- c("wt", "ba1", "ba2", "ba5")
info3_levels <- c("Vac3", "BA1", "BA2")
file_name_type <- c("vac3", "ba1", "ba2")

for (i in 1:4) { # The strains 
    for (j in 1:3) { # The types   
        for (exposure in c("naive", "exposed")) {
   
            info3_levels_str <- paste0(info3_levels[j], " ", exposure)
            file_name_type_2 <- paste0(file_name_type[j], "_", exposure)

            if (exposure == "naive") {
                titre_all_trim <- titre_all_naive %>% filter(info2 == info2_levels[i], type == info3_levels_str) %>% filter(!is.na(info3))
            } else {
                titre_all_trim <- titre_all_exposed %>% filter(info2 == info2_levels[i], type == info3_levels_str) %>% filter(!is.na(info3))
            }
            titre_all_trim_ids <- titre_all_trim$elig_study_id %>% unique
            titre_all_trim <- titre_all_trim %>% mutate(elig_study_id = factor(elig_study_id, levels = titre_all_trim_ids)) %>%
                mutate(elig_study_id = as.numeric(elig_study_id)) %>% filter(!is.na(info3))

            data_list_stan <- list(
                N = nrow(titre_all_trim),
                N_ind = length(titre_all_trim_ids),
                ids = titre_all_trim$elig_study_id,
                t = titre_all_trim$time_until_bleed,
                y = titre_all_trim$info3
            )

            model_A <- cmdstan_model(here::here("src", "stan", "two_line_model.stan"), compile = FALSE)
            m_compile_A <- model_A$compile(dir = here::here("src", "stan"),
                stanc_options = list("O1"))

            fitA <- m_compile_A$sample(
                data = data_list_stan,    # named list of data
                iter_warmup = 1000,          # number of warmup iterations per chain
                iter_sampling = 1000,            # total number of iterations per chain
                chains = 4,            # number of cores (could use one per chain)
                parallel_chains = 4,
                adapt_delta = 0.8,
                max_treedepth = 15,
                refresh = 200
            )
            fitA$save_object(file = here::here("outputs", "stanfits", paste0("fit_", file_name_var[i], "_", file_name_type_2, ".Rdata")))
        }
    }
}