library(tidyverse)
library(here)
library(lubridate)

#########################################
# Find symptom-only infections #
########################################
add_symptoms_only <- function(raw_legacy_full) {
    symptoms.only <- raw_legacy_full %>% 
        filter(!is.na(episode_number)) %>%
        group_by(elig_study_id, episode_number) %>%
        fill(REDCAP_PCR_test_site, .direction = "updown") %>%
        fill(Sx_severity, .direction = "updown") %>%
        slice_head() %>%
        ungroup() %>%
        filter(str_detect(Sx_severity, "mild") |
                str_detect(Sx_severity, "moderate") |
                str_detect(Sx_severity, "severe")) %>%
        filter(is.na(REDCAP_PCR_test_site))
    
    symptoms.only <- symptoms.only %>%
    mutate(episode_symptoms_only = "symptoms_only")
    
    symptoms.only <- symptoms.only %>%
    select(elig_study_id, episode_number, episode_symptoms_only)
    
    raw_legacy_full <- raw_legacy_full %>%
    left_join(symptoms.only)
  raw_legacy_full
}

############################################################
############### Get times of everything into one dataframe
############################################################

get_df_legacy_raw_annot <- load(here::here("data", "Legacy_DataAnnotatedDateSeries_2022-09-09.RData"))
raw_legacy_full_0909 <- chrono.df

get_df_legacy_raw_annot <- load(here::here("data", "Legacy_DataAnnotatedDateSeries_2022-10-14.RData"))
raw_legacy_full_1014 <- chrono.df

raw_legacy_full_0909 <- raw_legacy_full_0909 %>% add_symptoms_only
raw_legacy_full_1014 <- raw_legacy_full_1014 %>% add_symptoms_only

raw_legacy_full_0909 %>% select(elig_study_id, episode_variant_summarised) %>% unique %>%
    pull(episode_variant_summarised) %>% table
raw_legacy_full_1014 %>% select(elig_study_id, episode_variant_summarised) %>% unique %>%
    pull(episode_variant_summarised) %>% table

raw_legacy_full_0909 %>% filter(episode_variant_summarised == "Omicron-BA.5") %>% 
    pull(ic50_Omicron_BA5) %>% `>`(0) %>% sum(na.rm = TRUE)
raw_legacy_full_1014 %>% filter(episode_variant_summarised == "Omicron-BA.5") %>%
    pull(ic50_Omicron_BA5) %>% `>`(0) %>% sum(na.rm = TRUE)

raw_legacy_full_0909 %>% pull(ic50_Omicron_BA5) %>% `>`(0) %>% sum(na.rm = TRUE)
raw_legacy_full_1014 %>% pull(ic50_Omicron_BA5) %>% `>`(0) %>% sum(na.rm = TRUE)

raw_legacy_full <- raw_legacy_full_1014

legacy_ids_all <- raw_legacy_full %>% pull(elig_study_id) %>% unique


  
#####################
# Get bleed times #
####################
bleed_times_full <- left_join(
    raw_legacy_full %>% arrange(elig_study_id) %>% filter(!is.na(visit_date)) %>%
        select(elig_study_id, calendar_date) %>% group_by(elig_study_id) %>% unique %>% mutate(info1 = row_number(), type = "bleed_time") %>%
        select(elig_study_id, calendar_date, type, info1),

    raw_legacy_full %>% arrange(elig_study_id) %>% filter(!is.na(visit_date)) %>% 
        select(elig_study_id, calendar_date, ic50_wildtype, ic50_Alpha, ic50_Delta, ic50_Omicron_BA1, ic50_Omicron_BA2, ic50_Omicron_BA5) %>% 
        pivot_longer(!c(elig_study_id, calendar_date), names_to = "info2", values_to = "info3") 
)

bleed_times_full <- bleed_times_full %>% mutate(info3 = log2(info3 / 5)) %>%
    mutate(info3 = case_when(   
    info3 == 10 ~ 7,   
    info3 > 3 ~ info3 - 2,
    info3 == 1 ~ 1,
    info3 == 0 ~ 1,
    info3 == 10 ~ 7
    )
    ) %>% 
    mutate(color =  case_when( 
            info3 == 1 ~ "censored",
            info3 == 7 ~ "censored",
            TRUE~ "true",
        )
    )

#########################################
# Get ROCHE-N results #
########################################

roche_times_full <- raw_legacy_full %>% arrange(elig_study_id) %>% filter(!is.na(Roche_N_result)) %>%
    select(elig_study_id, calendar_date, info2 = Roche_N_result) %>% group_by(elig_study_id) %>%
    mutate(info1 = row_number(), type = "roche_result") %>%
    select(elig_study_id, calendar_date, type, info1, info2)

#########################################
# Get vaccination times #
########################################

vac_inter <- raw_legacy_full %>% arrange(elig_study_id) %>% 
    select(elig_study_id, date_dose_1, date_dose_2, date_dose_3, date_dose_4, dose_1, dose_2, dose_3, dose_4) %>% unique 

vac_times_full <- vac_inter %>% select(elig_study_id, date_dose_1, date_dose_2, date_dose_3, date_dose_4) %>% 
    pivot_longer(!elig_study_id, names_to = "info1", values_to = "calendar_date") %>% mutate(info1 = substr(info1, 6, 11)) %>%
left_join(
    vac_inter %>% select(elig_study_id, dose_1, dose_2, dose_3, dose_4) %>% 
        pivot_longer(!elig_study_id, names_to = "info1", values_to = "info2") 
) %>% mutate(type = "vaccine_dose") %>% select(elig_study_id, calendar_date, type, info1, info2) %>%
    mutate(info1 = as.numeric(substr(info1, 6, 6)))

#########################################
# Get known infection times (PCR) #
########################################
inf_times_full <- raw_legacy_full %>% arrange(elig_study_id) %>% filter(!is.na(episode_start)) %>%
    select(elig_study_id, calendar_date = episode_start, info1 = episode_number, info2 = episode_variant_summarised, episode_symptoms_only) %>% unique %>% 
    filter(is.na(episode_symptoms_only)) %>%
    group_by(elig_study_id) %>% mutate(info1 = row_number()) %>%
    mutate(type = "infection") %>% select(elig_study_id, calendar_date, type, info1, info2) 

# Combine!!!
everything_times <- bind_rows(bleed_times_full, roche_times_full, vac_times_full, inf_times_full)

# People with odd entries people
vac_data_problem_id <- c("212", "341", "348", "358", "376", "717", "713", "707", "712", "715")
everything_times <- everything_times %>% filter(!elig_study_id %in% vac_data_problem_id)
save(everything_times, file = "data/df/everything_long.RData")