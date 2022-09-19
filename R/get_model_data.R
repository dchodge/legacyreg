library(tidyverse)
library(here)
library(lubridate)

load(file = here::here("data", "df", "everything_long.RData")) # everything_times
variants_names <-  c("Wild type", "Alpha", "Delta", "Omicron BA1", "Omicron BA2", "Omicron BA5")

############################################################
############### For infected with a 3rd dose of vaccine
############################################################

# Must have had at least 3 doses of vaccine
## Get ids 
dose3_ids <- everything_times %>% 
    filter(type == "vaccine_dose") %>% filter(!is.na(calendar_date)) %>% 
    filter(info1 == 3) %>% pull(elig_study_id)
everything_times_3dose <- everything_times %>% filter(elig_study_id %in% dose3_ids)

# ids of the vaccinated, get bleed times and vac times of relevant people
dose3_bleed <- everything_times_3dose %>% filter(type == "bleed_time") %>%
    select( elig_study_id, calendar_date, info1) %>% unique
dose3_vac <- everything_times_3dose %>% filter(type == "vaccine_dose") %>% 
    select( elig_study_id, calendar_date, info1, info2) %>% unique %>% 
    filter(!is.na(calendar_date)) %>% filter(info1 == 3) # Timing of 3rd dose matters only
dose3_roche <- everything_times_3dose %>% filter(type == "roche_result") %>% 
    select( elig_study_id, calendar_date, info1, info2) %>% unique %>% 
    filter(!is.na(calendar_date)) %>% arrange(elig_study_id)
dose3_inf <- everything_times_3dose %>% filter(type == "infection") %>% 
    select(elig_study_id, calendar_date, info1, info2) %>% unique # Any infection

# Find those for who inf comes before 3rd dose, can either be PCR-confirmed or from ROCHE test
dose3_inf_roche <- left_join(
    dose3_vac %>% select(elig_study_id, dose3_date = calendar_date),
    dose3_inf %>% filter(info1 == 1) %>% select(elig_study_id, inf_date = calendar_date), by = "elig_study_id") %>% left_join(
    dose3_roche %>% filter(info2 == "positive") %>% group_by(elig_study_id) %>% filter(info1 == min(info1)) %>%
        select(elig_study_id, roche_pos = calendar_date ), by = "elig_study_id"
) %>% mutate(inf_date_combined = case_when(
    inf_date < dose3_date ~ inf_date,
    roche_pos < dose3_date ~ roche_pos
)) %>% select(elig_study_id, inf_date_combined)

dose3_exposed_ids <- dose3_inf_roche %>% filter(!is.na(inf_date_combined)) %>% pull(elig_study_id)
dose3_naive_ids <- setdiff(dose3_ids, dose3_exposed_ids) # Everyone who does not have an infection before dose 3

# Gets the relevant post-vac bleeds, up until end of study or next infection (PCR-confirmed)
get_time_until_inf <- function(ids, bleed_df, vac_df, inf_df)  {

   # ids <- dose3_exposed_ids
   # bleed_df <- dose3_bleed
   # vac_df <- dose3_vac
   # inf_df <- dose3_inf
    comparison_all_dates_vac3_after <- left_join(
        vac_df %>% filter(elig_study_id %in% ids) %>%
            rename(dose3_date = calendar_date, vac_no = info1) %>% select(!info2),
        bleed_df %>% filter(elig_study_id %in% ids) %>% rename(bleed_date = calendar_date, bleed_no = info1), 
            by = "elig_study_id") %>% 
    left_join(
        inf_df %>% rename(inf_date = calendar_date) , by = "elig_study_id"
    ) %>% 
        mutate(date_for_dose3 = case_when(
            (dose3_date < inf_date)~ TRUE,
            TRUE~FALSE
        )) %>% as.data.frame %>% filter(date_for_dose3) %>% 
        mutate(rel_bleeds_bool = case_when(
            (bleed_date > dose3_date) & (bleed_date < inf_date) ~ TRUE,
            TRUE~FALSE
        )) %>%
         filter(rel_bleeds_bool) %>% 
        mutate(days = as.numeric(bleed_date - dose3_date)) %>%
        select(elig_study_id, days, bleed_no, inf_date)
    comparison_all_dates_vac3_after
}


times_post_dose3_exposed <- get_time_until_inf(dose3_exposed_ids, dose3_bleed, dose3_vac, dose3_inf)
times_post_dose3_naive <- get_time_until_inf(dose3_naive_ids, dose3_bleed, dose3_vac, dose3_inf)


###################################################################################
############### For BA1 infection only population
#####################################################################################

# Get all people with BA1 infection
ba1_inf_id <- everything_times_3dose %>% filter(type == "infection") %>%
    filter(info2 == "Omicron-BA.1") %>% select( elig_study_id, calendar_date, info2) %>% unique %>% pull(elig_study_id)

# BA1 case must come after three vac
inf_ba1_b4_dose3 <- left_join(
    everything_times_3dose %>% filter(elig_study_id %in% ba1_inf_id) %>% filter(type == "vaccine_dose") %>%
        filter(info1  == 3) %>% select(elig_study_id, dose3_date = calendar_date),
    everything_times_3dose %>% filter(elig_study_id %in% ba1_inf_id) %>% filter(type == "infection") %>%
        rename(inf_date = calendar_date)
) %>% unique %>% mutate(inf_b4_dose3 = inf_date < dose3_date) %>% filter(inf_b4_dose3) %>%
    filter(info2 == "Omicron-BA.1") %>% pull(elig_study_id)
ba1_inf_id <- setdiff(ba1_inf_id, inf_ba1_b4_dose3)

everything_times_ba1 <- everything_times_3dose %>% filter(elig_study_id %in% ba1_inf_id)

# ids of the vaccinated, get bleed times and vac times of relevant people
ba1_inf_bleed <- everything_times_ba1 %>%
    filter(type == "bleed_time") %>% select( elig_study_id, calendar_date, info1) %>% unique
ba1_inf_vac <- everything_times_ba1 %>%
    filter(type == "vaccine_dose") %>% select( elig_study_id, calendar_date, info1, info2) %>% unique %>% 
    filter(!is.na(calendar_date))
ba1_inf_roche <- everything_times_ba1 %>%
    filter(type == "roche_result") %>% select( elig_study_id, calendar_date, info1, info2) %>% unique %>% 
    filter(!is.na(calendar_date)) %>% arrange(elig_study_id)
ba1_inf_inf <- everything_times_ba1 %>%
    filter(type == "infection") %>% select( elig_study_id, calendar_date, info1, info2) %>% unique %>%
    filter(info2 == "Omicron-BA.1")

ba1_exposed_PCR <- ba1_inf_inf %>% filter(info1 %in% c(2, 3)) %>% pull(elig_study_id) %>% unique
ba1_naive_roche_maybe <- ba1_inf_inf %>% filter(info1 == 1) %>% select(elig_study_id, ba1_date = calendar_date) %>% unique
ba1_exposed_roche <- left_join(ba1_naive_roche_maybe, ba1_inf_roche) %>% filter(info2 == "positive") %>% 
    mutate(pos_before_ba1_date = ba1_date > calendar_date) %>% filter(pos_before_ba1_date) %>% 
    pull(elig_study_id) %>% unique

ba1_exposed_id <- c(ba1_exposed_PCR, ba1_exposed_roche) %>% unique
ba1_naive_id <- setdiff(ba1_inf_id, ba1_exposed_id)

#####
## FOR BA1 FIRST
#####
get_ba_naive <- function(ids, bleed_df, vac_df, inf_df, roche_df) {
    times_post_ba1_naive <- left_join(
        vac_df %>% filter(elig_study_id %in% ids) %>% filter(info1 == 3) %>%
        rename(vac_date = calendar_date, vac_no = info1) %>% select(!info2),
        bleed_df %>% filter(elig_study_id %in% ids) %>% rename(bleed_date = calendar_date, bleed_no = info1)) %>% 
    left_join(
        inf_df %>% filter(elig_study_id %in% ids) %>% rename(ba1_inf_date = calendar_date) %>% select(!c(info1, info2))
    ) %>% 
        mutate(date_for_ba1 = case_when(
            (bleed_date > ba1_inf_date) ~ TRUE,
            TRUE~FALSE
        )) %>% as.data.frame %>% filter(date_for_ba1) %>% 
        mutate(days = as.numeric(bleed_date - ba1_inf_date)) %>%
        select(elig_study_id, days, bleed_no, bleed_date)
    times_post_ba1_naive
}

#####
## FOR BA1 NOT FIRST
#####

get_ba_exposed <- function(ids, bleed_df, vac_df, inf_df, roche_df) {

    #ids <- ba2_exposed_id
   # bleed_df <- ba2_inf_bleed 
   # vac_df <- ba2_inf_vac
   # inf_df <- ba2_inf_inf
   # roche_df <- ba2_inf_roche

    ba1_exposed_inf2 <- inf_df %>% filter(elig_study_id %in% ids) %>% 
        select(elig_study_id, inf2_date = calendar_date)

    # Get pcr dates
    ba1_exposed_A <- inf_df %>% filter(info1 %in% c(2, 3)) %>% pull(elig_study_id) %>% unique
    ba1_exposed_inf1_pcr <- everything_times %>% filter(elig_study_id %in% ba1_exposed_A) %>%
        filter(type == "infection") %>% select( elig_study_id, calendar_date, info1, info2) %>% group_by(elig_study_id) %>%
        filter(info1 == 1) %>% select(elig_study_id, inf1_date = calendar_date)

    # get roche dates
    ba1_naive_A_maybe <- inf_df %>% filter(info1 == 1) %>% select(elig_study_id, ba1_date = calendar_date) %>% unique
    ba1_exposed_inf1_roche <- left_join(ba1_naive_A_maybe, roche_df) %>% filter(info2 == "positive") %>% 
        mutate(pos_before_ba1_date = ba1_date > calendar_date) %>% filter(pos_before_ba1_date) %>% 
        group_by(elig_study_id) %>% filter(info1 == min(info1)) %>%
        select(elig_study_id, inf1_date = calendar_date)

    ba1_exposed_inf1 <- bind_rows(ba1_exposed_inf1_pcr, ba1_exposed_inf1_roche)

    times_post_ba1_exposed <- ba1_exposed_inf1 %>% left_join(ba1_exposed_inf2) %>%     
        left_join(bleed_df %>% filter(elig_study_id %in% ids) %>%
            rename(bleed_date = calendar_date, bleed_no = info1)) %>% 
        mutate(date_for_ba1 = case_when(
            (bleed_date > inf2_date) ~ TRUE,
            TRUE~FALSE
        )) %>% as.data.frame %>% filter(date_for_ba1) %>% 
        mutate(days = as.numeric(bleed_date - inf2_date)) %>%
        select(elig_study_id, days, bleed_no, bleed_date)

}

times_post_ba1_naive <- get_ba_naive(ba1_naive_id, ba1_inf_bleed, ba1_inf_vac, ba1_inf_inf, ba1_inf_roche)
times_post_ba1_exposed <- get_ba_exposed(ba1_exposed_id, ba1_inf_bleed, ba1_inf_vac, ba1_inf_inf, ba1_inf_roche)


###################################################################################
############### For BA2 infection only population
#####################################################################################

# Get all people with BA2 infection
ba2_inf_id <- everything_times_3dose %>% filter(type == "infection") %>%
    filter(info2 == "Omicron-BA.2") %>% select( elig_study_id, calendar_date, info2) %>% unique %>% pull(elig_study_id)

# BA1 case must come after three vac
inf_ba2_b4_dose3 <- left_join(
    everything_times_3dose %>% filter(elig_study_id %in% ba2_inf_id) %>% filter(type == "vaccine_dose") %>%
        filter(info1  == 3) %>% select(elig_study_id, dose3_date = calendar_date),
    everything_times_3dose %>% filter(elig_study_id %in% ba2_inf_id) %>% filter(type == "infection") %>%
        rename(inf_date = calendar_date)
) %>% unique %>% mutate(inf_b4_dose3 = inf_date < dose3_date) %>% filter(inf_b4_dose3) %>%
    filter(info2 == "Omicron-BA.2") %>% pull(elig_study_id)
ba2_inf_id <- setdiff(ba2_inf_id, inf_ba2_b4_dose3)

everything_times_ba2 <- everything_times_3dose %>% filter(elig_study_id %in% ba2_inf_id)

# ids of the vaccinated, get bleed times and vac times of relevant people
ba2_inf_bleed <- everything_times_ba2 %>%
    filter(type == "bleed_time") %>% select( elig_study_id, calendar_date, info1) %>% unique
ba2_inf_vac <- everything_times_ba2 %>%
    filter(type == "vaccine_dose") %>% select( elig_study_id, calendar_date, info1, info2) %>% unique %>% 
    filter(!is.na(calendar_date))
ba2_inf_roche <- everything_times_ba2 %>%
    filter(type == "roche_result") %>% select( elig_study_id, calendar_date, info1, info2) %>% unique %>% 
    filter(!is.na(calendar_date)) %>% arrange(elig_study_id)
ba2_inf_inf <- everything_times_ba2 %>%
    filter(type == "infection") %>% select( elig_study_id, calendar_date, info1, info2) %>% unique %>%
    filter(info2 == "Omicron-BA.2")

ba2_exposed_PCR <- ba2_inf_inf %>% filter(info1 %in% c(2, 3)) %>% pull(elig_study_id) %>% unique
ba2_naive_roche_maybe <- ba2_inf_inf %>% filter(info1 == 1) %>% select(elig_study_id, ba2_date = calendar_date) %>% unique
ba2_exposed_roche <- left_join(ba2_naive_roche_maybe, ba2_inf_roche) %>% filter(info2 == "positive") %>% 
    mutate(pos_before_ba2_date = ba2_date > calendar_date) %>% filter(pos_before_ba2_date) %>% 
    pull(elig_study_id) %>% unique

ba2_exposed_id <- c(ba2_exposed_PCR, ba2_exposed_roche) %>% unique
ba2_naive_id <- setdiff(ba2_inf_id, ba2_exposed_id)


times_post_ba2_naive <- get_ba_naive(ba2_naive_id, ba2_inf_bleed, ba2_inf_vac, ba2_inf_inf, ba2_inf_roche)
times_post_ba2_exposed <- get_ba_exposed(ba2_exposed_id, ba2_inf_bleed, ba2_inf_vac, ba2_inf_inf, ba2_inf_roche)


###################################################################################
############### For BA5 infection only population
#####################################################################################

# Get all people with BA5 infection
ba5_inf_id <- everything_times_3dose %>% filter(type == "infection") %>%
    filter(info2 %in% c("Omicron-BA.4", "Omicron-BA.4/5", "Omicron-BA.5")) %>% select( elig_study_id, calendar_date, info2) %>% unique %>% pull(elig_study_id)

everything_times_ba5 <- everything_times_3dose %>% filter(elig_study_id %in% ba5_inf_id)

# ids of the vaccinated, get bleed times and vac times of relevant people
ba5_inf_bleed <- everything_times_ba5 %>%
     filter(type == "bleed_time") %>% select( elig_study_id, calendar_date, info1) %>% unique
ba5_inf_vac <- everything_times_ba5 %>%
    filter(type == "vaccine_dose") %>% select( elig_study_id, calendar_date, info1, info2) %>% unique %>% 
    filter(!is.na(calendar_date))
ba5_inf_roche <- everything_times_ba5 %>%
    filter(type == "roche_result") %>% select( elig_study_id, calendar_date, info1, info2) %>% unique %>% 
    filter(!is.na(calendar_date)) %>% arrange(elig_study_id)
ba5_inf_inf <- everything_times_ba5 %>%
    filter(type == "infection") %>% select( elig_study_id, calendar_date, info1, info2) %>% unique %>%
    filter(info2 %in% c("Omicron-BA.4", "Omicron-BA.4/5", "Omicron-BA.5"))

ba5_exposed_PCR <- ba5_inf_inf %>% filter(info1 %in% c(2, 3)) %>% pull(elig_study_id) %>% unique
ba5_naive_roche_maybe <- ba5_inf_inf %>% filter(info1 == 1) %>% select(elig_study_id, ba5_date = calendar_date) %>% unique
ba5_exposed_roche <- left_join(ba5_naive_roche_maybe, ba5_inf_roche) %>% filter(info2 == "positive") %>% 
    mutate(pos_before_ba5_date = ba5_date > calendar_date) %>% filter(pos_before_ba5_date) %>% 
    pull(elig_study_id) %>% unique

ba5_exposed_id <- c(ba5_exposed_PCR, ba5_exposed_roche) %>% unique
ba5_naive_id <- setdiff(ba5_inf_id, ba5_exposed_id)

times_post_ba5_naive <- get_ba_naive(ba5_naive_id, ba5_inf_bleed, ba5_inf_vac, ba5_inf_inf, ba5_inf_roche)
times_post_ba5_exposed <- get_ba_exposed(ba5_exposed_id, ba5_inf_bleed, ba5_inf_vac, ba5_inf_inf, ba5_inf_roche)



###################################################################################
############### Get relevant titre data
#####################################################################################


titre_vals_xtract <- everything_times %>% 
    filter(type == "bleed_time") %>% select( elig_study_id, calendar_date, info1, info2, info3) %>% 
    mutate(info2 = case_when(
        info2 == "ic50_wildtype"~"Wild type",
        info2 == "ic50_Alpha"~"Alpha",
        info2 == "ic50_Delta"~"Delta",
        info2 == "ic50_Omicron_BA1"~"Omicron BA1",
        info2 == "ic50_Omicron_BA2"~"Omicron BA2",
        info2 == "ic50_Omicron_BA5"~"Omicron BA5"
    )) %>% mutate(info2 = factor(info2, levels = variants_names))


get_titre_times <- function(times_post_XXX_first) {
    bleed_info_first_list <- list(); j <- 1
    for (i in 1:length(times_post_XXX_first$elig_study_id)) {
        id <- times_post_XXX_first$elig_study_id[i]

        bleeds <- times_post_XXX_first %>% filter(elig_study_id == id) %>% pull(bleed_no)
        bleeds_days <- times_post_XXX_first %>% filter(elig_study_id == id) %>% pull(days)

        if (length(bleeds) > 0) {
            df_temp <- data.frame(info1 = bleeds, time_until_bleed = bleeds_days)
            bleed_info_first_list[[j]] <- titre_vals_xtract %>% filter(elig_study_id == id) %>%
                filter(info1 %in% bleeds) %>% left_join(df_temp, by = "info1")
            j <- j + 1
        }
    }
    bleed_info_first_list %>% bind_rows %>% filter(time_until_bleed >= 0)
}

titre_vac3_naive <- get_titre_times(times_post_dose3_naive)
titre_ba1_naive <- get_titre_times(times_post_ba1_naive)
titre_ba2_naive <- get_titre_times(times_post_ba2_naive)
titre_ba5_naive <- get_titre_times(times_post_ba5_naive)

titre_vac3_exposed <- get_titre_times(times_post_dose3_exposed)
titre_ba1_exposed <- get_titre_times(times_post_ba1_exposed)
titre_ba2_exposed <- get_titre_times(times_post_ba2_exposed)
titre_ba5_exposed <- get_titre_times(times_post_ba5_exposed)

titre_all_naive <- bind_rows(
    titre_vac3_naive %>% mutate(type = "Vac3 naive") %>% unique,
    titre_ba1_naive %>% mutate(type = "BA1 naive") %>% unique,
    titre_ba2_naive %>% mutate(type = "BA2 naive") %>% unique,
    titre_ba5_naive  %>% mutate(type = "BA5 naive") %>% unique
) %>% filter(info2 %in% c("Wild type", "Omicron BA1", "Omicron BA2", "Omicron BA5"))

titre_all_exposed <- bind_rows(
    titre_vac3_exposed %>% mutate(type = "Vac3 exposed") %>% unique,
    titre_ba1_exposed %>% mutate(type = "BA1 exposed") %>% unique,
    titre_ba2_exposed %>% mutate(type = "BA2 exposed") %>% unique,
    titre_ba5_exposed  %>% mutate(type = "BA5 exposed") %>% unique
) %>% filter(info2 %in% c("Wild type", "Omicron BA1", "Omicron BA2", "Omicron BA5"))

save(titre_all_naive, file = here::here("data", "df", "titre_all_naive.RData"))
save(titre_all_exposed, file = here::here("data", "df", "titre_all_exposed.RData"))