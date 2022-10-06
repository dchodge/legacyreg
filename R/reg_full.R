library(tidyverse)
library(here)
library(lubridate)


get_df_legacy_raw_annot <- load(here::here("data", "Legacy_DataAnnotatedDateSeries_2022-09-09.RData"))
raw_legacy_full <- chrono.df
meta_data <- raw_legacy_full %>% select(elig_study_id, age, sex, centre) %>% unique


load(file = here::here("data", "df", "everything_long.RData")) # everything_times
## Get ids 
dose3_ids <- everything_times %>% 
    filter(type == "vaccine_dose") %>% filter(!is.na(calendar_date)) %>% 
    filter(info1 == 3) %>% pull(elig_study_id)
everything_times_3dose <- everything_times %>% filter(elig_study_id %in% dose3_ids)

dose3_inf <- everything_times_3dose %>% filter(type == "infection") %>% 
    select(elig_study_id, calendar_date, info1, info2) %>% unique # Any infection
dose3_bleed <- everything_times_3dose %>% filter(type == "bleed_time") %>% 
    select(elig_study_id, calendar_date, info1, info2, info3) %>% unique # Any infection
dose3_vac <- everything_times_3dose %>% filter(type == "vaccine_dose") %>% 
    select(elig_study_id, calendar_date, info1, info2, info3) %>% unique # Any infection

dose3_inf_BA1 <- dose3_inf %>% filter(info2 == "Omicron-BA.1") 
dose3_inf_BA1_ids <- dose3_inf_BA1 %>% pull(elig_study_id)

dose3_inf_BA2 <- dose3_inf %>% filter(info2 == "Omicron-BA.2") 
dose3_inf_BA2_ids <- dose3_inf_BA1 %>% pull(elig_study_id)

start_ba1_date <- dose3_inf_BA1 %>% pull(calendar_date) %>% min # 2021-11-19
end_ba1_date <- dose3_inf_BA1 %>% pull(calendar_date) %>% max # 2022-03-21

#### Need to throw out those with vaccination date or previous infection
dose3_bleed_dates <- dose3_bleed %>% filter((calendar_date < end_ba1_date))

# Remove previous vaccinations
dose3_vac_trim <- dose3_vac %>% select(!info3) %>% drop_na %>% group_by(elig_study_id) %>% filter(info1 == max(info1)) %>% 
    rename(lastvacdate = calendar_date) %>% select(elig_study_id, lastvacdate )
dose3_bleed_dates_A <- dose3_bleed_dates %>% left_join(dose3_vac_trim) %>% mutate(bleed_after_vac = calendar_date > lastvacdate) %>% 
    filter(bleed_after_vac) %>% select(!c(lastvacdate, bleed_after_vac))

# Remove previous infections
dose3_inf_trim <- dose3_inf %>% drop_na %>% group_by(elig_study_id) %>% filter(info1 == min(info1)) %>% 
    filter(info2 %in% c("Delta", "D614G", "Alpha")) %>%
    rename(inf_date = calendar_date) %>% select(elig_study_id, inf_date )
dose3_bleed_dates_B <- dose3_bleed_dates_A %>% left_join(dose3_inf_trim) %>% mutate(bleed_after_inf = calendar_date > inf_date) %>% 
    filter(bleed_after_inf | is.na(bleed_after_inf)) %>% select(!c(inf_date, bleed_after_inf))


bleed_info_BA1_inf <- dose3_bleed_dates %>% filter(elig_study_id %in% dose3_inf_BA1_ids) %>% 
    filter(info2 == "ic50_Omicron_BA1") %>% left_join(select(dose3_inf_BA1, elig_study_id, inf_date = calendar_date)) %>%
    filter(calendar_date < inf_date) %>% group_by(elig_study_id) %>% filter(info1 == max(info1)) %>% drop_na %>% 
    filter(calendar_date > as.Date("2021-10-01")) %>%
    mutate(BA1_infection = 1)

dose3_inf_not_BA1_ids <- setdiff(dose3_bleed %>% pull(elig_study_id), dose3_inf_BA1_ids)
bleed_info_no_BA1_inf <- dose3_bleed_dates %>% filter(elig_study_id %in% dose3_inf_not_BA1_ids) %>% 
    filter(info2 == "ic50_Omicron_BA1") %>% 
    #left_join(select(dose3_inf_BA2, elig_study_id, inf_date = calendar_date)) %>%
  #  filter(calendar_date < inf_date | is.na(inf_date)) %>% 
    group_by(elig_study_id) %>% filter(info1 == max(info1)) %>% 
  #  select(!inf_date) %>%
    drop_na %>% 
    mutate(BA1_infection = 0)

reg_data_BA1 <- bind_rows(bleed_info_BA1_inf, bleed_info_no_BA1_inf)
reg_data_BA1 <- reg_data_BA1 %>% mutate(info3 = round(info3, 0))

library(brms)
reg_data_BA1 <- reg_data_BA1 %>% left_join(meta_data) %>% select(!inf_date) %>% drop_na %>% 
    mutate(centre = as.character(centre)) %>% filter(centre != "ealingnwp")

fitA <- brm(BA1_infection ~ sex + centre + gp(info3), family = bernoulli("logit"), data = reg_data_BA1, cores = 4)

require(posterior); require(tidybayes); require(modelr)

fitA %>% as_draws_df %>% select()

posterior_values <- reg_data_BA1 %>% add_epred_draws(fitA) group_by(sex, centre) %>% 
    

posterior <- posterior_epred(fitA, ndarws = 500)
str(posterior)

require(ggdist)
labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")

p1 <- posterior_values %>%
    ggplot() + 
        #geom_dots(data = reg_data_BA1, aes(2 ^ info3 * 5, BA1_infection, 
        #    side = ifelse(BA1_infection, "bottom", "top")) , pch = 19, alpha = 1, scale = 0.5, color = "grey20") + 
        stat_lineribbon(aes(2 ^ info3 * 5, .epred), .width = .95, fill = "red", alpha = 0.5) + theme_bw() + 
        scale_x_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y) + 
        labs(x = "Titre value before infection", y = "Proportion of population infected during BA1 wave") 


p2 <- posterior_values %>% 
    ggplot() + 
       # geom_count(aes(info3, BA1_infection)) + 
        stat_pointinterval(aes(centre, .epred), .width = .95, fill = "red", alpha = 0.5) + theme_bw() + 
        labs(x = "Center", y = "Proportion of population \ninfected during BA1 wave")

p3 <- posterior_values %>% 
    ggplot() + 
       # geom_count(aes(info3, BA1_infection)) + 
        stat_pointinterval(aes(sex, .epred), .width = .95, fill = "red", alpha = 0.5) + theme_bw() + 
        labs(x = "Gender", y = "Proportion of population \ninfected during BA1 wave")

require(patchwork)
p1 + (p2 / p3)

