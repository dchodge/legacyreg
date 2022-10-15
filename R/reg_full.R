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

# Get the infection, bleeds, and vaccine dose 
dose3_inf <- everything_times_3dose %>% filter(type == "infection") %>% 
    select(elig_study_id, calendar_date, info1, info2) %>% unique # Any infection
dose3_bleed <- everything_times_3dose %>% filter(type == "bleed_time") %>% 
    select(elig_study_id, calendar_date, info1, info2, info3) %>% unique # Any bleed_time
dose3_vac <- everything_times_3dose %>% filter(type == "vaccine_dose") %>% 
    select(elig_study_id, calendar_date, info1, info2, info3) %>% unique # Any vaccine_dose

dose3_inf %>% group_by(info2) %>% tally()

### Do this for BA1
get_reg_data_BAX <- function(inf_variant_str, ic50_variant_str) {

   # inf_variant_str <- "Delta"#"Omicron-BA.1" #"Delta"
    #ic50_variant_str <- "ic50_Delta" #"ic50_Omicron_BA1" #"ic50_Delta"
   # 
    dose3_inf_BAX <- dose3_inf %>% filter(info2 == inf_variant_str) 
    dose3_inf_BAX_ids <- dose3_inf_BAX %>% pull(elig_study_id)
    start_BAX_date <- dose3_inf_BAX %>% pull(calendar_date) %>% min # 2021-11-19
    end_BAX_date <- dose3_inf_BAX %>% pull(calendar_date) %>% max # 2022-03-21

    # Have to throw away a lot of entries as they were recruited after BA1 infection, or they have missing info for BA1
    bleed_info_BAX_inf <- dose3_bleed %>% filter(elig_study_id %in% dose3_inf_BAX_ids) %>% 
        filter(info2 == ic50_variant_str) %>% left_join(select(dose3_inf_BAX, elig_study_id, inf_date = calendar_date)) %>%
        filter(calendar_date < inf_date) %>% group_by(elig_study_id) %>% filter(info1 == max(info1)) %>% drop_na %>% 
        filter((calendar_date > (start_BAX_date + days(0))) & (calendar_date < end_BAX_date)) %>%
        mutate(infection = 1)

    dose3_inf_not_BAX_ids <- setdiff(dose3_bleed %>% pull(elig_study_id), dose3_inf_BAX_ids)
    bleed_info_no_BAX_inf <- dose3_bleed %>% filter(elig_study_id %in% dose3_inf_not_BAX_ids) %>% 
        filter((calendar_date > (start_BAX_date + days(0)))) %>%
        filter(calendar_date < end_BAX_date) %>%
        filter(info2 == ic50_variant_str) %>% 
        group_by(elig_study_id) %>% filter(info1 == min(info1)) %>% 
        drop_na %>% 
        mutate(infection = 0)

    reg_data_BAX <- bind_rows(bleed_info_BAX_inf, bleed_info_no_BAX_inf)
    reg_data_BAX
}

reg_data_Delta <- get_reg_data_BAX("Delta", "ic50_Delta")
reg_data_BA1 <- get_reg_data_BAX("Omicron-BA.1", "ic50_Omicron_BA1")
reg_data_BA2 <- get_reg_data_BAX("Omicron-BA.2", "ic50_Omicron_BA2")

# Get info on vaccine dose timiings


plt_schematics <- function(reg_data, variant_string, title_string){
    labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")

    reg_data %>% 
        ggplot() +
            geom_count(aes(x = calendar_date, y = 5 * 2^info3, color = as.logical(infection)), 
                alpha = 0.7) + 
            scale_color_manual(values = c("gray", "red")) +
            scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y) + 
            guides(size = "none") + labs(x = "Calendar date", y = "Titre value",  color = "Becomes infected?",
            title = title_string) +
            theme_bw()

    ggsave(here::here("outputs", "figs", "reg_titre", paste0(variant_string, "_schematic.png")), width = 10, height = 6)
}

plt_schematics(reg_data_Delta, "Delta", "Infections during the Delta wave (2021-2022)")
plt_schematics(reg_data_BA1, "BA1", "Infections during the BA1 wave (2021-2022)")
plt_schematics(reg_data_BA2, "BA2", "Infections during the BA2 wave (2022)")

reg_data_D_meta <- reg_data_Delta %>% left_join(meta_data) %>% select(!inf_date) %>% drop_na %>% 
    mutate(centre = as.character(centre)) %>% filter(centre != "ealingnwp")
reg_data_BA1_meta <- reg_data_BA1 %>% left_join(meta_data) %>% select(!inf_date) %>% drop_na %>% 
    mutate(centre = as.character(centre)) %>% filter(centre != "ealingnwp")
reg_data_BA2_meta <- reg_data_BA2 %>% left_join(meta_data) %>% select(!inf_date) %>% drop_na %>% 
    mutate(centre = as.character(centre)) 

library(brms)

fit_D <- brm(infection ~ age + sex + centre + gp(info3), family = bernoulli("logit"), data = reg_data_D_meta, cores = 4)
fit_BA1 <- brm(infection ~ age + sex + centre + gp(info3), family = bernoulli("logit"), data = reg_data_BA1_meta, cores = 4)
fit_BA2 <- brm(infection ~ age + sex + centre + gp(info3), family = bernoulli("logit"), data = reg_data_BA2_meta, cores = 4)

require(posterior); require(tidybayes); require(modelr)
posterior_values_D <- reg_data_D_meta %>% add_epred_draws(fit_D)
e_titre40_D <- posterior_values_D %>% filter(info3 == 1) %>% pull(.epred) %>% mean
posterior_values_D <- posterior_values_D %>% mutate(.eprd_rel = .epred / e_titre40_D)

posterior_values_BA1 <- reg_data_BA1_meta %>% add_epred_draws(fit_BA1)
e_titre40_BA1 <- posterior_values_BA1 %>% filter(info3 == 1) %>% pull(.epred) %>% mean
posterior_values_BA1 <- posterior_values_BA1 %>% mutate(.eprd_rel = .epred / e_titre40_BA1)

posterior_values_BA2 <- reg_data_BA2_meta %>% add_epred_draws(fit_BA2)
e_titre40_BA2 <- posterior_values_BA2 %>% filter(info3 == 1) %>% pull(.epred) %>% mean
posterior_values_BA2 <- posterior_values_BA2 %>% mutate(.eprd_rel = .epred / e_titre40_BA2)

main_plot <- function(reg_data, posterior_values, variant_string) { 

    require(ggdist)
    labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")
    p1 <- posterior_values %>%
        ggplot() + 
          #  stat_summary(data = reg_data, aes(2 ^ info3 * 5, infection), shape = 3, size = 2, fun = "mean", geom = "point") + 
            stat_lineribbon(aes(2 ^ info3 * 5, .eprd_rel), .width = .95, fill = "red", alpha = 0.5, 
                point_interval = "mean_qi") + theme_bw() + 
            scale_x_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y) + 
            labs(x = "Titre value before infection",
                y = paste0("Proportion of population infected relative to those with titre \u2264 40"))


    p2 <- posterior_values %>% 
        ggplot() + 
         #   stat_summary(data = reg_data, aes(centre, infection), shape = 3, size = 2, fun = "mean", geom = "point") + 
            stat_pointinterval(aes(centre, .eprd_rel), .width = .95, fill = "red", alpha = 0.5, 
                point_interval = "mean_qi") + theme_bw() + 
            labs(x = "Center", 
                y = paste0("Proportion of population infected \nrelative to those with titre \u2264 40"))
    p3 <- posterior_values %>% 
        ggplot() + 
           # stat_summary(data = reg_data, aes(sex, infection), shape = 3, size = 2, fun = "mean", geom = "point") + 
            stat_pointinterval(aes(sex, .eprd_rel), .width = .95, fill = "red", alpha = 0.5, 
                point_interval = "mean_qi") + theme_bw() + 
            labs(x = "Gender",
                y = paste0("Proportion of population infected \nrelative to those with titre \u2264 40"))

    require(patchwork)
    p1 + (p2 / p3) + plot_annotation(title = paste0(variant_string , " infections"),
        subtitle = "Posterior predictive plots on y ~ gender + center + gp(preinf_titre)", tag_levels = "A")
    ggsave(here::here("outputs", "figs", "reg_titre", paste0(variant_string, ".png")), width = 10, height = 6)
}   

main_plot(reg_data_D_meta, posterior_values_D, "Delta")
main_plot(reg_data_BA1_meta, posterior_values_BA1, "BA1")
main_plot(reg_data_BA2_meta, posterior_values_BA2, "BA2")



require(brms)
reg_data_BA1_vac <- reg_data_BA1 %>% left_join(filter(dose3_vac, info1 == 3) %>% 
    select(elig_study_id, dose3_date = calendar_date)) %>% 
    mutate(days_dose3_2_bleed = as.numeric(calendar_date - dose3_date )) %>%
    mutate(days_bleed_2_inf = as.numeric(inf_date - calendar_date ))

reg_data_BA1_vac_meta <- reg_data_BA1_vac %>% left_join(meta_data) %>% select(!inf_date)  %>% 
    mutate(centre = as.character(centre)) %>% filter(centre != "ealingnwp") %>%
    filter(days_dose3_2_bleed > 0, (days_bleed_2_inf > 0 | is.na(days_bleed_2_inf))) %>% 
    mutate(days_bleed_2_inf = replace_na(days_bleed_2_inf, 0)) %>% 
    mutate(dose_3_until_titre_rel = days_dose3_2_bleed + days_bleed_2_inf)

reg_data_BA1_vac_meta %>% as.data.frame
labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")

reg_data_BA1_vac_meta %>% 
        ggplot() +
            geom_count(aes(x = calendar_date, y = 5 * 2^info3, color = as.logical(infection)), 
                alpha = 0.7) + 
            scale_color_manual(values = c("gray", "red")) +
            scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y) + 
            guides(size = "none") + labs(x = "Calendar date", y = "Titre value",  color = "Becomes infected?") + 
            theme_bw()



posterior_values_BA1 <- reg_data_BA1_vac_meta %>% add_epred_draws(fit_BA1_vac)
e_titre40_BA1 <- posterior_values_BA1 %>% filter(info3 == 1) %>% pull(.epred) %>% mean
posterior_values_BA1 <- posterior_values_BA1 %>% mutate(.eprd_rel = .epred / e_titre40_BA1)


main_plot_alt <- function(reg_data, posterior_values, variant_string) { 
    reg_data <- reg_data_BA1_vac_meta
    posterior_values <- posterior_values_BA1
    require(ggdist)
    labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")
    p1 <- posterior_values %>%
        ggplot() + 
          #  stat_summary(data = reg_data, aes(2 ^ info3 * 5, infection), shape = 3, size = 2, fun = "mean", geom = "point") + 
            stat_lineribbon(aes(2 ^ estimated_wane_titre_gp * 5, .eprd_rel), .width = .95, fill = "red", alpha = 0.5, 
                point_interval = "mean_qi") + theme_bw() + 
            scale_x_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y) + 
            labs(x = "Titre value before infection",
                y = paste0("Proportion of population infected relative to those with titre \u2264 40"))


    p2 <- posterior_values %>% 
        ggplot() + 
         #   stat_summary(data = reg_data, aes(centre, infection), shape = 3, size = 2, fun = "mean", geom = "point") + 
            stat_pointinterval(aes(centre, .eprd_rel), .width = .95, fill = "red", alpha = 0.5, 
                point_interval = "mean_qi") + theme_bw() + 
            labs(x = "Center", 
                y = paste0("Proportion of population infected \nrelative to those with titre \u2264 40"))
    p3 <- posterior_values %>% 
        ggplot() + 
           # stat_summary(data = reg_data, aes(sex, infection), shape = 3, size = 2, fun = "mean", geom = "point") + 
            stat_pointinterval(aes(sex, .eprd_rel), .width = .95, fill = "red", alpha = 0.5, 
                point_interval = "mean_qi") + theme_bw() + 
            labs(x = "Gender",
                y = paste0("Proportion of population infected \nrelative to those with titre \u2264 40"))
                
    p4 <- posterior_values %>% 
        ggplot() + 
         #   stat_summary(data = reg_data, aes(centre, infection), shape = 3, size = 2, fun = "mean", geom = "point") + 
            stat_lineribbon(aes(age, .eprd_rel), .width = .95, fill = "red", alpha = 0.5, 
                point_interval = "mean_qi") + theme_bw() + 
            labs(x = "Age (years)", 
                y = paste0("Proportion of population infected \nrelative to those with titre \u2264 40"))
    p5 <- posterior_values %>% 
        ggplot() + 
           # stat_summary(data = reg_data, aes(sex, infection), shape = 3, size = 2, fun = "mean", geom = "point") + 
            stat_lineribbon(aes(dose_3_until_titre_rel, .eprd_rel), .width = .95, fill = "red", alpha = 0.5, 
                point_interval = "mean_qi") + theme_bw() + 
            labs(x = "Days after 3rd dose",
                y = paste0("Proportion of population infected \nrelative to those with titre \u2264 40"))
    require(patchwork)
    p1 / (p2 + p3) / (p4 + p5) + plot_annotation(title = paste0(variant_string , " infections"),
        subtitle = "Posterior predictive plots on y ~ gender + center + gp(preinf_titre)", tag_levels = "A")
    ggsave(here::here("outputs", "figs", "reg_titre", paste0(variant_string, ".png")), width = 10, height = 15)
}   


reg_data_BA1_vac_meta <- reg_data_BA1_vac_meta %>% mutate(estimated_wane_titre = info3 * (1 - days_bleed_2_inf * 0.003))

reg_data_BA1_vac_meta %>% 
    ggplot() + 
        geom_count(aes(dose_3_until_titre_rel, estimated_wane_titre, color = as.character(infection)), alpha = 0.7) + 
                    scale_color_manual(values = c("gray", "red")) 


reg_data_BA1_vac_meta %>% 
    ggplot() + 
        geom_segment(aes(x = days_dose3_2_bleed, xend = dose_3_until_titre_rel, y = info3, yend = estimated_wane_titre, group = elig_study_id), alpha = 0.7, 
            position = position_dodge(0.5)) 

reg_data_BA1_vac_meta <- reg_data_BA1_vac_meta %>% mutate(estimated_wane_titre_gp = round(estimated_wane_titre, 0))
fit_BA1_vac <- brm(infection ~ dose_3_until_titre_rel + age + sex + centre + gp(estimated_wane_titre_gp), family = bernoulli("logit"), data = reg_data_BA1_vac_meta, cores = 4)


require(modelr)
posterior_values_BA1 <- reg_data_BA1_vac_meta %>% 
    group_by(centre, sex, infection) %>% 
    modelr::data_grid(age = seq(20, 70, 5), 
        estimated_wane_titre_gp = c(1:7), 
        dose_3_until_titre_rel =  seq(1, 150, 5)) %>%
    add_epred_draws(fit_BA1_vac, ndraws = 100)
e_titre40_BA1 <- posterior_values_BA1 %>% filter(estimated_wane_titre_gp == 1) %>% pull(.epred) %>% mean
posterior_values_BA1 <- posterior_values_BA1 %>% mutate(.eprd_rel = .epred )

main_plot_alt(reg_data_BA1_vac_meta, posterior_values_BA1, "BA1_alt")

library(scales)
logit_alt <- trans_new("logit perc",
    transform = function(x) exp(x) / (1 + exp(x)),
    inverse = function(x) log(x / (1 - x))
)


fit_BA1_vac_df <- fit_BA1_vac %>% as_draws_df
i_age_vec <- fit_BA1_vac_df$b_Intercept
s_age_vec <- fit_BA1_vac_df$b_age
s_vac3_vec <- fit_BA1_vac_df$b_dose_3_until_titre_rel
s_titre_vec <- fit_BA1_vac_df$b_estimated_wane_titre

logit_alt <- function(x, i, s) exp(x * s + i) / (1 + exp(x * s + i))

ggplot() + 
    xlim(1, 7) + ylim(0, 1) +
    lapply(1:500,
        function(x)
            geom_function(fun = logit_alt, args = list(i = i_age_vec[x], s = s_titre_vec[x]), alpha = 0.5, size = 0.5 ) 
    )