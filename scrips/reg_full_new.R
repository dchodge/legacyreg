require(tidyverse)
require(posterior)
load(file = here::here("data", "df", "titre_all_naive.RData")) # titre_all_naive
load(file = here::here("data", "df", "titre_all_exposed.RData")) # titre_all_exposed

titre_boost_wane <- function(posterior, t_vec) {

    boost_i <- posterior[[1]]
    boost_s <- posterior[[2]]
    wane_s <- posterior[[3]]
    t_p <- posterior[[4]]

    mu <- rep(0, length(t_vec))
    i <- 1
    for (t in t_vec) {
        if (t < t_p) { 
            mu[i] <- (boost_i) + (boost_s) * t; 
        } else {
            mu[i] <- (wane_s) * (t - t_p) + (boost_i) + (boost_s) * t_p; 
        }
        mu[i] <- max(mu[i], 0);
        i <- i + 1;
    }
    return(mu);
} 

get_pars_ind <- function(titre_data, infec_strain, infec_strains_short, exposure_history) {
    titre_all_trim <- titre_data %>% filter(info2 == infec_strain, type == paste0("Vac3 ", exposure_history)) %>% filter(!is.na(info3))
    titre_all_trim_ids <- titre_all_trim$elig_study_id %>% unique
    titre_all_trim <- titre_all_trim %>% mutate(elig_study_id = factor(elig_study_id, levels = titre_all_trim_ids)) %>%
        mutate(elig_study_id = as.numeric(elig_study_id))
    N_id <- length(titre_all_trim_ids)
    temp_stanfit <- readRDS(here::here("outputs", "stanfits", paste0("fit_", infec_strains_short, "_", "vac3", "_", exposure_history, ".Rdata")))

    boost_i_a <- temp_stanfit %>% as_draws(variables = c("boost_i")) %>% mean
    boost_s_a <- temp_stanfit %>% as_draws(variables = c("boost_s")) %>% mean
    wane_s_a <- temp_stanfit %>% as_draws(variables = c("wane_s")) %>% mean
    t_p_a <- temp_stanfit %>% as_draws(variables = c("t_p")) %>% mean

    df_naive_pars <- data.frame(
        ids = titre_all_trim_ids,    
        i = boost_i_a + (temp_stanfit %>% as_draws(variables = c("boost_i_ind")) %>% as_draws_df %>% apply(2, mean) %>% .[1:N_id] %>% as.numeric),
        boost_s = boost_s_a + (temp_stanfit %>% as_draws(variables = c("boost_s_ind")) %>% as_draws_df %>% apply(2, mean) %>% .[1:N_id] %>% as.numeric),
        wane = wane_s_a + (temp_stanfit %>% as_draws(variables = c("wane_s_ind")) %>% as_draws_df %>% apply(2, mean) %>% .[1:N_id] %>% as.numeric),
        t_p = t_p_a + (temp_stanfit %>% as_draws(variables = c("t_p_ind")) %>% as_draws_df %>% apply(2, mean) %>% .[1:N_id] %>% as.numeric)
    ) %>% mutate(exposure_hist = exposure_history)
    df_naive_pars
}

#################################################
####### GET POSTERIOR DISTRIBUTIONS  #######
#################################################
df_post_BA1_inf <- bind_rows(
    get_pars_ind(titre_all_naive, "Omicron BA1", "ba1", "naive"),
    get_pars_ind(titre_all_exposed, "Omicron BA1", "ba1", "exposed")
)

df_post_BA2_inf <- bind_rows(
    get_pars_ind(titre_all_naive, "Omicron BA2", "ba2", "naive"),
    get_pars_ind(titre_all_exposed, "Omicron BA2", "ba2", "exposed")
)

df_post_BA5_inf <- bind_rows(
    get_pars_ind(titre_all_naive, "Omicron BA5", "ba5", "naive"),
    get_pars_ind(titre_all_exposed, "Omicron BA5", "ba5", "exposed")
)


#################################################
####### GET DATA  #######
#################################################

get_df_legacy_raw_annot <- load(here::here("data", "Legacy_DataAnnotatedDateSeries_2022-10-14.RData"))
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
dose3_roche <- everything_times_3dose %>% filter(type == "roche_result") %>% 
    select( elig_study_id, calendar_date, info1, info2) %>% unique %>% 
    filter(!is.na(calendar_date)) %>% arrange(elig_study_id)
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

df_dose_hist <- bind_rows(
    data.frame(elig_study_id = dose3_exposed_ids, infection_history = "exposed"),
    data.frame(elig_study_id = dose3_naive_ids, infection_history = "naive")
)


#################################################
####### GET TRAJECTORIES OF INFECTED PEOPLE  #######
#################################################


get_trajectories_infected <- function(df_post_BAX_inf, dose3_inf, dose3_vac, inf_variant_str, ic50_variant_str) {
    dose3_inf_BAX <- dose3_inf %>% filter(info2 == inf_variant_str) 
    dose3_inf_BAX_ids <- dose3_inf_BAX %>% pull(elig_study_id)

    # Have to throw away a lot of entries as they were recruited after BA1 infection, or they have missing info for BA1
    BAX_inf_ppl <- dose3_inf %>% filter(info2 == inf_variant_str) %>% rename(inf_date = calendar_date) %>%
        left_join(select(dose3_vac, elig_study_id, info1, vac_date = calendar_date) %>% filter(info1 == 3) %>% select(elig_study_id, vac_date)) %>%
        select(elig_study_id, inf_date, vac_date) %>% unique %>%
        mutate(​dose_3_until_inf = as.numeric(inf_date - vac_date)) %>%
        filter(vac_date < inf_date)

    ids_BAX <- BAX_inf_ppl %>% pull(elig_study_id)

    N <- length(ids_BAX)
    traj_fit_list <- list()
    for (j in 1:N) {
        T <- BAX_inf_ppl %>% filter(elig_study_id == ids_BAX[j]) %>% pull(​dose_3_until_inf)
        inf_date <- BAX_inf_ppl %>% filter(elig_study_id == ids_BAX[j]) %>% pull(inf_date)

        post_pars <- df_post_BAX_inf %>% filter(ids == ids_BAX[j])    
        exposure_hist <- df_post_BAX_inf %>% filter(ids == ids_BAX[j]) %>% pull(exposure_hist)

        if ((nrow(post_pars)) > 0) {
            traj_fit_list[[j]] <- data.frame(
                id = ids_BAX[j],
                inf_date = inf_date[1],
                exposure_hist = exposure_hist,
                t = 1:T[1],
                traj_i = titre_boost_wane(post_pars[2:5], 1:T[1])
            )
        }
    }

    traj_fit_df <- traj_fit_list %>% bind_rows
    traj_fit_df
}

traj_fit_df_BA1 <- get_trajectories_infected(df_post_BA1_inf, dose3_inf, dose3_vac, "Omicron-BA.1", "ic50_Omicron_BA1")
traj_fit_df_BA2 <- get_trajectories_infected(df_post_BA2_inf, dose3_inf, dose3_vac, "Omicron-BA.2", "ic50_Omicron_BA2")
traj_fit_df_BA5 <- get_trajectories_infected(df_post_BA5_inf, dose3_inf, dose3_vac, "Omicron-BA.5", "ic50_Omicron_BA5")

df_points_BA1 <- traj_fit_df_BA1 %>% group_by(id) %>% filter(t == max(t)) 
traj_fit_df_BA1 %>%
    ggplot() + 
        geom_line(aes(x = t, y = traj_i, group = id), size = 0.1) + 
        geom_point(data = df_points_BA1, aes(x = t, y = traj_i, fill = exposure_hist), size = 2, shape = 21) + 
        theme_bw()


df_points_BA2 <- traj_fit_df_BA2 %>% group_by(id) %>% filter(t == max(t)) 
traj_fit_df_BA2 %>%
    ggplot() + 
        geom_line(aes(x = t, y = traj_i, group = id), size = 0.1) + 
        geom_point(data = df_points_BA2, aes(x = t, y = traj_i, fill = exposure_hist), size = 2, shape = 21) + 
        theme_bw()

df_points_BA5 <- traj_fit_df_BA5 %>% group_by(id) %>% filter(t == max(t)) 
traj_fit_df_BA5 %>%
    ggplot() + 
        geom_line(aes(x = t, y = traj_i, group = id), size = 0.1) + 
        geom_point(data = df_points_BA5, aes(x = t, y = traj_i, fill = exposure_hist), size = 2, shape = 21) + 
        theme_bw()


get_full_data_compare <- function(dose3_inf, dose3_bleed, inf_variant_str, df_post_BAX_inf, df_points_BAX) {
# None infected people
    BAX_dates <- df_points_BAX %>% pull(inf_date) %>% unique %>% sort # "2021-12-02" -> "2022-03-21"
    start_BAX_date <- min(BAX_dates)
    end_BAX_date <- max(BAX_dates)

    dose3_inf_BAX <- dose3_inf %>% filter(info2 == inf_variant_str) 
    dose3_inf_BAX_ids <- dose3_inf_BAX %>% pull(elig_study_id)

    dose3_inf_not_BAX_ids <- setdiff(dose3_bleed %>% pull(elig_study_id), dose3_inf_BAX_ids)

    ids_alt <- intersect(df_post_BAX_inf$ids, dose3_inf_not_BAX_ids)

    bleed_info_no_BAX_inf <- dose3_bleed %>% filter(elig_study_id %in% dose3_inf_not_BAX_ids) %>% 
        left_join(select(dose3_vac, elig_study_id, info1, vac_date = calendar_date) %>%
        filter(info1 == 3) %>% select(elig_study_id, vac_date)) %>%
        filter((calendar_date > (start_BAX_date - days(10)))) %>%
        filter(calendar_date < end_BAX_date + days(10)) %>%
        filter(calendar_date > vac_date) %>%
        filter(info2 == ic50_variant_str) %>% 
        group_by(elig_study_id) %>% 
        drop_na %>% 
        mutate(infection = 0) %>%
        left_join(df_dose_hist)

    plot_vals <- bind_rows(
        df_points_BAX %>% select(id, date = inf_date, titre = traj_i, infection_history = exposure_hist) %>% mutate(infection = 1),
        bleed_info_no_BAX_inf %>% select(id = elig_study_id, date = calendar_date, titre = info3, infection, infection_history)
    )

    plot_vals_meta <- plot_vals %>% mutate(elig_study_id = id) %>% left_join(meta_data)
    dose3_inf_never_inf <- setdiff(dose3_bleed %>% pull(elig_study_id), dose3_inf %>% pull(elig_study_id))
    df_never_infected <- data.frame(
        ids = dose3_inf_never_inf,
        info = "Never infected"
    )

    plot_vals_meta <- rename(plot_vals_meta, ids = id) %>% left_join(df_never_infected) %>% 
        mutate(info = if_else(is.na(info), "Gets infected", "Never infected"))
    
    recode_infhist <- c("naive" = "Never infected", "exposed" = "Previously infected")

    plot_vals_meta <- plot_vals_meta %>% mutate(infection_history = recode(infection_history, !!!recode_infhist)) %>%
        mutate(infection = if_else(infection == 1, "Titre at infection", "Titre at bleed"))
    plot_vals_meta
}

plot_vals_meta_BA1 <- get_full_data_compare(dose3_inf, dose3_bleed, "Omicron-BA.1", df_post_BA1_inf, df_points_BA1)
plot_vals_meta_BA2 <- get_full_data_compare(dose3_inf, dose3_bleed, "Omicron-BA.2", df_post_BA2_inf, df_points_BA2)
plot_vals_meta_BA5 <- get_full_data_compare(dose3_inf, dose3_bleed, "Omicron-BA.5", df_post_BA5_inf, df_points_BA5)

plot_descriptive_BAX <- function(plot_vals_meta, titre_string) {
    labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")

    p1 <- plot_vals_meta %>% 
        ggplot(aes(x = date, y = titre, color = infection)) + 
            geom_point(shape = 21, alpha =1) + scale_color_manual(values = c("gray", "red")) + theme_bw() +
            scale_y_continuous(breaks = (1:7), labels = labs_plot_y) + 
            labs(x = "Date", y = "Titre value", color = "Titre measure")

    p2 <- plot_vals_meta %>% 
        ggplot(aes(x = age, y = titre, color = infection)) + 
            geom_point(shape = 21, alpha = 1) + scale_color_manual(values = c("gray", "red")) + theme_bw() +
            geom_smooth(method = "lm") +
            scale_y_continuous(breaks = (1:7), labels = labs_plot_y)  + 
            labs(x = "Age (years)", y = "Titre value", color = "Titre measure")

    p3 <- plot_vals_meta %>% 
        ggplot(aes(x = centre, y = titre, color = infection)) + 
            geom_point( aes(color = infection), shape = 21, 
                position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.2 ), alpha = 1) + 
            geom_boxplot(aes(color = infection), alpha = 0.8, width = 0.5) + theme_bw() +
            scale_color_manual(values = c("gray", "red")) + scale_fill_manual(values = c("gray", "red")) + theme_bw() +
            geom_smooth(method = "lm") +
            scale_y_continuous(breaks = (1:7), labels = labs_plot_y) +
            labs(x = "Centre", y = "Titre value", color = "Titre measure")


    p4 <- plot_vals_meta %>% 
        ggplot(aes(x = infection_history, y = titre)) +  
            geom_point( aes(color = infection), shape = 21,
                position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.2 ), alpha = 1) +    
            geom_boxplot(aes(color = infection), width = 0.5, alpha = 0.8) + theme_bw() +
            scale_color_manual(values = c("gray", "red")) + scale_fill_manual(values = c("gray", "red")) + 
            scale_y_continuous(breaks = (1:7), labels = labs_plot_y) + 
            labs(x = "Infection history", y = "Titre value", color = "Titre measure")
    (p1 + p2) / (p3 + p4) + plot_layout(guides = "collect") + plot_annotation(title = titre_string)
    ggsave(here::here("outputs", "figs", "cop", paste0("descriptive_",  titre_string, ".pdf")))
}

plot_descriptive_BAX(plot_vals_meta_BA1, "BA1_infection")
plot_descriptive_BAX(plot_vals_meta_BA2, "BA2_infection")
plot_descriptive_BAX(plot_vals_meta_BA5, "BA5_infection")




require(brms)
fit_BA1_vac <- brm(infection ~ titre + age + infection_history, family = bernoulli("logit"), 
    data = plot_vals_meta, cores = 4)

require(tidybayes)
posterior_values_BA1 <- plot_vals_meta %>% 
    group_by(infection_history) %>% 
    modelr::data_grid(age = seq(20, 70, 5), 
        titre = c(1:7)) %>%
    add_epred_draws(fit_BA1_vac, ndraws = 100)


##### IDEAS! #####
# NEED TIM's NEW PRIORS
# REMOVE THOSE WITH PREVIOUS INFECTIONS
# COPARE INFERRED TIRE VALUES TO RECENT BLEED VALUES
# NEED MEASURE OF EXPOSURE (titre vs. number of infections?)