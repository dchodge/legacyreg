## Load model data

#load(file = here::here("outputs", "df", "fitted_lines_figs.RData")) # post_pred_fitted

## ## ## ## ## ## ## ## ## ## ## ## ## 
## Plot the full titres and fits ##
## ## ## ## ## ## ## ## ## ## ## ## ## 

plot_model_data <- function(titre_all_naive, titre_all_exposed, type_str) {
    type_str_fil_naive <- paste0(type_str, " naive")
    type_str_fil_exposed <- paste0(type_str, " exposed")

    titre_all_naive_dose3 <- titre_all_naive %>% filter(type == !!type_str_fil_naive)
    titre_all_exposed_dose3 <- titre_all_exposed %>% filter(type == !!type_str_fil_exposed)

    labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")

    p1 <- titre_all_naive_dose3 %>% 
        ggplot() + 
        geom_line(aes(x = time_until_bleed, y = 2 ^ info3 * 5, color = info2, group = paste(elig_study_id, info2)),
            size = 0.3) + 
        geom_point(aes(x = time_until_bleed, y = 2 ^ (info3 ) * 5, fill = info2), shape = 21, alpha = 0.7) + 
        labs(x = "Days between vac dose and bleed", y = "Titre value", title = "Third vaccine dose without previous infection", color = "Variant") +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y ) + theme_bw() + 
        scale_x_continuous(limits = c(0, NA)) +
        guides(color = "none", fill = "none") +
        facet_wrap(vars(info2))
    p2 <- titre_all_exposed_dose3 %>% #filter(!elig_study_id %in% ids_detect) %>% 
        ggplot() + 
        geom_line(aes(x = time_until_bleed, y = 2 ^ info3 * 5, color = info2, group = paste(elig_study_id, info2)),
            size = 0.3) + 
        geom_point(aes(x = time_until_bleed, y = 2 ^ info3 * 5, fill = info2), shape = 21, alpha = 0.7) + 

        labs(x = "Days between vac dose and bleed", y = "Titre value", title = "Third vaccine dose with previous infection", color = "Variant") +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y ) + theme_bw() + 
        guides(color = "none", fill = "none") +
        facet_wrap(vars(info2))
    require(patchwork)
    p1 + p2 + plot_layout(guides = "collect")
    ggsave(here::here("outputs", "figs", paste0("model_data_", type_str, ".pdf")))
}

plot_model_data_fitted <- function(titre_all_naive, titre_all_exposed, fitted_naive, fitted_exposed, type_str) {
    type_str_fil_naive <- paste0(type_str, " naive")
    type_str_fil_exposed <- paste0(type_str, " exposed")

    titre_all_naive_dose3 <- titre_all_naive %>% filter(type == !!type_str_fil_naive)
    titre_all_exposed_dose3 <- titre_all_exposed %>% filter(type == !!type_str_fil_exposed)

    fitted_naive_type  <- fitted_naive %>% filter(type == !!type_str)
    fitted_exposed_type <- fitted_exposed %>% filter(type == !!type_str)

    labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")

    p1 <- titre_all_naive_dose3 %>% 
        ggplot() + 
        geom_line(aes(x = time_until_bleed, y = 2 ^ info3 * 5, color = info2, group = paste(elig_study_id, info2)),
            size = 0.3) + 
        geom_point(aes(x = time_until_bleed, y = 2 ^ (info3 ) * 5, fill = info2), shape = 21, alpha = 0.7) + 
        geom_line(data = fitted_naive_type, aes(x = t, y = 2 ^ (fitted_val) * 5), color = "black", size = 1.5, alpha = 0.7) + 
        labs(x = "Days between vac dose and bleed", y = "Titre value", title = "Third vaccine dose without previous infection", color = "Variant") +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y ) + theme_bw() + 
        scale_x_continuous(limits = c(0, NA)) +
        guides(color = "none", fill = "none") +
        facet_wrap(vars(info2))
    p2 <- titre_all_exposed_dose3 %>% #filter(!elig_study_id %in% ids_detect) %>% 
        ggplot() + 
        geom_line(aes(x = time_until_bleed, y = 2 ^ info3 * 5, color = info2, group = paste(elig_study_id, info2)),
            size = 0.3) + 
        geom_point(aes(x = time_until_bleed, y = 2 ^ info3 * 5, fill = info2), shape = 21, alpha = 0.7) + 
        geom_line(data = fitted_exposed_type, aes(x = t, y = 2 ^ (fitted_val) * 5), color = "black", size = 1.5, alpha = 0.7) + 
        labs(x = "Days between vac dose and bleed", y = "Titre value", title = "Third vaccine dose with previous infection", color = "Variant") +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y ) + theme_bw() + 
        guides(color = "none", fill = "none") +
        facet_wrap(vars(info2))
    require(patchwork)
    p1 + p2 + plot_layout(guides = "collect")
    ggsave(here::here("outputs", "figs", paste0("model_data_fitted_", type_str, ".pdf")))
}

load(file = here::here("data", "df", "titre_all_naive.RData")) # titre_all_naive
load(file = here::here("data", "df", "titre_all_exposed.RData")) # titre_all_exposed

load(file = here::here("data", "df", "fitted_lines_naive.RData")) # post_pred_fitted_naive
load(file = here::here("data", "df", "fitted_lines_exposed.RData")) # post_pred_fitted_exposed


plot_model_data(titre_all_naive, titre_all_exposed, "Vac3")
plot_model_data(titre_all_naive, titre_all_exposed, "BA1")
plot_model_data(titre_all_naive, titre_all_exposed, "BA2")
plot_model_data(titre_all_naive, titre_all_exposed, "BA5")

plot_model_data_fitted(titre_all_naive, titre_all_exposed, post_pred_fitted_naive, post_pred_fitted_exposed, "Vac3")
plot_model_data_fitted(titre_all_naive, titre_all_exposed, post_pred_fitted_naive, post_pred_fitted_exposed, "BA1")
plot_model_data_fitted(titre_all_naive, titre_all_exposed, post_pred_fitted_naive, post_pred_fitted_exposed, "BA2")
plot_model_data_fitted(titre_all_naive, titre_all_exposed, post_pred_fitted_naive, post_pred_fitted_exposed, "BA5")


info2_levels <- c("Wild type", "Omicron BA1", "Omicron BA2", "Omicron BA5")
file_name_var <- c("wt", "ba1")
info3_levels <- c("Vac 3 naive", "BA 1 naive", "Vac 3 exposed", "BA 1 exposed")
file_name_type <- c("vac3_first", "ba1_first", "vac3_not_first", "ba1_not_first")

## ## ## ## ## ## ## ## ## ## ## ## ## 
## Plot summary plots ##
## ## ## ## ## ## ## ## ## ## ## ## ## 

post_fitted_list <- list(); k <- 1
for (i in 1:4) {
    for (j in 1:4) {
        for (exposure in c("naive", "exposed")) {
            info3_levels_str <- paste0(info3_levels[j], " ", exposure)
            file_name_type_2 <- paste0(file_name_type[j], "_", exposure)

            temp_stanfit <- readRDS(here::here("outputs", "stanfits", paste0("fit_", file_name_var[i], "_", file_name_type_2, ".Rdata")))
            post_fixed_eff <- temp_stanfit %>% as_draws(variables = c("boost_s", "boost_i", "wane_s", "t_p")) %>% as_draws_df
        
            post_fitted_list[[k]] <- post_fixed_eff %>% mutate(info2 = info2_levels[i],  type = info3_levels[j], exposure_type = exposure) 
            k <- k + 1
        }
    }
}


post_fitted <- post_fitted_list %>% bind_rows %>% mutate(info2 = factor(info2, levels = info2_levels), type = factor(type, levels = info3_levels)) %>%
    mutate(
        boost = t_p * boost_s, 
        wane_s = 100*wane_s
        )
post_fitted_mean <- post_fitted %>% group_by(info2, type, exposure_type) %>%
    summarise(wane_s = mean(wane_s), t_p = mean(t_p), boost_i = mean(boost_i), boost_s = mean(boost_s), boost = t_p * boost_s)

post_fitted %>% 
    ggplot() + 
        geom_point(aes(x = 2^wane_s, y = 2^boost, color = info2), size = 0.1, alpha = 0.1) + 
        stat_summary(data = post_fitted_mean, aes(x =  2^wane_s, y = 2^boost, fill = info2, shape = exposure_type), size = 3, alpha = 0.8, fun = "mean", geom = "point", 
            color = "black") + 
        facet_wrap(vars(type)) + 
        scale_shape_manual(values = c(21, 22)) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (-7:7)) + scale_x_continuous(trans = "log2", breaks = 2 ^ (-7:7)) +
        guides(color = "none") +
        theme_bw() + labs(y = "Titre wane 100 days after peak (GTR)", x = "Titre boost (GTR)", fill = "Variant")

post_fitted %>% 
    ggplot() + 
        geom_point(aes(y = 2^wane_s, x =t_p, color = info2), size = 0.1, alpha = 0.1) + 
        stat_summary(data = post_fitted_mean, aes(y =  2^wane_s, x = t_p, fill = info2), size = 3, alpha = 0.8, fun = "mean", geom = "point", 
            shape = 21, color = "black") + 
        facet_wrap(vars(type)) + 
        scale_y_continuous(trans = "log2", breaks = 2 ^ (-7:7)) +
        guides(color = "none") +
        theme_bw() + labs(y = "Titre wane 100 days after peak (GTR)", x = "Time from exposure until peak", fill = "Variant")



uniFill <- c("Wild type"="red","Omicron BA1"="green")
uniShape <- c("Infection nïave" = 21, "Exposed" = 22)

post_fitted_mean_alt <- post_fitted_mean %>% mutate(type2 = c(rep("Infection nïave", 2), rep("Exposed", 2))) %>% 
    mutate(dose = rep(c("Third vaccine dose", "BA1 infection"), 2))

post_fitted_mean_alt_wide <- post_fitted_mean_alt %>% select(!type) %>% pivot_wider(names_from = type2, values_from = c(wane_s, boost, t_p, boost_i, boost_s ))

post_fitted_mean_alt %>% 
    ggplot() + 
        geom_point(aes(x =  2^wane_s, y = 2^boost, fill = info2, shape = type2), color = "black", size = 5, alpha = 0.8) +
        geom_segment(data = post_fitted_mean_alt_wide,
                aes(x=2^`wane_s_Infection nïave`, xend=2^`wane_s_Exposed`, y = 2^`boost_Infection nïave`, yend= 2^`boost_Exposed` ), size = 0.4,
                arrow = arrow(length = unit(0.4, "cm")), alpha = 0.5) +
        scale_fill_manual(values = uniFill) +
        scale_shape_manual(values = uniShape) + 
        guides(fill = guide_legend(override.aes = list(shape = 21))) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (seq(-5, 5, 0.2)), labels = round(2 ^ (seq(-5, 5, 0.2)), 2)) +
        scale_x_continuous(trans = "log2", breaks = 2 ^ (seq(-5, 5, 0.2))) +
        theme_bw() + labs(x = "Wane", y = "Titre boost (GTR)", shape = "Exposure", fill = "Variant") + 
        facet_wrap(vars(dose))


post_fitted_mean_ba1 %>% 
    ggplot() + 
        geom_point(aes(x = 2^boost_i * 5, y = 2^boost, fill = info2, shape = type), color = "black", size = 5, alpha = 0.8) +
        scale_fill_manual(values = uniFill) +
        scale_shape_manual(values = uniShape) + 
        guides(fill = guide_legend(override.aes = list(shape = 21))) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (-7:7)) + 
        scale_x_continuous(trans = "log2", breaks = 2 ^ (-7:7)) +
        theme_bw() + labs(x = "Start titre", y = "Titre boost (GTR)", shape = "Exposure", fill = "Variant")



post_fitted_mean_ba1 %>% 
    ggplot() + 
        geom_point(aes(x = 2^wane_s, y = 2^boost, fill = info2, shape = type), color = "black", size = 5, alpha = 0.8) + 
      #  geom_segment(data = post_fitted_mean_wide,
      #          aes(x=2^`wane_s_Second dose`, xend=2^`wane_s_Third dose`, y = 2^`boost_Second dose`, yend= 2^`boost_Third dose` ), size = 0.4,
      #          arrow = arrow(length = unit(0.4, "cm")), alpha = 0.5) +
      #  geom_segment(data = post_fitted_mean_wide,
      #          aes(x=2^`wane_s_Third dose`, xend=2^`wane_s_Omicron BA1`, y = 2^`boost_Third dose`, yend = 2^`boost_Omicron BA1`), size = 0.4,
           #     arrow = arrow(length = unit(0.4, "cm")), alpha = 0.5) +
        scale_fill_manual(values = uniFill) +
        scale_shape_manual(values = uniShape) + 
        guides(fill = guide_legend(override.aes = list(shape = 21))) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (-7:7)) + scale_x_continuous(trans = "log2", breaks = 2 ^ (seq(-5, 5, 0.2)), labels = round(2 ^ (seq(-5, 5, 0.2)), 2)) +
        guides(color = "none") +
        theme_bw() + labs(x = "Titre wane 100 days after peak (GTR)", y = "Titre boost (GTR)", fill = "Variant")



post_fitted_mean_ba2 <- post_fitted_mean %>% filter(type %in% c("Omicron BA2 first", "Omicron BA2 not first"))
post_fitted_mean_wide_ba2 <- post_fitted_mean_ba2 %>% pivot_wider(names_from = type, values_from = c(t_p, boost_s, boost, wane_s))
uniShape <- c("Omicron BA2 first" = 21, "Omicron BA2 not first" = 22)


post_fitted_mean_ba2 %>% 
    ggplot() + 
        geom_point(aes(y = 2^wane_s, x = t_p, fill = info2, shape = type), color = "black", size = 5, alpha = 0.8) +
        scale_fill_manual(values = uniFill) +
        scale_shape_manual(values = uniShape) + 
        guides(fill = guide_legend(override.aes = list(shape = 21))) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (seq(-5, 5, 0.2)), labels = round(2 ^ (seq(-5, 5, 0.2)), 2)) +
        theme_bw() + labs(y = "Titre wane 100 days after peak (GTR)", x = "Time from exposure until peak", shape = "Exposure", fill = "Variant")



post_fitted_mean_ba2 %>% 
    ggplot() + 
        geom_point(aes(x = 2^boost_i * 5, y = 2^boost, fill = info2, shape = type), color = "black", size = 5, alpha = 0.8) +
        scale_fill_manual(values = uniFill) +
        scale_shape_manual(values = uniShape) + 
        guides(fill = guide_legend(override.aes = list(shape = 21))) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (-7:7)) + 
        scale_x_continuous(trans = "log2", breaks = 2 ^ (-7:7)) +
        theme_bw() + labs(x = "Start titre", y = "Titre boost (GTR)", shape = "Exposure", fill = "Variant")



post_fitted_mean_ba2 %>% 
    ggplot() + 
        geom_point(aes(x = 2^wane_s, y = 2^boost, fill = info2, shape = type), color = "black", size = 5, alpha = 0.8) + 
      #  geom_segment(data = post_fitted_mean_wide,
      #          aes(x=2^`wane_s_Second dose`, xend=2^`wane_s_Third dose`, y = 2^`boost_Second dose`, yend= 2^`boost_Third dose` ), size = 0.4,
      #          arrow = arrow(length = unit(0.4, "cm")), alpha = 0.5) +
      #  geom_segment(data = post_fitted_mean_wide,
      #          aes(x=2^`wane_s_Third dose`, xend=2^`wane_s_Omicron BA1`, y = 2^`boost_Third dose`, yend = 2^`boost_Omicron BA1`), size = 0.4,
           #     arrow = arrow(length = unit(0.4, "cm")), alpha = 0.5) +
        scale_fill_manual(values = uniFill) +
        scale_shape_manual(values = uniShape) + 
        guides(fill = guide_legend(override.aes = list(shape = 21))) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (-7:7)) + scale_x_continuous(trans = "log2", breaks = 2 ^ (seq(-5, 5, 0.2)), labels = round(2 ^ (seq(-5, 5, 0.2)), 2)) +
        guides(color = "none") +
        theme_bw() + labs(x = "Titre wane 100 days after peak (GTR)", y = "Titre boost (GTR)", fill = "Variant")

