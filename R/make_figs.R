## Load model data
require(magrittr)
require(dplyr)
require(ggplot2)
require(posterior)
require(tidyr)
## ## ## ## ## ## ## ## ## ## ## ## ## 
## Plot the full titres and fits ##
## ## ## ## ## ## ## ## ## ## ## ## ## 

plot_model_data <- function(titre_all_naive, titre_all_exposed, type_str, title_str) {
    type_str_fil_naive <- paste0(type_str, " naive")
    type_str_fil_exposed <- paste0(type_str, " exposed")

    titre_all_naive_dose3 <- titre_all_naive %>% filter(type == !!type_str_fil_naive) %>% filter(info2 != "Omicron BA5")
    titre_all_exposed_dose3 <- titre_all_exposed %>% filter(type == !!type_str_fil_exposed) %>% filter(info2 != "Omicron BA5")

    labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")

    lower <- 5;
    upper <- 2 ^ (7) * 5;

    p1 <- titre_all_naive_dose3 %>% 
        ggplot() + 
        geom_line(aes(x = time_until_bleed, y = 2 ^ info3 * 5, color = info2, group = paste(elig_study_id, info2)),
            size = 0.3) + 
        geom_point(aes(x = time_until_bleed, y = 2 ^ (info3 ) * 5, fill = info2), shape = 21, alpha = 0.7) + 
        labs(x = "Days between vac dose and bleed", y = "Titre value", title = paste0(title_str, " without previous infection"), color = "Variant") +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y, limits = c(lower, upper) ) + theme_bw() + 
        scale_x_continuous(limits = c(0, NA)) +
        guides(color = "none", fill = "none") +
        facet_grid(cols = vars(info2))
    p2 <- titre_all_exposed_dose3 %>% #filter(!elig_study_id %in% ids_detect) %>% 
        ggplot() + 
        geom_line(aes(x = time_until_bleed, y = 2 ^ info3 * 5, color = info2, group = paste(elig_study_id, info2)),
            size = 0.3) + 
        geom_point(aes(x = time_until_bleed, y = 2 ^ info3 * 5, fill = info2), shape = 21, alpha = 0.7) + 

        labs(x = "Days between vac dose and bleed", y = "Titre value", title = paste0(title_str, " with previous infection"), color = "Variant") +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y , limits = c(lower, upper)) + theme_bw() + 
        guides(color = "none", fill = "none") +
        facet_grid(cols = vars(info2))
    require(patchwork)
    p1 + p2 + plot_layout(guides = "collect")
    ggsave(here::here("outputs", "figs", paste0("model_data_", type_str, ".png")), width = 10, height = 6)
}

plot_model_data_fitted <- function(titre_all_naive, titre_all_exposed, fitted_naive, fitted_exposed, type_str, title_str) {
    type_str_fil_naive <- paste0(type_str, " naive")
    type_str_fil_exposed <- paste0(type_str, " exposed")

    titre_all_naive_dose3 <- titre_all_naive %>% filter(type == !!type_str_fil_naive) %>% filter(info2 != "Omicron BA5")
    titre_all_exposed_dose3 <- titre_all_exposed %>% filter(type == !!type_str_fil_exposed) %>% filter(info2 != "Omicron BA5") 

    fitted_naive_type  <- fitted_naive %>% filter(type == !!type_str) %>% filter(info2 != "Omicron BA5")
    fitted_exposed_type <- fitted_exposed %>% filter(type == !!type_str) %>% filter(info2 != "Omicron BA5")

    lower <- 5;
    upper <- 2 ^ (8.75) * 5;

    labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")

    p1 <- titre_all_naive_dose3 %>% 
        ggplot() + 
        geom_line(aes(x = time_until_bleed, y = 2 ^ info3 * 5, color = info2, group = paste(elig_study_id, info2)),
            size = 0.3) + 
        geom_point(aes(x = time_until_bleed, y = 2 ^ (info3 ) * 5, fill = info2), shape = 21, alpha = 0.7) + 
        geom_line(data = fitted_naive_type, aes(x = t, y = 2 ^ (fitted_val) * 5), color = "black", size = 1.5, alpha = 0.7) + 
        labs(x = "Days between vac dose and bleed", y = "Titre value", title = paste0(title_str, " without previous infection"), color = "Variant") +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y, limits = c(lower, upper) ) + theme_bw() + 
        scale_x_continuous(limits = c(0, NA)) +
        guides(color = "none", fill = "none") +
        facet_grid(cols = vars(info2))
    p2 <- titre_all_exposed_dose3 %>% #filter(!elig_study_id %in% ids_detect) %>% 
        ggplot() + 
        geom_line(aes(x = time_until_bleed, y = 2 ^ info3 * 5, color = info2, group = paste(elig_study_id, info2)),
            size = 0.3) + 
        geom_point(aes(x = time_until_bleed, y = 2 ^ info3 * 5, fill = info2), shape = 21, alpha = 0.7) + 
        geom_line(data = fitted_exposed_type, aes(x = t, y = 2 ^ (fitted_val) * 5), color = "black", size = 1.5, alpha = 0.7) + 
        labs(x = "Days between vac dose and bleed", y = "Titre value", title = paste0(title_str, " with previous infection"), color = "Variant") +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y, limits = c(lower, upper) ) + theme_bw() + 
        guides(color = "none", fill = "none") +
        facet_grid(cols = vars(info2))
    require(patchwork)
    p1 + p2 + plot_layout(guides = "collect")
    ggsave(here::here("outputs", "figs", paste0("model_data_fitted_", type_str, ".png")), width = 10, height = 6)
}

load(file = here::here("data", "df", "titre_all_naive.RData")) # titre_all_naive
load(file = here::here("data", "df", "titre_all_exposed.RData")) # titre_all_exposed

load(file = here::here("data", "df", "fitted_lines_naive.RData")) # post_pred_fitted_naive
load(file = here::here("data", "df", "fitted_lines_exposed.RData")) # post_pred_fitted_exposed


plot_model_data(titre_all_naive, titre_all_exposed, "Vac3", "Third vaccine dose")
plot_model_data(titre_all_naive, titre_all_exposed, "BA1", "Omicron BA1 infection")
plot_model_data(titre_all_naive, titre_all_exposed, "BA2", "Omicron BA2 infection")
plot_model_data(titre_all_naive, titre_all_exposed, "BA5", "Omicron BA5 infection")

plot_model_data_fitted(titre_all_naive, titre_all_exposed, post_pred_fitted_naive, post_pred_fitted_exposed, "Vac3", "Third vaccine dose")
plot_model_data_fitted(titre_all_naive, titre_all_exposed, post_pred_fitted_naive, post_pred_fitted_exposed, "BA1", "Omicron BA1 infection")
plot_model_data_fitted(titre_all_naive, titre_all_exposed, post_pred_fitted_naive, post_pred_fitted_exposed, "BA2", "Omicron BA2 infection")
plot_model_data_fitted(titre_all_naive, titre_all_exposed, post_pred_fitted_naive, post_pred_fitted_exposed, "BA5", "Omicron BA5 infection")


info2_levels <- c("Wild type", "Omicron BA1", "Omicron BA2", "Omicron BA5")
file_name_var <- c("wt", "ba1", "ba2")
info3_levels <- c("Vac 3", "BA 1", "BA 2", "BA 5")
file_name_type <- c("vac3", "ba1", "ba2", "ba3")
labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")

## ## ## ## ## ## ## ## ## ## ## ## ## 
## Plot summary plots ##
## ## ## ## ## ## ## ## ## ## ## ## ## 

post_fitted_list <- list(); k <- 1
for (i in 1:3) {
    for (j in 1:3) {
        for (exposure in c("naive", "exposed")) {
            info3_levels_str <- paste0(info3_levels[j])
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
        boost_abs = t_p * boost_s + boost_i, 
        wane_s = 100 * wane_s, .after = t_p
        )

relabel_type <- c("Vac 3" = "Post third vaccine dose", "BA 1" = "Post BA1 infection", "BA 2" = "Post BA2 infection")
relabel_exposure_type <- c("naive" = "Infection naive", "exposed" = "Previously infected")

post_fitted <- post_fitted %>% mutate(type = recode(type, !!!relabel_type)) %>%
    mutate(exposure_type = recode(exposure_type, !!!relabel_exposure_type))


post_fitted_mean <- post_fitted %>% group_by(type, info2, exposure_type) %>%
    summarise(across(boost_s:boost_abs, mean))
# Without uncertainty 
post_fitted_mean_wide <- post_fitted_mean %>% ungroup %>% 
    pivot_wider(names_from = exposure_type, values_from = c(wane_s:boost_s ))

uniFill <- c("Infection naive" = "#1A85FF", "Previously infected" = "#D41159")
uniShape <- c("Infection naive" = 21, "Previously infected" = 22)


############################
### Without uncertainty ###
############################
# Relative boosting
post_fitted_mean %>% 
    ggplot() + 
        geom_point(aes(x =  2^wane_s, y = 2^boost, shape = exposure_type, fill = exposure_type), size = 4, alpha = 0.8, 
            color = "black") + 
        facet_grid(cols = vars(info2), rows = vars(type)) + 
        scale_fill_manual(values = uniFill) +
        scale_shape_manual(values = uniShape) +
        guides(fill = guide_legend(override.aes = list(shape = c(21, 22)))) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (-7:7), limits = c(1, 128)) +
        scale_x_continuous(trans = "log2", breaks = 2 ^ seq(-7, 1, 0.5), labels = round(2 ^ seq(-7, 1, 0.5), 2 ), limits = c(0.3, 1) ) +
        guides(color = "none", shape = "none") +
        theme_bw() + labs(x = "Titre wane 100 days after peak (GTR)", y = "Titre boost at peak (GTR)", fill = "Exposure history")

# Absolute boosting
post_fitted_mean %>% 
    ggplot() + 
        geom_point(aes(x =  2^wane_s, y = 5 * 2^boost_abs, shape = exposure_type, fill = exposure_type), size = 4, alpha = 0.8, 
            color = "black") + 
        facet_grid(cols = vars(info2), rows = vars(type)) + 
        scale_fill_manual(values = uniFill) +
        scale_shape_manual(values = uniShape) +
        guides(fill = guide_legend(override.aes = list(shape = c(21, 22)))) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y) + scale_x_continuous(trans = "log2", breaks = 2 ^ seq(-7, 1, 0.5), labels = round(2 ^ seq(-7, 1, 0.5), 2 ), limits = c(0.3, 1) ) +
        guides(color = "none", shape = "none") +
        theme_bw() + labs(x = "Titre wane 100 days after peak (GTR)", y = "Titre value at peak", fill = "Exposure history")
ggsave(here::here("outputs", "figs", "main_fig_faceted.png"))

############################
### With uncertainty ###
############################

post_fitted %>% filter(exposure_type == "Previously infected", info2 == "Omicron BA1", type == "Post third vaccine dose")

# Relative boosting
post_fitted %>% 
    ggplot() + 
        geom_density_2d(aes(x =  2^wane_s, y = 2^boost, color = exposure_type)) + 
        facet_grid(cols = vars(info2), rows = vars(type)) + 
        geom_point(data = post_fitted_mean, aes(x =  2^wane_s, y = 2^boost, shape = exposure_type, fill = exposure_type), size = 3, alpha = 0.8, 
           color = "black") +
        scale_color_manual(values = uniFill) +
        scale_fill_manual(values = uniFill) +
        scale_shape_manual(values = uniShape) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (-7:7), limits = c(0.1, 640)) +
        scale_x_continuous(trans = "log2", breaks = 2 ^ seq(-7, 1, 0.5), labels = round(2 ^ seq(-7, 1, 0.5), 2 ) ) +
        guides(color = "none", shape = "none") +
        theme_bw() + labs(x = "Titre wane 100 days after peak (GTR)", y = "Titre boost at peak (GTR)", fill = "Exposure history (variant)")

# Absolute boosting
post_fitted %>% 
    ggplot() + 
        geom_density_2d(aes(x =  2^wane_s, y = 5 * 2^boost_abs, color = exposure_type), level = c(0.5, 0.95)) + 
        facet_grid(cols = vars(info2), rows = vars(type)) + 
       geom_point(data = post_fitted_mean, aes(x =  2^wane_s, y = 5 * 2^boost_abs, shape = exposure_type, fill = exposure_type), size = 3, alpha = 0.8, 
            color = "black") +
        scale_color_manual(values = uniFill) +
        scale_fill_manual(values = uniFill) +
        scale_shape_manual(values = uniShape) +
        scale_y_continuous(trans = "log2", breaks = 5 * 2 ^ (-7:7), limits = c(2, 640)) +
        scale_x_continuous(trans = "log2", breaks = 2 ^ seq(-7, 1, 0.5), labels = round(2 ^ seq(-7, 1, 0.5), 2 ) ) +
        guides(color = "none", shape = "none") +
        theme_bw() + labs(x = "Titre wane 100 days after peak (GTR)", y = "Titre value at peak", fill = "Exposure history (variant)")




post_fitted %>% 
    ggplot() + 
        geom_point(aes(x = 2^wane_s, y = 2^boost, color = model), size = 0.1, alpha = 0.1) + 
        stat_summary(data = post_fitted_mean, aes(x =  2^wane_s, y = 2^boost, shape = model, fill = model), size = 3, alpha = 0.8, fun = "mean", geom = "point", 
            color = "black") + 
        geom_segment(data = post_fitted_mean_wide,
                aes(x = 2^`wane_s_naive`, xend=2^`wane_s_exposed`, 
                    y = 2^`boost_naive`, yend= 2^`boost_exposed` ), size = 0.4,
                arrow = arrow(length = unit(0.4, "cm")), alpha = 0.5)  +
        facet_grid(rows = vars(type), cols = vars()) + 
        scale_fill_manual(values = uniFill) +
        scale_color_manual(values = uniFill) +
        scale_shape_manual(values = c(21, 22, 21, 22, 21, 22)) +
        guides(fill = guide_legend(override.aes = list(shape = c(21, 22, 21, 22, 21, 22)))) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (-7:7)) + scale_x_continuous(trans = "log2",breaks = 2 ^ seq(-7, 1, 0.5), labels = round(2 ^ seq(-7, 1, 0.5), 2 ), limits = c(0.3, 1) ) +
        guides(color = "none", shape = "none") +
        theme_bw() + labs(x = "Titre wane 100 days after peak (GTR)", y = "Titre boost at peak (GTR)", fill = "Exposure history (variant)")
ggsave(here::here("outputs", "figs", "main_fig_uncert.pdf"))

names(post_fitted_mean_wide)


ggsave(here::here("outputs", "figs", "main_fig.pdf"))
