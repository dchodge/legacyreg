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
uniShape <- c("Post third vaccine dose" = 21, "Post BA1 infection" = 22, "Post BA2 infection" = 23)

post_fitted_mean <- post_fitted_mean %>% mutate(boost_abs_cen = if_else(boost_abs < 7, boost_abs, 7)) 
post_fitted_mean_cens_dist <- post_fitted_mean %>% mutate(diff = boost_abs - boost_abs_cen) %>% filter(diff > 0)

############################
### Without uncertainty ###
############################
# Relative boosting
post_fitted_mean %>% 
    ggplot() + 
        geom_point(aes(x =  2^wane_s, y = 2^boost, shape = type, fill = exposure_type), size = 4, alpha = 0.8, 
            color = "black")  + 
        facet_grid(cols = vars(info2)) + 
        scale_fill_manual(values = uniFill) +
        scale_shape_manual(values = uniShape) +
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21)))) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (-7:7)) +
        scale_x_continuous(trans = "log2", breaks = 2 ^ seq(-7, 1, 0.5), labels = round(2 ^ seq(-7, 1, 0.5), 2 ), limits = c(0.3, 1) ) +
        guides(color = "none") +
        theme_bw() + labs(x = "Titre wane 100 days after peak (GTR)", y = "Titre boost at peak (GTR)", fill = "Exposure history")
ggsave(here::here("outputs", "figs", "main_fig_faceted_boost.png"), width = 9, height = 7)

# Absolute boosting
post_fitted_mean %>% 
    ggplot() + 
        geom_hline(yintercept = 5 * 2^7, linetype = "dotted") +
        geom_segment(data = post_fitted_mean_cens_dist,
            aes(x =  2^wane_s, y = 5 * 2^boost_abs_cen, xend =  2^wane_s, yend = 5 * 2^(boost_abs)),  
            size = 0.5, alpha = 0.8, linetype = "dashed",
            color = "gray15") +
        geom_point(data = post_fitted_mean_cens_dist,
            aes(x =  2^wane_s, y = 5 * 2^boost_abs),  
            size = 5, alpha = 0.8, shape = 4,
            color = "gray15") +
        geom_point(aes(x =  2^wane_s, y = 5 * 2^boost_abs_cen, shape = type, fill = exposure_type), size = 4, alpha = 0.8, 
            color = "black") +
        facet_grid(cols = vars(info2)) + 
        scale_fill_manual(values = uniFill) +
        scale_shape_manual(values = uniShape) +
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21)))) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y) + scale_x_continuous(trans = "log2", breaks = 2 ^ seq(-7, 1, 0.5), labels = round(2 ^ seq(-7, 1, 0.5), 2 ), limits = c(0.3, 1) ) +
        guides(color = "none") +
        theme_bw() + labs(x = "Titre wane 100 days after peak (GTR)", y = "Titre value at peak", fill = "Infection history", shape = "Exposure type")

ggsave(here::here("outputs", "figs", "main_fig_faceted_titre.png"), width = 9, height = 7)

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
        geom_segment(data = post_fitted_mean,
            aes(x =  2^wane_s, y = 5 * 2^boost_abs_cen, xend =  2^wane_s, yend = 5 * 2^(boost_abs)),  
            size = 0.1, alpha = 0.8, 
            color = "gray") +
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


load(file = here::here("data", "df", "fitted_lines_naive.RData"))
load(file = here::here("data", "df", "fitted_lines_exposed.RData"))

naive_pred_post_plot <- post_pred_fitted_naive %>% filter(type != "BA5", info2 != "Omicron BA5") %>% mutate(hist = "naive")
exposed_pred_post_plot <- post_pred_fitted_exposed %>% filter(type != "BA5",info2 != "Omicron BA5") %>% mutate(hist = "exposed")

pred_post_plot <- bind_rows(naive_pred_post_plot, exposed_pred_post_plot)

relabel_type <- c("Vac3" = "Post third vaccine dose", "BA1" = "Post BA1 infection", "BA2" = "Post BA2 infection")
relabel_exposure_type <- c("naive" = "Infection naive", "exposed" = "Previously infected")

pred_post_plot <- pred_post_plot %>% 
    mutate(type = recode(type, !!!relabel_type)) %>%
    mutate(hist = recode(hist, !!!relabel_exposure_type))

pred_post_plot <- pred_post_plot %>% unite("new_col", type, hist)

relabel_type_new_cl <- 
    c("Post third vaccine dose_Infection naive" = "Third dose after 2 previous antigen exposures", #\n (All vaccines)", 
     "Post third vaccine dose_Previously infected" = "Third dose after 3 previous antigen exposures",#\n (2 vaccine doses and 1 infection)",
     "Post BA1 infection_Infection naive" = "BA1 infection after 3 previous antigen exposures",#\n (3 vaccines doses)",
     "Post BA1 infection_Previously infected" = "BA1 infection after 4 previous antigen exposures",#\n (3 vaccine doses and 1 infection)",
     "Post BA2 infection_Infection naive" = "BA2 infection after 3 previous antigen exposures",#\n (3 vaccines doses)",
     "Post BA2 infection_Previously infected" = "BA2 infection after 4 previous antigen exposures"#\n (3 vaccine doses and 1 infection)"
     )

pred_post_plot <- pred_post_plot %>% 
    mutate(new_col = recode(new_col, !!!relabel_type_new_cl)) 

pred_post_plot <- pred_post_plot %>% separate(new_col, c("Exposure", "Infection history"), " after ", remove = FALSE)


labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")

pred_post_plot %>% filter(t < 150) %>%
    ggplot() +
        geom_hline(yintercept = 5 * 2^7, linetype = "dashed", color = "gray30") + 
        geom_hline(yintercept = 5 * 2^1, linetype = "dashed", color = "gray30") + 
        geom_line(aes(x = t, y = 5 * 2^fitted_val, color = Exposure), size = 1.5, alpha = 0.8) + 
        facet_grid(cols = vars(info2), rows = vars(`Infection history`)) + 
      #  scale_color_manual(values = c("red", "darkred", "blue", "darkblue", "lightgreen", "green")) +
        scale_linetype_manual(values = c("dotted", "dashed", "solid")) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y, limits = c(10, 2000)) + 
        theme_bw() +
        labs(x = "Time post exposure (days)", y = "Titre value", color = "Recent exposure", linetype = "Infection history")


data.frame(
    x = c(1, 20, 120),
    y = c(4, 7, 6)
) %>% 
    ggplot() + 
        geom_hline(yintercept = 5 * 2^7, linetype = "dashed", color = "gray30") + 
        geom_hline(yintercept = 5 * 2^1, linetype = "dashed", color = "gray30") + 
        geom_line(aes(x, 5 * 2^y), size = 2) + 
        scale_y_continuous(trans = "log2",  breaks = 2 ^ (1:7) * 5, labels = labs_plot_y, limits = c(10, 1200)) + 
        theme_minimal() + 
        geom_segment(aes(x = 15, y = 10, xend = 15, yend = 5 * 2^7),
                  arrow = arrow(length = unit(0.5, "cm"))) +
       geom_segment(aes(x = 35, y = 5 * 2^4, xend = 35, yend = 5 * 2^7),
                  arrow = arrow(length = unit(0.5, "cm"))) +

        labs(x = "Days post infection", y = "Titre value")



ggsave(here::here("outputs", "figs", "final_figs", "fig1.pdf"))

