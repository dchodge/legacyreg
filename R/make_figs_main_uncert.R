## Load model data
require(magrittr)
require(dplyr)
require(ggplot2)
require(posterior)
require(tidyr)
## ## ## ## ## ## ## ## ## ## ## ## ## 

load(file = here::here("data", "df", "titre_all_naive.RData")) # titre_all_naive
load(file = here::here("data", "df", "titre_all_exposed.RData")) # titre_all_exposed

load(file = here::here("data", "df", "fitted_lines_naive_uncert.RData"))
load(file = here::here("data", "df", "fitted_lines_exposed_uncert.RData"))



relabel_type_new_cl <- 
    c("Third vaccine dose_Infection naive" = "2 previous \nantigen exposures", #\n (All vaccines)", 
     "Third vaccine dose_Previously infected" = "3 previous \nantigen exposures",#\n (2 vaccine doses and 1 infection)",
     "BA1 infection_Infection naive" = "3 previous \nantigen exposures",#\n (3 vaccines doses)",
     "BA1 infection_Previously infected" = "4 previous \nantigen exposures",#\n (3 vaccine doses and 1 infection)",
     "BA2 infection_Infection naive" = "3 previous \nantigen exposures",#\n (3 vaccines doses)",
     "BA2 infection_Previously infected" = "4 previous \nantigen exposures"#\n (3 vaccine doses and 1 infection)"
     )


## ## ## ## ## ## ## ## ## ## ## ## ## 
# Get summary points data frame
## ## ## ## ## ## ## ## ## ## ## ## ## 
info2_levels <- c("Ancestral", "Omicron BA1", "Omicron BA2", "Omicron BA5")
file_name_var <- c("wt", "ba1", "ba2")
info3_levels <- c("Vac 3", "BA 1", "BA 2", "BA 5")
file_name_type <- c("vac3", "ba1", "ba2", "ba3")

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
        abs_titre = t_p * boost_s + boost_i, 
        wane_s = 100 * wane_s, .after = t_p
        )

relabel_type_1 <- c("Vac 3" = "Third vaccine dose", "BA 1" = "BA1 infection", "BA 2" = "BA2 infection")
relabel_exposure_type_1 <- c("naive" = "Infection naive", "exposed" = "Previously infected")

post_fitted_recode <- post_fitted %>% mutate(type = recode(type, !!!relabel_type_1)) %>%
    mutate(infection_history = recode(exposure_type, !!!relabel_exposure_type_1)) %>%
    rename(measured_variant = info2, recent_exposure = type)

## ## ## ## ## ## ## ## ## ## ## ## ## 
# Get post pred lines data frame
## ## ## ## ## ## ## ## ## ## ## ## ## 

naive_pred_post_plot <- post_pred_fitted_naive_uncert %>% filter(type != "BA5", info2 != "Omicron BA5") %>%
    mutate(infection_history = "Infection naive")
exposed_pred_post_plot <- post_pred_fitted_exposed_uncert %>% filter(type != "BA5",info2 != "Omicron BA5") %>%
    mutate(infection_history = "Previously infected")

pred_post_plot <- bind_rows(naive_pred_post_plot, exposed_pred_post_plot) %>%
    rename(measured_variant = info2, recent_exposure = type)

relabel_type_2 <- c("Vac3" = "Third vaccine dose", "BA1" = "BA1 infection", "BA2" = "BA2 infection")
relabel_measured_variant_2 <- c("Wild type" = "Ancestral")

pred_post_plot_recode <- pred_post_plot %>% 
     mutate(recent_exposure = recode(recent_exposure, !!!relabel_type_2)) %>%
     mutate(measured_variant = recode(measured_variant, !!!relabel_measured_variant_2)) 

## ## ## ## ## ## ## ## ## ## ## ## ## 
# Can the post_fitted and pred_post_plot
## ## ## ## ## ## ## ## ## ## ## ## ## 

post_fitted_all <- post_fitted_recode %>% 
    mutate(abs_titre_censored = if_else(abs_titre < 7, abs_titre, 7)) %>% 
    unite("number_ag_exposure", recent_exposure, infection_history, remove = FALSE) %>%
    mutate(number_ag_exposure = recode(number_ag_exposure, !!!relabel_type_new_cl)) 

post_fitted_all_cens_dist <- post_fitted_all %>% mutate(diff = abs_titre - abs_titre_censored) %>% filter(diff > 0)

pred_post_plot_clean <- pred_post_plot_recode %>% group_by(measured_variant, recent_exposure, infection_history) %>%
    unite("number_ag_exposure", recent_exposure, infection_history, remove = FALSE) %>%
    mutate(number_ag_exposure = recode(number_ag_exposure, !!!relabel_type_new_cl)) 


# The plots!

labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")
labs_plot_y_gtr <- c("1", "2", "4", "8", "16", "32", "64", "128")
colorsdefined <- c("Third vaccine dose" = "#AA4A44", "BA1 infection" = "#0d98ba", "BA2 infection" = "#1c39bb")

label_text_size <- 3
p1 <- data.frame(
    x = c(1, 20, 120),
    y = c(3, 6, 4)
) %>% 
    ggplot() + 
        geom_hline(yintercept = 5 * 2^7, linetype = "dashed", color = "gray30") + 
        geom_hline(yintercept = 5 * 2^1, linetype = "dashed", color = "gray30") + 
        geom_line(aes(x, 5 * 2^y), size = 2) + 
        scale_y_continuous(trans = "log2",  breaks = 2 ^ (1:7) * 5, labels = labs_plot_y, limits = c(40, 1200)) + 
        scale_x_continuous(breaks = seq(0, 120, 20)) + 
        theme_bw() + 
        geom_segment(aes(x = 12, y = 5 * 2^3, xend = 12, yend = 5 * 2^6),
                  arrow = arrow(length = unit(0.5, "cm")), size = 2, color = "white") +
        geom_segment(aes(x = 12, y = 5 * 2^3, xend = 12, yend = 5 * 2^6),
                  arrow = arrow(length = unit(0.5, "cm")), size = .5) +
        ggtext::geom_richtext(aes(x = 6, y = 5 * 2^4.5, label = "Peak titre value"), size = label_text_size, angle = 90) +
        geom_segment(aes(x = 20, y = 5 * 2^6.5, xend = 120, yend = 5 * 2^6.5),
                  arrow = arrow(ends='both', length = unit(0.5, "cm")), size = 2, color = "white") +
        geom_segment(aes(x = 20, y = 5 * 2^6.5, xend = 120, yend = 5 * 2^6.5),
                  arrow = arrow(ends='both', length = unit(0.5, "cm")), size = .5) +
        ggtext::geom_richtext(aes(x = 65, y = 5 * 2^6.9, label = "100 days post peak"), size = label_text_size) +
        geom_segment(aes(x = 120, y = 5 * 2^6, xend = 120, yend = 5 * 2^4),
                  arrow = arrow(length = unit(0.5, "cm")), size = 2, color = "white") +
        geom_segment(aes(x = 120, y = 5 * 2^6, xend = 120, yend = 5 * 2^4),
                  arrow = arrow(length = unit(0.5, "cm")), size = .5) +
        geom_segment(aes(x = 110, y = 5 * 2^6, xend = 130, yend = 5 * 2^6), linetype = "dashed", 
            color = "gray30") +
        geom_segment(aes(x = 110, y = 5 * 2^4, xend = 130, yend = 5 * 2^4), linetype = "dashed", 
            color = "gray30") +
      geom_segment(aes(x = 2, y = 5 * 2^6, xend = 22, yend = 5 * 2^6), linetype = "dashed", 
            color = "gray30") +
        ggtext::geom_richtext(aes(x = 115, y = 5 * 2^5, label = "Titre wane"), size = label_text_size, angle = 90) +
        labs(x = "Days post infection", y = "Titre value") + 
        theme(text = element_text(size = 15))
require(ggdist)
p2 <- pred_post_plot_clean %>% filter(t < 150) %>%
    ggplot() +
        geom_hline(yintercept = 7, linetype = "dashed", color = "gray30") + 
        geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") + # , linetype = infection_history
       # geom_line(aes(x = t, y = 5 * 2^fitted_val, color = recent_exposure, group = paste0(sample_no, infection_history, recent_exposure)), size = 0.2, alpha = 0.2) + 
        stat_ribbon(aes(x = t, y = fitted_val, fill = recent_exposure), point_interval = "mean_qi", .width = 0.95, alpha = 0.6) +
        stat_summary(aes(x = t, y = fitted_val, group = recent_exposure), color = "white", fun = "mean", geom = "line", size = 1, alpha = 1) + 
        stat_summary(aes(x = t, y = fitted_val, color = recent_exposure, linetype = infection_history), fun = "mean", geom = "line", size = 0.5, alpha = 1) + 
        facet_grid(cols = vars(measured_variant), rows = vars(number_ag_exposure)) + 
        scale_linetype_manual(values = c("dashed", "solid")) +
        scale_y_continuous(breaks = 1:7, labels = labs_plot_y, limits = c(1, 8.5)) + 
        theme_bw() +
        guides(linetype = guide_legend(override.aes = list(size = c(0.5, 0.5))), fill = "none") +
        theme(text = element_text(size = 15)) +
        scale_color_manual(values = colorsdefined) +
        scale_fill_manual(values = colorsdefined) +
        labs(x = "Time post exposure (days)", y = "Titre value", color = "Most recent exposure", linetype = "Infection history")

post_fitted_mean <- post_fitted_all %>% group_by(measured_variant, number_ag_exposure, recent_exposure, infection_history) %>%
    summarise(across(boost_s:abs_titre, mean))

p3 <- post_fitted_all %>% 
    ggplot() + 
        geom_hline(yintercept = 5 * 2^7, linetype = "dashed", color = "gray30") + 
        geom_hline(yintercept = 5 * 2^1, linetype = "dashed", color = "gray30") + 
        geom_point(aes(x =  2^wane_s, y = 5 * 2^abs_titre, color = recent_exposure), 
            size = 0.05, alpha = 0.05) +
        geom_point(data = post_fitted_mean, aes(x =  2^wane_s, y = 5 * 2^abs_titre, fill = recent_exposure,
            shape = infection_history), 
            color = "black", size = 3, alpha = 1) +
        facet_grid(cols = vars(measured_variant), rows = vars(number_ag_exposure)) + 
        scale_shape_manual(values = c("Infection naive" = 21, "Previously infected" = 22)) +
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21)))) +
        scale_y_continuous(trans = "log2", limits = c(10, 4000), breaks = 2 ^ (1:7) * 5, labels = labs_plot_y) + scale_x_continuous(trans = "log2", breaks = 2 ^ seq(-7, 1, 0.5), labels = round(2 ^ seq(-7, 1, 0.5), 2 ), limits = c(0.3, 1) ) +
        guides(color = "none") +
        theme_bw() + labs(x = "Titre wane 100 days after peak (GTR)", y = "Titre value at peak", fill = "Most recent exposure", shape = "Infection history") + 
        scale_fill_manual(values = colorsdefined) +
        scale_color_manual(values = colorsdefined) +
        theme(text = element_text(size = 15))


require(patchwork)
p1 / p2 / p3  + plot_layout(heights = c(1, 2, 2)) + plot_annotation(tag_levels = "A") 
ggsave(here::here("outputs", "figs", "final_figs", "fig1_uncert.png"), height = 16, width = 12)



colorsdefined <- c("Third vaccine dose" = "#AA4A44", "BA1 infection" = "#0d98ba", "BA2 infection" = "#1c39bb")

pred_post_plot_clean

label_text_size <- 3
p1 <- data.frame(
    x = c(1, 20, 120),
    y = c(3, 6, 4)
) %>% 
    ggplot() + 
        geom_hline(yintercept = 5 * 2^7, linetype = "dashed", color = "gray30") + 
        geom_hline(yintercept = 5 * 2^1, linetype = "dashed", color = "gray30") + 
        geom_line(aes(x, 5 * 2^y), size = 2) + 
        scale_y_continuous(trans = "log2",  breaks = 2 ^ (1:7) * 5, labels = labs_plot_y, limits = c(40, 1200)) + 
        scale_x_continuous(breaks = seq(0, 120, 20)) + 
        theme_bw() + 
        geom_segment(aes(x = 12, y = 5 * 2^3, xend = 12, yend = 5 * 2^6),
                  arrow = arrow(length = unit(0.5, "cm")), size = 2, color = "white") +
        geom_segment(aes(x = 12, y = 5 * 2^3, xend = 12, yend = 5 * 2^6),
                  arrow = arrow(length = unit(0.5, "cm")), size = .5) +
        ggtext::geom_richtext(aes(x = 6, y = 5 * 2^4.5, label = "Titre boost"), size = label_text_size, angle = 90) +
        geom_segment(aes(x = 20, y = 5 * 2^6.5, xend = 120, yend = 5 * 2^6.5),
                  arrow = arrow(ends='both', length = unit(0.5, "cm")), size = 2, color = "white") +
        geom_segment(aes(x = 20, y = 5 * 2^6.5, xend = 120, yend = 5 * 2^6.5),
                  arrow = arrow(ends='both', length = unit(0.5, "cm")), size = .5) +
        ggtext::geom_richtext(aes(x = 65, y = 5 * 2^6.9, label = "100 days post peak"), size = label_text_size) +
        geom_segment(aes(x = 120, y = 5 * 2^6, xend = 120, yend = 5 * 2^4),
                  arrow = arrow(length = unit(0.5, "cm")), size = 2, color = "white") +
        geom_segment(aes(x = 120, y = 5 * 2^6, xend = 120, yend = 5 * 2^4),
                  arrow = arrow(length = unit(0.5, "cm")), size = .5) +
        geom_segment(aes(x = 110, y = 5 * 2^6, xend = 130, yend = 5 * 2^6), linetype = "dashed", 
            color = "gray30") +
        geom_segment(aes(x = 110, y = 5 * 2^4, xend = 130, yend = 5 * 2^4), linetype = "dashed", 
            color = "gray30") +
       geom_segment(aes(x = 2, y = 5 * 2^6, xend = 22, yend = 5 * 2^6), linetype = "dashed", 
            color = "gray30") +
         geom_segment(aes(x = 2, y = 5 * 2^3, xend = 22, yend = 5 * 2^3), linetype = "dashed", 
            color = "gray30") +
        ggtext::geom_richtext(aes(x = 115, y = 5 * 2^5, label = "Titre wane"), size = label_text_size, angle = 90) +
        labs(x = "Days post infection", y = "Titre value") + 
        theme(text = element_text(size = 15))

p2 <- pred_post_plot_clean %>% group_by(sample_no, measured_variant, recent_exposure, infection_history, number_ag_exposure) %>% 
    mutate(fitted_val_boost = fitted_val - min(fitted_val)) %>% filter(t < 150) %>%
    ggplot() +
       # geom_line(aes(x = t, y = 5 * 2^fitted_val, color = recent_exposure, group = paste0(sample_no, infection_history, recent_exposure)), size = 0.2, alpha = 0.2) + 
        stat_ribbon(aes(x = t, y = fitted_val_boost, fill = recent_exposure), point_interval = "mean_qi", .width = 0.95, alpha = 0.6) +
        stat_summary(aes(x = t, y = fitted_val_boost, group = recent_exposure), color = "white", fun = "mean", geom = "line", size = 1, alpha = 1) + 
        stat_summary(aes(x = t, y = fitted_val_boost, color = recent_exposure, linetype = infection_history), fun = "mean", geom = "line", size = 0.5, alpha = 1) + 
        facet_grid(cols = vars(measured_variant), rows = vars(number_ag_exposure)) + 
        scale_linetype_manual(values = c("dashed", "solid")) +
        scale_y_continuous(breaks = 0:7, labels = labs_plot_y_gtr, limits = c(-0.3, 6)) + 
        theme_bw() +
        guides(linetype = guide_legend(override.aes = list(size = c(0.5, 0.5))), fill = "none") +
        theme(text = element_text(size = 15)) +
        scale_color_manual(values = colorsdefined) +
        scale_fill_manual(values = colorsdefined) +
        labs(x = "Time post exposure (days)", y = "Titre boost (GTR)", color = "Most recent exposure", linetype = "Infection history")



p3 <- post_fitted_all %>% 
    ggplot() + 
        geom_point(aes(x =  2^wane_s, y = 2^boost, color = recent_exposure), 
            size = 0.05, alpha = 0.05) +
        geom_point(data = post_fitted_mean, aes(x =  2^wane_s, y = 2^boost, fill = recent_exposure,
            shape = infection_history), 
            color = "black", size = 3, alpha = 1) +
        facet_grid(cols = vars(measured_variant), rows = vars(number_ag_exposure)) + 
        scale_shape_manual(values = c("Infection naive" = 21, "Previously infected" = 22)) +
        guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21)))) +
        scale_y_continuous(trans = "log2", limits = c(1, 32), breaks = 2 ^ (0:7), labels = labs_plot_y_gtr) + scale_x_continuous(trans = "log2", breaks = 2 ^ seq(-7, 1, 0.5), labels = round(2 ^ seq(-7, 1, 0.5), 2 ), limits = c(0.3, 1) ) +
        guides(color = "none") +
        theme_bw() + labs(x = "Titre wane 100 days after peak (GTR)", y = "Titre boost (GTR)", fill = "Most recent exposure", shape = "Infection history") + 
        scale_fill_manual(values = colorsdefined) +
        scale_color_manual(values = colorsdefined) +
        theme(text = element_text(size = 15))


require(patchwork)
p1 / p2 / p3  + plot_layout(heights = c(1, 2, 2)) + plot_annotation(tag_levels = "A") 
ggsave(here::here("outputs", "figs", "final_figs", "figS1_uncert.png"), height = 16, width = 12)

