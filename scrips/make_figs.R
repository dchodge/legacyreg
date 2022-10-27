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

    titre_all_naive_dose3 <- titre_all_naive %>% filter(type == !!type_str_fil_naive) # %>% filter(info2 != "Omicron BA5")
    titre_all_exposed_dose3 <- titre_all_exposed %>% filter(type == !!type_str_fil_exposed) #%>% filter(info2 != "Omicron BA5")

    labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")

    lower <- 5;
    upper <- 2 ^ (7) * 5;

    p1 <- titre_all_naive_dose3 %>% 
        ggplot() + 
        geom_line(aes(x = time_until_bleed, y = 2 ^ info3 * 5, color = info2, group = paste(elig_study_id, info2)),
            size = 0.3) + 
        geom_point(aes(x = time_until_bleed, y = 2 ^ (info3 ) * 5, fill = info2), shape = 21, alpha = 0.7) + 
        labs(x = "Days between vac dose and bleed", y = "Titre value", title = paste0(title_str, " and infection naive"), color = "Variant") +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y, limits = c(lower, upper) ) + theme_bw() + 
        scale_x_continuous(limits = c(0, NA)) +
        guides(color = "none", fill = "none") +
        facet_grid(cols = vars(info2))
    p2 <- titre_all_exposed_dose3 %>% #filter(!elig_study_id %in% ids_detect) %>% 
        ggplot() + 
        geom_line(aes(x = time_until_bleed, y = 2 ^ info3 * 5, color = info2, group = paste(elig_study_id, info2)),
            size = 0.3) + 
        geom_point(aes(x = time_until_bleed, y = 2 ^ info3 * 5, fill = info2), shape = 21, alpha = 0.7) + 

        labs(x = "Days between vac dose and bleed", y = "Titre value", title = paste0(title_str, " and previously infected"), color = "Variant") +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y , limits = c(lower, upper)) + theme_bw() + 
        guides(color = "none", fill = "none") +
        facet_grid(cols = vars(info2))
    require(patchwork)
    p1 + p2 + plot_layout(guides = "collect")
    ggsave(here::here("outputs", "figs", paste0("model_data_", type_str, ".png")), width = 10, height = 6)
}

plot_model_data_fitted <- function(titre_all_naive, titre_all_exposed, fitted_naive, fitted_exposed, type_str, title_str, color_val, subtitle1, subtitle2) {
    type_str_fil_naive <- paste0(type_str, " naive")
    type_str_fil_exposed <- paste0(type_str, " exposed")

    relabel_info2 <- c("Wild type" = "Ancestral")

    titre_all_naive_dose3 <- titre_all_naive %>% filter(type == !!type_str_fil_naive) %>% 
        mutate(info2 = recode(info2, !!!relabel_info2)) 
    titre_all_exposed_dose3 <- titre_all_exposed %>% filter(type == !!type_str_fil_exposed) %>% 
        mutate(info2 = recode(info2, !!!relabel_info2)) 

    fitted_naive_type  <- fitted_naive %>% filter(type == !!type_str) %>%
        mutate(info2 = recode(info2, !!!relabel_info2)) 
    fitted_exposed_type <- fitted_exposed %>% filter(type == !!type_str) %>%
        mutate(info2 = recode(info2, !!!relabel_info2)) 

    lower <- 10;
    upper <- 2 ^ (8.75) * 5;

    labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")

    p1 <- titre_all_naive_dose3 %>% 
        ggplot() + 
        geom_hline(yintercept = 5 * 2^7, linetype = "dashed", color = "gray30") + 
        geom_hline(yintercept = 5 * 2^1, linetype = "dashed", color = "gray30") + 
        geom_line(aes(x = time_until_bleed, y = 2 ^ info3 * 5, group = paste(elig_study_id, info2)),
            size = 0.3, color = color_val) + 
        geom_point(aes(x = time_until_bleed, y = 2 ^ (info3 ) * 5), shape = 21, alpha = 0.7, fill = color_val) + 
        geom_line(data = fitted_naive_type, aes(x = t, y = 2 ^ (fitted_val) * 5), color = "white", size = 2, alpha = 0.8) + 
        geom_line(data = fitted_naive_type, aes(x = t, y = 2 ^ (fitted_val) * 5), color = "black", size = 1, alpha = 0.8) + 
        labs(x = "Days between exposure and bleed", y = "Titre value", title = paste0(title_str, " and infection naive"), color = "Variant", 
            subtitle = subtitle1) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y, limits = c(lower, upper) ) + theme_bw() + 
        scale_x_continuous(limits = c(0, NA)) +
        guides(color = "none", fill = "none") +
        facet_grid(cols = vars(info2))
    p2 <- titre_all_exposed_dose3 %>% #filter(!elig_study_id %in% ids_detect) %>% 
        ggplot() + 
        geom_hline(yintercept = 5 * 2^7, linetype = "dashed", color = "gray30") + 
        geom_hline(yintercept = 5 * 2^1, linetype = "dashed", color = "gray30") + 
        geom_line(aes(x = time_until_bleed, y = 2 ^ info3 * 5, group = paste(elig_study_id, info2)),
            size = 0.3, color = color_val) + 
        geom_point(aes(x = time_until_bleed, y = 2 ^ info3 * 5, fill = info2), shape = 22, alpha = 0.7, fill = color_val) + 
       geom_line(data = fitted_exposed_type, aes(x = t, y = 2 ^ (fitted_val) * 5), color = "white", size = 2, alpha = 0.8) + 
        geom_line(data = fitted_exposed_type, aes(x = t, y = 2 ^ (fitted_val) * 5), color = "black", size = 1, alpha = 0.8) +         
        labs(x = "Days between exposure and bleed", y = "Titre value", title = paste0(title_str, " and previously infected"), color = "Variant", 
            subtitle = subtitle2) +
        scale_y_continuous(trans = "log2", breaks = 2 ^ (1:7) * 5, labels = labs_plot_y, limits = c(lower, upper) ) + theme_bw() + 
        guides(color = "none", fill = "none") +
        facet_grid(cols = vars(info2))
    require(patchwork)
    p1 + p2 + plot_layout(guides = "collect") + plot_annotation(title = title_str)
}

load(file = here::here("data", "df", "titre_all_naive.RData")) # titre_all_naive
load(file = here::here("data", "df", "titre_all_exposed.RData")) # titre_all_exposed

load(file = here::here("data", "df", "fitted_lines_naive.RData")) # post_pred_fitted_naive
load(file = here::here("data", "df", "fitted_lines_exposed.RData")) # post_pred_fitted_exposed

plot_model_data(titre_all_naive, titre_all_exposed, "Vac3", "Third vaccine dose")
plot_model_data(titre_all_naive, titre_all_exposed, "BA1", "Omicron BA1 infection")
plot_model_data(titre_all_naive, titre_all_exposed, "BA2", "Omicron BA2 infection")

p1 <- plot_model_data_fitted(titre_all_naive, titre_all_exposed, post_pred_fitted_naive, post_pred_fitted_exposed, "Vac3", "Third vaccine dose", "#AA4A44",
    subtitle1 = "Third antigenic exposure\nExposure history: two vaccine doses",
    subtitle2 = "Fourth antigenic exposure\nExposure History: two vaccines doses and one infection"
)
p2 <- plot_model_data_fitted(titre_all_naive, titre_all_exposed, post_pred_fitted_naive, post_pred_fitted_exposed, "BA1", "Omicron BA1 infection", "#0d98ba",    
    subtitle1 = "Fourth antigenic exposure\nExposure history: three vaccine doses",
    subtitle2 = "Fifth antigenic exposure\nExposure history: three vaccines doses and one infection"
)
p3 <- plot_model_data_fitted(titre_all_naive, titre_all_exposed, post_pred_fitted_naive, post_pred_fitted_exposed, "BA2", "Omicron BA2 infection", "#1c39bb",    
    subtitle1 = "Fourth antigenic exposure\nExposure history: three vaccine doses",
    subtitle2 = "Fifth antigenic exposure\nExposure history: three vaccines doses and one infection"
)

p1 / p2 / p3 +  plot_annotation(tag_levels = "A")

ggsave(here::here("outputs", "figs", "final_figs", "fitS2.png"), width = 10, height = 12)


# Including uncertainty
#post_fitted %>% 
 #   ggplot() + 
  #      geom_density_2d(aes(x =  2^wane_s, y = 5 * 2^boost_abs, color = exposure_type), level = c(0.5, 0.95)) + 
  #      facet_grid(cols = vars(info2), rows = vars(type)) + 
  #      geom_segment(data = post_fitted_mean,
  #          aes(x =  2^wane_s, y = 5 * 2^boost_abs_cen, xend =  2^wane_s, yend = 5 * 2^(boost_abs)),  
   #         size = 0.1, alpha = 0.8, 
   #         color = "gray") +
   #     scale_color_manual(values = uniFill) +
   #     scale_fill_manual(values = uniFill) +
   #     scale_shape_manual(values = uniShape) +
   #     scale_y_continuous(trans = "log2", breaks = 5 * 2 ^ (-7:7), limits = c(2, 640)) +
   #     scale_x_continuous(trans = "log2", breaks = 2 ^ seq(-7, 1, 0.5), labels = round(2 ^ seq(-7, 1, 0.5), 2 ) ) +
   #     guides(color = "none", shape = "none") +
   #     theme_bw() + labs(x = "Titre wane 100 days after peak (GTR)", y = "Titre value at peak", fill = "Exposure history (variant)")

