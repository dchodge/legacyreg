
require(bayesplot)
require(tidybayes)
require(posterior)


load(file = here::here("data", "df", "titre_all_naive.RData")) # titre_all_naive
load(file = here::here("data", "df", "titre_all_exposed.RData")) # titre_all_exposed

info2_levels <- c("Wild type", "Omicron BA1", "Omicron BA2", "Omicron BA5")
file_name_var <- c("wt", "ba1", "ba2", "ba5")
info3_levels <- c("Vac3", "BA1", "BA2", "BA5")
file_name_type <- c("vac3", "ba1", "ba2", "ba5")

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


get_fitted_lines_uncert <- function(exposure) {
    post_pred_fitted_list <- list(); k <- 1
    for (i in 1:4) {
        for (j in 1:4) {
            
                info3_levels_str <- paste0(info3_levels[j], " ", exposure)
                file_name_type_2 <- paste0(file_name_type[j], "_", exposure)

                if (exposure == "naive") {
                    titre_all_trim <- titre_all_naive %>% filter(info2 == info2_levels[i], type == info3_levels_str) %>% filter(!is.na(info3))
                } else {
                    titre_all_trim <- titre_all_exposed %>% filter(info2 == info2_levels[i], type == info3_levels_str) %>% filter(!is.na(info3))
                }
                temp_stanfit <- readRDS(here::here("outputs", "stanfits", paste0("fit_", file_name_var[i], "_", file_name_type_2, ".Rdata")))
                post_fixed_eff <- temp_stanfit %>% as_draws(variables = c("boost_i", "boost_s", "wane_s", "t_p")) %>% as_draws_df
                sample_vals <- post_fixed_eff %>% .[1:1000, ]

                for (a in 1:nrow(sample_vals)) {
                    if (length(titre_all_trim$time_until_bleed) > 0) {
                        fitted_val_temp <- titre_boost_wane(sample_vals[a, ], 0:max(titre_all_trim$time_until_bleed))
                        post_pred_fitted_list[[k]] <- data.frame(
                            sample_no = a,
                            info2 = info2_levels[i],
                            type = info3_levels[j],
                            t = 0:max(titre_all_trim$time_until_bleed),
                            fitted_val = fitted_val_temp
                        )
                    } else {
                        post_pred_fitted_list[[k]] <- data.frame(
                            sample_no = a,
                            info2 = info2_levels[i],
                            type = info3_levels[j],
                            t = 0,
                            fitted_val = NA
                        )
                    }
                    k <- k + 1
                }
        }
    }
    post_pred_fitted_list %>% bind_rows %>% bind_rows %>% mutate(info2 = factor(info2, levels = info2_levels), type = factor(type, levels = info3_levels))
}

post_pred_fitted_naive_uncert <- get_fitted_lines_uncert("naive")
post_pred_fitted_exposed_uncert <- get_fitted_lines_uncert("exposed")
save(post_pred_fitted_naive_uncert, file = here::here("data", "df", "fitted_lines_naive_uncert.RData"))
save(post_pred_fitted_exposed_uncert, file = here::here("data", "df", "fitted_lines_exposed_uncert.RData"))



get_fitted_lines <- function(exposure) {
    post_pred_fitted_list <- list(); k <- 1
    for (i in 1:4) {
        for (j in 1:4) {
            
                info3_levels_str <- paste0(info3_levels[j], " ", exposure)
                file_name_type_2 <- paste0(file_name_type[j], "_", exposure)

                if (exposure == "naive") {
                    titre_all_trim <- titre_all_naive %>% filter(info2 == info2_levels[i], type == info3_levels_str) %>% filter(!is.na(info3))
                } else {
                    titre_all_trim <- titre_all_exposed %>% filter(info2 == info2_levels[i], type == info3_levels_str) %>% filter(!is.na(info3))
                }
                temp_stanfit <- readRDS(here::here("outputs", "stanfits", paste0("fit_", file_name_var[i], "_", file_name_type_2, ".Rdata")))
                post_fixed_eff <- temp_stanfit %>% as_draws(variables = c("boost_i", "boost_s", "wane_s", "t_p")) %>% as_draws_df
                mean_vals <- post_fixed_eff %>% apply(2, mean)

                if (length(titre_all_trim$time_until_bleed) > 0) {
                    fitted_val_temp <- titre_boost_wane(mean_vals, 0:max(titre_all_trim$time_until_bleed))
                    post_pred_fitted_list[[k]] <- data.frame(
                        info2 = info2_levels[i],
                        type = info3_levels[j],
                        t = 0:max(titre_all_trim$time_until_bleed),
                        fitted_val = fitted_val_temp
                    )
                } else {
                    post_pred_fitted_list[[k]] <- data.frame(
                        info2 = info2_levels[i],
                        type = info3_levels[j],
                        t = 0,
                        fitted_val = NA
                    )
                }
                k <- k + 1
        }
    }
    post_pred_fitted_list %>% bind_rows %>% bind_rows %>% mutate(info2 = factor(info2, levels = info2_levels), type = factor(type, levels = info3_levels))
}

post_pred_fitted_naive <- get_fitted_lines("naive")
post_pred_fitted_exposed <- get_fitted_lines("exposed")
save(post_pred_fitted_naive, file = here::here("data", "df", "fitted_lines_naive.RData"))
save(post_pred_fitted_exposed, file = here::here("data", "df", "fitted_lines_exposed.RData"))

require(rstan)

# Make a dataframe of fit criteria

get_fitted_diagnostics <- function(exposure) {
    diag_fitted_list <- list(); k <- 1
    for (i in 1:4) {
        for (j in 1:4) {
            
                info3_levels_str <- paste0(info3_levels[j], " ", exposure)
                file_name_type_2 <- paste0(file_name_type[j], "_", exposure)

                if (exposure == "naive") {
                    titre_all_trim <- titre_all_naive %>% filter(info2 == info2_levels[i], type == info3_levels_str) %>% filter(!is.na(info3))
                } else {
                    titre_all_trim <- titre_all_exposed %>% filter(info2 == info2_levels[i], type == info3_levels_str) %>% filter(!is.na(info3))
                }
                temp_stanfit <- readRDS(here::here("outputs", "stanfits", paste0("fit_", file_name_var[i], "_", file_name_type_2, ".Rdata")))
                fit_diagnostics <- temp_stanfit$diagnostic_summary()

                row_entries <- c( paste0("fit_", file_name_var[i], "_", file_name_type_2), 
                    temp_stanfit$diagnostic_summary()$num_divergent,
                    temp_stanfit$diagnostic_summary()$num_max_treedepth,
                    temp_stanfit$diagnostic_summary()$ebfmi
                )
                names(row_entries) <- c("model",
                    paste("num_divergent", 1:4, sep = "_"),
                    paste("num_max_treedepth", 1:4, sep = "_"),
                    paste("ebfmi", 1:4, sep = "_")
                    )

                diag_fitted_list[[k]] <- as.data.frame(row_entries) %>% t %>% as.data.frame
                k <- k + 1
        }
    }
    diag_fitted <- diag_fitted_list %>% bind_rows
    row.names(diag_fitted) <- 1:16
    diag_fitted
}

df_fitted_naive <- get_fitted_diagnostics("naive")
df_fitted_exposed <- get_fitted_diagnostics("exposed")

save(df_fitted_naive, file = here::here("outputs", "stanfits", "diagnostic_naive.RData"))
save(df_fitted_exposed, file = here::here("outputs", "stanfits", "diagnostic_exposed.RData"))
