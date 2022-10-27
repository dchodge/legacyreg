#--- individual-level posterior predictive plots
titre_type_options <- c("Wildtype", "BA.1", "BA.2", "BA.5")
event_type_options <- c("Vaccination", "BA.1 infection",
                        "BA.2 infection", "BA.5 infection")
exposure_type_options <- c("Naive", "Exposed")


dt_all_post_pred <- extract_all_ind_posterior_preds()
dt_all_raw_data <- extract_all_raw_data()

# save(dt_all_post_pred,
#      file = "outputs/posterior_predictives/all_ind_posterior_pred_summaries.Rdata")

id_lookup <- dt_all_raw_data[, .(id = unique(id)),
                             by = c("titre_type", 
                                    "event_type", 
                                    "exposure_type", 
                                    "stan_id")]

dt_all_post_pred <- merge(dt_all_post_pred,
                          id_lookup, by = c("titre_type", 
                                            "event_type",
                                            "exposure_type",
                                            "stan_id"),
                          all.x = TRUE)



p_all_plots_vax <- plot_ind_posterior_pred(dt_all_post_pred,
                                           dt_all_raw_data,
                                           event_type_arg = "Vaccination",
                                           include_data = TRUE)

ggsave("outputs/figs_tim/all_vaccine_post_preds.pdf",
       p_all_plots_vax,
       width = 10,
       height = 16)

p_all_plots_ba1 <- plot_ind_posterior_pred(dt_all_post_pred,
                                           dt_all_raw_data,
                                           event_type_arg = "BA.1 infection",
                                           include_data = TRUE)

ggsave("outputs/figs_tim/all_ba1_post_preds.pdf",
       p_all_plots_ba1,
       width = 10,
       height = 12)

p_all_plots_ba2 <- plot_ind_posterior_pred(dt_all_post_pred,
                                           dt_all_raw_data,
                                           event_type_arg = "BA.2 infection",
                                           include_data = TRUE)

ggsave("outputs/figs_tim/all_ba2_post_preds.pdf",
       p_all_plots_ba2,
       width = 10,
       height = 12)

# fits are empty for these datasets. I know we didn't have enough BA.5
# infections for reasonable fits, ask whether they were deliberately left out 
# of the data prep for model fitting
#
# p_all_plots_ba5 <- plot_ind_posterior_pred(dt_all_post_pred,
#                                            dt_all_raw_data,
#                                            event_type_arg = "BA.5 infection",
#                                            include_data = TRUE)
# 
# ggsave("outputs/figs_tim/all_ba5_post_preds.pdf",
#        p_all_plots_ba5,
#        width = 10,
#        height = 3)
