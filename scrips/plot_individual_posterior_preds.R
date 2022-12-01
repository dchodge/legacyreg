#--- individual-level posterior predictive plots
titre_type_options <- c("Wildtype", "BA.1", "BA.2", "BA.5")
event_type_options <- c("Vaccination", "BA.1 infection",
                        "BA.2 infection", "BA.5 infection")
exposure_type_options <- c("Naive", "Exposed")


dt_all_post_pred_no_id <- extract_all_ind_posterior_preds()
dt_all_raw_data <- extract_all_raw_data()

# save(dt_all_post_pred,
#      file = "outputs/posterior_predictives/all_ind_posterior_pred_summaries.Rdata")

id_lookup <- dt_all_raw_data[, .(id = unique(id)),
                             by = c("titre_type", 
                                    "event_type", 
                                    "exposure_type", 
                                    "stan_id")]

dt_all_post_pred <- merge(dt_all_post_pred_no_id,
                          id_lookup, by = c("titre_type", 
                                            "event_type",
                                            "exposure_type",
                                            "stan_id"),
                          all.x = TRUE)
#--- event type: vaccine
#--- exposure type: infection naive
p_ind_plots_vax_naive <- plot_ind_posterior_pred(dt_all_post_pred,
                                                 dt_all_raw_data,
                                                 event_type_arg = "Vaccination",
                                                 exposure_type_arg = "Naive",
                                                 include_data = TRUE)

ggsave("outputs/figs_tim/supplementary_figures/individual_level_post_preds/all_vaccine_naive_post_preds.pdf",
       p_ind_plots_vax_naive,
       width = 11,
       height = 28)

#--- event type: vaccine
#--- exposure type: previously exposed
p_ind_plots_vax_exposed <- plot_ind_posterior_pred(dt_all_post_pred,
                                                   dt_all_raw_data,
                                                   event_type_arg = "Vaccination",
                                                   exposure_type_arg = "Exposed",
                                                   include_data = TRUE)

ggsave("outputs/figs_tim/supplementary_figures/individual_level_post_preds/all_vaccine_exposed_post_preds.pdf",
       p_ind_plots_vax_exposed,
       width = 11,
       height = 8)

#--- event type: BA.1 infection
#--- exposure type: infection naive
p_ind_plots_ba1_naive <- plot_ind_posterior_pred(dt_all_post_pred,
                                                 dt_all_raw_data,
                                                 event_type_arg = "BA.1 infection",
                                                 exposure_type_arg = "Naive",
                                                 include_data = TRUE)

ggsave("outputs/figs_tim/supplementary_figures/individual_level_post_preds/all_ba1_naive_post_preds.pdf",
       p_ind_plots_ba1_naive,
       width = 11,
       height = 20)

#--- event type: BA.1 infection
#--- exposure type: previously exposed
p_ind_plots_ba1_exposed <- plot_ind_posterior_pred(dt_all_post_pred,
                                                   dt_all_raw_data,
                                                   event_type_arg = "BA.1 infection",
                                                   exposure_type_arg = "Exposed",
                                                   include_data = TRUE)

ggsave("outputs/figs_tim/supplementary_figures/individual_level_post_preds/all_ba1_exposed_post_preds.pdf",
       p_ind_plots_ba1_exposed,
       width = 10,
       height = 5)

#--- event type: BA.2 infection
#--- exposure type: infection naive
p_ind_plots_ba2_naive <- plot_ind_posterior_pred(dt_all_post_pred,
                                           dt_all_raw_data,
                                           event_type_arg = "BA.2 infection",
                                           exposure_type_arg = "Naive",
                                           include_data = TRUE)

ggsave("outputs/figs_tim/supplementary_figures/individual_level_post_preds/all_ba2_naive_post_preds.pdf",
       p_ind_plots_ba2_naive,
       width = 10,
       height = 20)

#--- event type: BA.2 infection
#--- exposure type: previously exposed
p_ind_plots_ba2_exposed <- plot_ind_posterior_pred(dt_all_post_pred,
                                                   dt_all_raw_data,
                                                   event_type_arg = "BA.2 infection",
                                                   exposure_type_arg = "Exposed",
                                                   include_data = TRUE)

ggsave("outputs/figs_tim/supplementary_figures/individual_level_post_preds/all_ba2_exposed_post_preds.pdf",
       p_ind_plots_ba2_exposed,
       width = 10,
       height = 5)
