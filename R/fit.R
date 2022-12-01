fit_model_all_subsets <- function() {
  # loop fitting model to each subset of data and saving the fit
  for(i_arg in 1:4) {
    for(j_arg in 1:4) {
      for(k in 1:2) {
        
        exposure_arg = exposure_loop[k]
        file_name_type_2 <- paste0(file_name_type[j_arg], "_", exposure_arg)
        
        if(j_arg != 1) {
          adjust_times <- TRUE
        } else if (j_arg == 1) {
          adjust_times <- FALSE
        }
        
        fit_current <- subset_and_fit(i = i_arg,
                                      j = j_arg,
                                      exposure = exposure_arg,
                                      initialise_from_priors = FALSE,
                                      adjust_times = TRUE)

        fit_current$save_object(file = paste0("outputs/stan_fits_tim/fit_",
                                              file_name_var[i_arg], "_",
                                              file_name_type_2, ".Rdata"))
      }
    }
  }
}
