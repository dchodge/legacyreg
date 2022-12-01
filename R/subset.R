# put together the data for Stan using indices to determine titre type and event type
# and a "naive" or "exposed" flag to determine whether the individuals were infected previously or not
subset_data <- function(i, j,
                        exposure,
                        adjust_times) {
  
  info3_levels_str <- paste0(info3_levels[j], " ", exposure)
  file_name_type_2 <- paste0(file_name_type[j], "_", exposure)
  
  if (exposure == "naive") {
    titre_all_trim <- titre_all_naive %>% 
      filter(info2 == info2_levels[i],
             type == info3_levels_str) %>% 
      filter(!is.na(info3))
  } else {
    titre_all_trim <- titre_all_exposed %>%
      filter(info2 == info2_levels[i],
             type == info3_levels_str) %>% 
      filter(!is.na(info3))
  }
  
  titre_all_trim_ids <- titre_all_trim$elig_study_id %>% unique
  
  titre_all_trim <- titre_all_trim %>%
    mutate(elig_study_id = factor(elig_study_id,
                                  levels = titre_all_trim_ids)) %>%
    # mutate(elig_study_id = as.numeric(elig_study_id)) %>% 
    filter(!is.na(info3)) %>% 
    data.table()
  
  out <- titre_all_trim[, stan_id := .GRP, by = elig_study_id]
  
  if(adjust_times == TRUE) {
    out <- titre_all_trim[, calendar_date := calendar_date + 5]
  }
  
  setnames(out, "elig_study_id", "id")
  
  return(out)
  
}
