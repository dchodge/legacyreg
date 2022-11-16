#--- extracting data for the supplementary table
dt_supp_tables_data <- extract_panel_c_data()

dt_table_s2_data_sum <- dt_supp_tables_data[, .(titre_at_peak_nat_me = titre_at_peak_nat_me, 
                                                titre_at_peak_nat_lo = titre_at_peak_nat_lo,
                                                titre_at_peak_nat_hi = titre_at_peak_nat_hi),
                                            by = c("titre_type", 
                                                   "event_type", 
                                                   "exposure_type")]


dt_table_s3_data_sum <- dt_supp_tables_data[, .(gtr_me = gtr_me, 
                                                gtr_lo = gtr_lo,
                                                gtr_hi = gtr_hi),
                                            by = c("titre_type", 
                                                   "event_type", 
                                                   "exposure_type")]

dt_table_s2_data_sum %>% 
  unique() %>% 
  .[order(event_type, exposure_type, titre_type)]

dt_table_s3_data_sum %>% 
  unique() %>% 
  .[order(event_type, exposure_type, titre_type)]

dt_supp_tables_data$titre_at_peak_nat_me
