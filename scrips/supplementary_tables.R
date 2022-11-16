#--- supplementary table 1 - inferred peak titre values
dt_panel_c_data[, .(titre_at_peak_nat_me,
                    titre_at_peak_nat_lo,
                    titre_at_peak_nat_hi),
                by = c("event_type", "exposure_type", "titre_type")]

#--- supplementary table 2 - inferred Geometric titre ratio's
dt_panel_c_data[, .(gtr_me,
                    gtr_lo,
                    gtr_hi),
                by = c("event_type", "exposure_type", "titre_type")]
