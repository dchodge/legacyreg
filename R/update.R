update_labels_panel_b <- function(dt_in) {
  
  dt_out <- copy(dt_in)
  
  dt_out[titre_type == "Wildtype", titre_type := "Ancestral"]
  dt_out[titre_type == "BA.1", titre_type := "Omicron BA.1"]
  dt_out[titre_type == "BA.2", titre_type := "Omicron BA.2"]
  dt_out[titre_type == "BA.5", titre_type := "Omicron BA.5"]
  
  dt_out[event_type == "Vaccination", event_type := "Third vaccine dose"]
  
  dt_out[exposure_type == "Naive", exposure_type := "Infection naive"]
  dt_out[exposure_type == "Exposed", exposure_type := "Previously infected"]
  
  dt_out[, event_type := fct_relevel(event_type, "Third vaccine dose")]
  
  return(dt_out)
  
} 

update_labels_panel_c <- function(dt_in) {
  
  dt_out <- copy(dt_in)
  
  dt_out[titre_type == "Wildtype", titre_type := "Ancestral"]
  dt_out[titre_type == "BA.1", titre_type := "Omicron BA.1"]
  dt_out[titre_type == "BA.2", titre_type := "Omicron BA.2"]
  dt_out[titre_type == "BA.5", titre_type := "Omicron BA.5"]
  
  dt_out[(event_type == "Wildtype infection" |
            event_type == "BA.1 infection" |
            event_type == "BA.2 infection") & 
           exposure_type == "Naive",
         `Number of exposures` := "3 previous \n antigen exposures"]
  
  dt_out[(event_type == "Wildtype infection" |
            event_type == "BA.1 infection" |
            event_type == "BA.2 infection") & 
           exposure_type == "Exposed",
         `Number of exposures` := "4 previous \n antigen exposures"]
  
  dt_out[event_type == "Vaccination" & exposure_type == "Naive",
         `Number of exposures` := "2 previous \n antigen exposures"]
  
  dt_out[event_type == "Vaccination" & exposure_type == "Exposed",
         `Number of exposures` := "3 previous \n antigen exposures"]
  
  dt_out[exposure_type == "Naive", exposure_type := "Infection naive"]
  dt_out[exposure_type == "Exposed", exposure_type := "Previously infected"]
  
  dt_out[event_type == "Vaccination", event_type := "Third vaccine dose"]
  
  # dt_gtr[, `Number of exposures` := factor(`Number of exposures`)]
  
  dt_out[, event_type := fct_relevel(event_type, "Third vaccine dose")]
  dt_out[, titre_type := fct_relevel(titre_type, "Wildtype")]
  dt_out[, `Number of exposures` := fct_relevel(`Number of exposures`,
                                                "2 previous \n antigen exposures")]
  
  return(dt_out)
}