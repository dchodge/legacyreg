library(data.table)
library(lemon)
library(ggplot2)

setDTthreads(threads = 8)

load("../legacyreg/data/Legacy_DataAnnotatedDateSeries_2022-09-09.Rdata")

dt_raw <- data.table(chrono.df)
dt_raw <- dt_raw[, elig_study_id := as.numeric(elig_study_id)]

setnames(dt_raw, 
         c("calendar_date", "elig_study_id", "episode_number",
           "episode_variant_summarised", "Roche_N_result",
           "ic50_wildtype", "ic50_Alpha", "ic50_Delta",
           "ic50_Omicron_BA1", "ic50_Omicron_BA2", "ic50_Omicron_BA5"),
         c("date", "id", "infection_id",
           "voc", "anti_n",
           "wildtype", "alpha", "delta",
           "omicron_ba1", "omicron_ba2", "omicron_ba5"))

# vaccine data.table
cols_to_keep_vaccines <- c("date", "id",
                           "dose_1", "dose_2",
                           "dose_3", "dose_4",
                           "date_dose_1", "date_dose_2",
                           "date_dose_3", "date_dose_4")

dt_vax_long_1 <- melt(dt_raw[, ..cols_to_keep_vaccines],
     measure.vars = c("dose_1", "dose_2", "dose_3", "dose_4"),
     variable.name = "dose",
     value.name = "dose_type"
)

dt_vax_long_2 <- melt(dt_vax_long_1,
                      measure.vars = c("date_dose_1", "date_dose_2",
                                       "date_dose_3", "date_dose_4"),
                      variable.name = "dose_tmp",
                      value.name = "dose_date"                 
)

dt_vax_long_2[order(id, dose)]

dt_vax <- setcolorder(dt_vax_long_2, "dose_date")[,
  `:=` (date = NULL, dose_tmp = NULL)
  ][
    !is.na(dose_date)][order(id, dose)
  ] %>%
  unique()

# anti-N data.table

# titre data.table
cols_to_keep_titres <- c("date", "id", "age", "infection_id",
                         "voc", "episode_start",
                         "wildtype", "alpha", "delta",
                         "omicron_ba1", "omicron_ba2", "omicron_ba5")


vocs <- c("wildtype", "alpha", "delta",
          "omicron_ba1", "omicron_ba2", "omicron_ba5")

dt_titres <- dt_raw[, ..cols_to_keep_titres]

dt_titres_long <- melt(dt_titres, 
                measure.vars = vocs,
                variable.name = "titre_type",
                value.name = "titre"
)[order(id, date, titre_type)]

dt_titres_trim <- dt_titres_long[!is.na(titre)]
        
dt_titres_trim[titre < 4000] %>% 
  ggplot() + 
  geom_point(aes(x = date, y = titre, colour = titre_type), alpha = 0.05) +
  geom_smooth(aes(x = date, y = titre, colour = titre_type)) +
  # geom_line(aes(x = date, y = titre, colour = titre_type))
  facet_wrap(~titre_type) +
  theme_minimal()


# trying filtering on original format
# titre data.table
cols_to_keep_raw <- c("date", "id", "age", "infection_id",
                      "voc", "episode_start",
                      "dose_1", "dose_2",
                      "dose_3", "dose_4",
                      "date_dose_1", "date_dose_2",
                      "date_dose_3", "date_dose_4",
                      "wildtype", "alpha", "delta",
                      "omicron_ba1", "omicron_ba2", "omicron_ba5",
                      "anti_n")


dt_trim <- dt_raw[, ..cols_to_keep_raw][order(id, date)]

dt_long <- melt(dt_trim, 
                measure.vars = vocs,
                variable.name = "titre_type",
                value.name = "titre"
)[order(id, date, titre_type)][!is.na(titre)]

# adding number of bleeds
dt_long[, bleed_number := .N, by = c("id", "titre_type")]

dt_tmp <- dt_long[id %in% 1:20 & bleed_number > 3][anti_n == "positive"][, .SD[date == min(date)], by = id]

dt_long[id %in% 1:20 & bleed_number > 3] %>% 
  ggplot() + 
  # geom_point(aes(x = date, y = titre, colour = titre_type), alpha = 0.05) +
  # geom_smooth(aes(x = date, y = titre, colour = titre_type)) +
  geom_line(aes(x = date, y = titre, colour = titre_type)) + 
  geom_vline(aes(xintercept = date_dose_1), linetype = "dashed") + 
  geom_vline(aes(xintercept = date_dose_2), linetype = "dashed") + 
  geom_vline(aes(xintercept = date_dose_3), linetype = "dashed") + 
  geom_vline(aes(xintercept = date_dose_4), linetype = "dashed") + 
  geom_vline(data = dt_tmp,
    aes(xintercept = date), colour = "red", linetype = "dashed") + 
  facet_rep_wrap(~id) +
  theme_minimal()



