library(data.table)
library(lemon)
library(ggplot2)

setDTthreads(threads = 8)

load("data/Legacy_DataAnnotatedDateSeries_2022-09-09.Rdata")

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

vocs <- c("wildtype", "alpha", "delta",
          "omicron_ba1", "omicron_ba2", "omicron_ba5")

# vaccine data.table
# cols_to_keep_vaccines <- c("date", "id",
#                            "dose_1", "dose_2",
#                            "dose_3", "dose_4",
#                            "date_dose_1", "date_dose_2",
#                            "date_dose_3", "date_dose_4")
# 
# dt_vax_long_1 <- melt(dt_raw[, ..cols_to_keep_vaccines],
#                       measure.vars = c("dose_1", "dose_2", "dose_3", "dose_4"),
#                       variable.name = "dose",
#                       value.name = "dose_type"
# )
# 
# dt_vax_long_2 <- melt(dt_vax_long_1,
#                       measure.vars = c("date_dose_1", "date_dose_2",
#                                        "date_dose_3", "date_dose_4"),
#                       variable.name = "dose_tmp",
#                       value.name = "dose_date"                 
# )
# 
# dt_vax_long_2[order(id, dose)]
# 
# dt_vax <- setcolorder(dt_vax_long_2, "dose_date")[,
#                                                   `:=` (date = NULL, dose_tmp = NULL)
# ][
#   !is.na(dose_date)][order(id, dose)
#   ] %>%
#   unique()
# 
# # anti-N data.table
# 
# # titre data.table
# cols_to_keep_titres <- c("date", "id", "age", "infection_id",
#                          "voc", "episode_start",
#                          "wildtype", "alpha", "delta",
#                          "omicron_ba1", "omicron_ba2", "omicron_ba5")
# 
# 
# 
# dt_titres <- dt_raw[, ..cols_to_keep_titres]
# 
# dt_titres_long <- melt(dt_titres, 
#                        measure.vars = vocs,
#                        variable.name = "titre_type",
#                        value.name = "titre"
# )[order(id, date, titre_type)]
# 
# dt_titres_trim <- dt_titres_long[!is.na(titre)]
# 
# dt_titres_trim[titre < 4000] %>% 
#   ggplot() + 
#   geom_point(aes(x = date, y = titre, colour = titre_type), alpha = 0.05) +
#   geom_smooth(aes(x = date, y = titre, colour = titre_type)) +
#   # geom_line(aes(x = date, y = titre, colour = titre_type))
#   facet_wrap(~titre_type) +
#   theme_minimal()


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
)[order(id, date, titre_type)][!is.na(titre)] %>% unique()

# adding number of bleeds
dt_long[, bleed_number := .N, by = c("id", "titre_type")]

dt_long[, max(bleed_number)]

# adding time since dose 3
dt_long[, time_since_dose_3 := as.numeric(date - date_dose_3, units = "days"),
        by = "id"]

tmp1 <- dt_long[time_since_dose_3 > 0, .SD[time_since_dose_3 == min(time_since_dose_3)], 
        by = c("id")]

tmp2 <- dt_long[time_since_dose_3 < 0, .SD[time_since_dose_3 == max(time_since_dose_3)], 
                by = c("id")]

dt_dose_3 <- rbind(tmp1, tmp2)[order(id, date, titre_type)]


p_dose_3_ind <- dt_dose_3[id %in% 1:100] %>% 
  ggplot(aes(x = time_since_dose_3, y = titre, colour = titre_type, group = id)) + 
  geom_point() + 
  geom_line() + 
  # geom_smooth(inherit.aes = FALSE, aes(x = time_since_dose_3, y = titre, colour = titre_type)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  facet_rep_wrap(~id) +
  theme_minimal()

dt_dose_3[time_since_dose_3 > -100 & time_since_dose_3 < 100][, uniqueN(id)]

ggsave("../legacyreg/outputs/figs/dose_3_ind.pdf",
       p_dose_3_ind,
       # width = 10,
       # height = 30,
       bg = "white")
