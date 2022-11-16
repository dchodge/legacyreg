#--- age lookup table

load("data/Legacy_DataAnnotatedDateSeries_2022-10-14.Rdata")

dt_raw_all <- data.table(chrono.df)

dt_age_lookup <- dt_raw_all[, .(elig_study_id, age)][,
  elig_study_id := factor(elig_study_id)] %>% unique()

setnames(dt_age_lookup, "elig_study_id", "id")

save(dt_age_lookup, file = "data/age_lookup.RData")
