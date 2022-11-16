#-- summarising raw data for Table 1
source("scrips/setup.R")
load("data/age_lookup.RData")

dt_raw <- extract_all_raw_data()

# merging id with age
dt_raw_w_age <- merge(dt_raw, dt_age_lookup, by = "id")[,
  .(id, age, event_type, exposure_type)] %>% unique()

# updating labels for plot readabililty
dt_raw_w_age <- update_exposure_event_labels(dt_raw_w_age)

# adding custom age groups to avoid reidentification
# dt_raw_w_age[age >= 20 & age < 30, age_group := "20-29"]
# dt_raw_w_age[age >= 30 & age < 40, age_group := "30-39"]
# dt_raw_w_age[age >= 40 & age < 50, age_group := "40-49"]
# dt_raw_w_age[age >= 50, age_group := "50+"]

# dt_raw_w_age[, age_group := factor(age_group)][, 
#   age_group := fct_relevel(age_group, c("20-29", 
#                                         "30-39",
#                                         "40-49",
#                                         "50+"))]
# 
# dt_raw_w_age_sum <- dt_raw_w_age[, .(count = uniqueN(stan_id)), 
#   by = c("event_type", "exposure_type", "age_group")]

dt_raw_w_age_sum <- dt_raw_w_age[, .(count = uniqueN(id)),
             by = c("event_type", "exposure_type")]

dt_raw_w_age_sum[, .(percentage = signif(count/463*100, 2)),
                 by = c("event_type", "exposure_type")]

dt_raw_w_age[, .(age_me = quantile(age, 0.5),
                 age_lo = quantile(age, 0.025),
                 age_hi = quantile(age, 0.975)),
             by = c("exposure_type")]
