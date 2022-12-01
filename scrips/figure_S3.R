#--- making age distribution supplementary figure
source("scrips/setup.R")

load("data/age_lookup.RData")

# extracting raw data
dt_raw <- extract_all_raw_data()

# merging id with age
dt_raw_w_age <- merge(dt_raw, age_lookup, by = "id")[,
  .(id, age, event_type, exposure_type, stan_id)] %>% unique()

# updating labels for plot readabililty
dt_raw_w_age <- update_exposure_event_labels(dt_raw_w_age)

# adding custom age groups to avoid reidentification
dt_raw_w_age[age >= 20 & age < 30, age_group := "20-29"]
dt_raw_w_age[age >= 30 & age < 40, age_group := "30-39"]
dt_raw_w_age[age >= 40 & age < 50, age_group := "40-49"]
dt_raw_w_age[age >= 50, age_group := "50+"]

dt_raw_w_age[, age_group := factor(age_group)][, 
  age_group := fct_relevel(age_group, c("20-29", 
                                        "30-39",
                                        "40-49",
                                        "50+"))]

dt_raw_w_age_sum <- dt_raw_w_age[, .(count = uniqueN(stan_id)), 
             by = c("event_type", "exposure_type", "age_group")]

p_age <- dt_raw_w_age_sum %>%  
  ggplot() + 
  geom_col(aes(x = age_group,
               y = count, 
               fill = interaction(event_type, exposure_type))) +
  facet_grid(event_type ~ exposure_type, scales = "fixed") + 
  theme_light() +
  theme(legend.position = "none") +
  labs(x = "Age group", y = "Count")

ggsave("outputs/figs_tim/figure_supp_age.pdf",
       p_age,
       width = 6,
       height = 8,
       bg = "white")

ggsave("outputs/figs_tim/figure_supp_age.png",
       p_age,
       width = 6,
       height = 8,
       bg = "white")
