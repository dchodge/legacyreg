get_df_legacy_raw_annot <- load(here::here("data", "Legacy_DataAnnotatedDateSeries_2022-09-09.RData"))
raw_legacy_full <- chrono.df
legacy_ids_all <- raw_legacy_full %>% pull(elig_study_id) %>% unique

df_age <- raw_legacy_full %>% select(elig_study_id, age) %>% unique

load(file = here::here("data", "df", "titre_all_naive.RData")) # titre_all_naive
load(file = here::here("data", "df", "titre_all_exposed.RData")) # titre_all_exposed

df_summary <- bind_rows(
    titre_all_naive, 
    titre_all_exposed
) %>% left_join(df_age) %>% filter(!type %in% c("BA5 naive", "BA5 exposed"))


df_summary %>% select(elig_study_id, type) %>% unique %>% group_by(type) %>% summarise(n = n()),