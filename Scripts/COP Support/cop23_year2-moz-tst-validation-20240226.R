# PROJECT:  moz-cop22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Review MOZ DP
# LICENSE:  MIT
# DATE:     2024-02-26
# UPDATED:  


#DEPENDENCIES ----------------------------------------------------------------------

library(tameDP)
library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(glue)
library(scales)


#Directory ---------------------------------------------------------------------------

#path <- "C:/Users/ksrikanth/Documents/Datapack/Mozambique_datapack_Finalized_20220415.xlsx"

folder <- "Data/Datapack"

path <-folder %>% 
  return_latest("Target Setting Tool_Mozambique_022224_Final_v.03_0350pm")

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}


# IMPORT  -------------------------------------------------------------------

#import all tabs
dp <- tame_dp(path)

#grab PLHIV tab for validations
dp_plhiv <- tame_dp(path, type = 'PLHIV')

#READ IN MSD
df_msd <- si_path() %>%
  return_latest("MER_Structured_Datasets_PSNU_IM_FY22-24_20240215_v1_1_Mozambique") %>%
  read_psd()


# FILTER DP -------------------------------------------------------------

# filter DP, join PSNU map and mutate FY; filter to only 2024 targets
dp_filtered <- dp %>%
  clean_indicator() %>% 
  # filter(fiscal_year == 2024) %>% 
  select(-c(country)) %>% 
  mutate(fiscal_year = as.character(fiscal_year)) %>%
  mutate(fiscal_year = str_replace(fiscal_year, "20", "FY"))

# filter PLHIV tab of dp and do the same munging
dp_plhiv_filtered <- dp_plhiv %>%
  clean_indicator() %>%
  select(-c(country)) %>% 
  mutate(fiscal_year = as.character(fiscal_year)) %>%
  mutate(fiscal_year = str_replace(fiscal_year, "20", "FY")) %>%
  mutate(standardizeddisaggregate = ifelse(indicator == "PLHIV_Residents", "Age/Sex/HIVStatus", standardizeddisaggregate)) %>% 
  mutate(indicator = ifelse(indicator == "PLHIV_Residents", "PLHIV", indicator))

#bind plhiv and all tabs for import into Tableau
dp_final <- bind_rows(dp_filtered, dp_plhiv_filtered)


#recode snuprioritization
dp_final <- dp_final %>% 
  mutate(snuprioritization = recode(snuprioritization,
                                    "2 - Scale-up: Aggressive" = "2 - Scale-Up: Aggressive",
                                    "1 - Scale-up: Saturation" = "1 - Scale-Up: Saturation")) 

today <- lubridate::today()


#write_csv(dp_final, glue::glue("Dataout/Mozambique-tst-validation_v1_{today}.csv"))

# table analytics ----------------------------------------------------------------------------

#get target table
dp_table <- dp_final %>% 
  group_by(operatingunit, indicator, standardizeddisaggregate, fiscal_year) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE), .groups = "drop") %>% 
  filter(fiscal_year != "FY23") %>% 
  pivot_wider(names_from = "fiscal_year", values_from = "targets") %>% 
  relocate(`FY24`, .after = 3) %>% 
  mutate(diff = `FY25` - `FY24`,
         delta = (`FY25` - `FY24`)/`FY24`)

write_csv(dp_table, glue::glue("Dataout/Mozambique-tst-validation_v1_{today}.csv"))


# dp_final %>% 
#   filter(indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST", "HTS_TST_POS",
#                           "TX_PVLS", "TX_PVLS_D")) %>% 
#   mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-09", "10-14") ~ "<15",
#                                   is.na(ageasentered) ~ NA,
#                                   TRUE ~ "15+")) %>% 
#   count(ageasentered, trendscoarse)
# 
# 
# #grab msd from below
# 
# 
# # bind_rows(df_msd_final, dp_final) %>% 
# #   filter(fiscal_year != "FY21") %>% 
# #   group_by(operatingunit, indicator, standardizeddisaggregate, fiscal_year) %>% 
# #   summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>% view
# 
# 
# # # MUNGE MSD -------------------------------------------------------------------
# 
# #pull in DP columns (will filter to snu1 if you changed this above)
# dp_cols <- dp_final %>%
#   names()
# 
# df_filtered <- df_msd %>%
#   # filter(fiscal_year %in% c(2022, 2023)) %>% #filter to 2022 and 2023
#   select(any_of(dp_cols),funding_agency, mech_code) 
# 
# 
# #join agency lookmap and mutate FY
# df_filtered1 <- df_filtered %>%
#   semi_join(msd_disagg_map, by = c("indicator", "numeratordenom", "standardizeddisaggregate")) %>%
#   clean_indicator() %>%
#   mutate(fiscal_year = as.character(fiscal_year)) %>%
#   mutate(fiscal_year = str_replace(fiscal_year, "20", "FY"))
# 
# #join agency lookmap and mutate FY
# df_filtered2 <- df_filtered %>%
#   semi_join(msd_disagg_map2, by = c("indicator", "numeratordenom", "standardizeddisaggregate", "fiscal_year")) %>%
#   clean_indicator() %>%
#   mutate(fiscal_year = as.character(fiscal_year)) %>%
#   mutate(fiscal_year = str_replace(fiscal_year, "20", "FY"))
# 
# # Collapse age bands (note: this step may take a long time)
# df_age_adj <- df_filtered %>%
#   left_join(age_map, by = c("indicator", "ageasentered" = "age_msd")) %>%
#   mutate(age_dp = ifelse(is.na(age_dp), ageasentered, age_dp)) %>%
#   select(-ageasentered) %>%
#   # mutate(cumulative = ifelse(is.na(cumulative), 0, cumulative)) %>%
#   # mutate(targets = ifelse(is.na(cumulative), 0, cumulative)) %>%
#   group_by(across(-c(cumulative, targets))) %>%
#   # group_by_all() %>%
#   # group_by(indicator, fiscal_year, standardizeddisaggregate, age_dp) %>%
#   summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop")
# 
# df_msd_final <- df_age_adj %>%
#   select(-c(funding_agency, mech_code)) %>%
#   relocate(age_dp, .after = 8) %>%
#   relocate(any_of(c("cumulative", "targets")), .after = 13) %>%
#   #relocate(funding_agency, .after = 15) %>%
#   rename(ageasentered = age_dp)
# 
# 
# 
# # BIND together
# df_final <- bind_rows(dp_final, df_msd_final)
# 
# 
# 
# # EXPORT ---------------------------------------------------------------
# 
# today <- lubridate::today()
# 
# write_csv(df_msd_final, glue::glue("Dataout/moz-cop-validation-msd-v1_{today}.csv"))
# 
