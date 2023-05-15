# PROJECT:  moz-cop22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Review MOZ approval memo
# LICENSE:  MIT
# DATE:     2022-05-11
# UPDATED:  


#DEPENDENCIES ----------------------------------------------------------------------

library(tameDP)
library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(glue)
library(scales)


#GLOBAL ---------------------------------------------------------------------------

folderpath <- "Data/Datapack"

dp_path <- folderpath %>% 
  return_latest("Target Setting Tool_Mozambique_MAY02 OVC-DREAMS DoD-KP_PREV removed")

plhiv_path <- folderpath %>% 
  return_latest("PSNUxIM_Mozambique_APR26")

# IMPORT --------------------------------------------

df_dp <- tame_dp(dp_path)

df_dp_mech <- tame_dp(plhiv_path, type = "PSNUxIM", map_names = TRUE)

df_plhiv <- tame_dp(dp_path, type = "PLHIV")


# MUNGE --------------------------------------------

df_dp %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2024) %>% 
  group_by(fiscal_year, indicator
           , snuprioritization
) %>% 
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
  ungroup() %>% View()

#check cascade indics
df_dp %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2024,
         indicator %in% c("TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS",
                          "HTS_SELF", "HTS_INDEX", "HTS_TST", "HTS_TST_POS", "TB_STAT", "TB_STAT_D", "TB_ART",
                          "TB_PREV", "TX_TB_D")) %>% 
  mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-09", "10-14") ~ "<15",
                                  ageasentered %in% c("15-24", "25-34", "35-49", "50+") ~ "15+",
                                  TRUE ~ ageasentered)) %>% 
  group_by(fiscal_year, indicator,trendscoarse
           , snuprioritization
           ) %>% 
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
  ungroup() %>% View()

#PMTCT
df_dp %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2024,
         str_detect(indicator, "PMTCT")) %>% 
  mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-09", "10-14") ~ "<15",
                                  ageasentered %in% c("15-24", "25-34", "35-49", "50+") ~ "15+")) %>% 
  group_by(fiscal_year, indicator,trendscoarse, 
           snuprioritization,
           statushiv) %>% 
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
  ungroup() %>% View()

#ovc
df_dp %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2024,
         str_detect(indicator, "OVC")) %>% 
  mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-04", "05-09", "10-14", "15-17") ~ "<18",
                                  TRUE ~ ageasentered)) %>% 
  group_by(fiscal_year, indicator,trendscoarse
           , snuprioritization
           ) %>% 
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
  ungroup() %>% View()

#check agency roll up ---------------------------

df_dp_mech %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2024) %>% 
  group_by(fiscal_year, indicator, funding_agency) %>% 
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
  ungroup() %>% View()

df_dp_mech %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2024,
         indicator %in% c("TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS",
                          "HTS_SELF", "HTS_INDEX", "HTS_TST", "HTS_TST_POS", "TB_STAT", "TB_STAT_D", "TB_ART",
                          "TB_PREV", "TX_TB_D")) %>% 
  mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-09", "10-14") ~ "<15",
                                  ageasentered %in% c("15-24", "25-34", "35-49", "50+") ~ "15+",
                                  TRUE ~ ageasentered)) %>% 
  group_by(fiscal_year, indicator,trendscoarse
           , funding_agency
  ) %>% 
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
  ungroup() %>% View()

#PMTCT
df_dp_mech %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2024,
         str_detect(indicator, "PMTCT")) %>% 
  mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-09", "10-14") ~ "<15",
                                  ageasentered %in% c("15-24", "25-34", "35-49", "50+") ~ "15+")) %>% 
  group_by(fiscal_year, indicator,trendscoarse, 
           funding_agency,
           statushiv) %>% 
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
  ungroup() %>% View()

#ovc
df_dp_mech %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2024,
         str_detect(indicator, "OVC")) %>% 
  mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-04", "05-09", "10-14", "15-17") ~ "<18",
                                  TRUE ~ ageasentered)) %>% 
  group_by(fiscal_year, indicator,trendscoarse
           , funding_agency
  ) %>% 
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
  ungroup() %>% View()

# PARTNER roll up ----------------------------------------------------

df_dp_mech %>% 
  clean_indicator() %>% 
  filter(fiscal_year == 2024,
         indicator %in% c("TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS", "HTS_SELF", "HTS_TST", "HTS_TST_POS", "HTS_RECENT",
                          "HTS_INDEX")) %>% 
  mutate(trendscoarse = case_when(ageasentered %in% c("<01", "01-09", "10-14") ~ "<15",
                                  ageasentered %in% c("15-24", "25-34", "35-49", "50+") ~ "15+",
                                  TRUE ~ ageasentered)) %>% 
  group_by(fiscal_year, indicator,trendscoarse
          , funding_agency, prime_partner_name, mech_code) %>% 
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
  ungroup() %>% View()

