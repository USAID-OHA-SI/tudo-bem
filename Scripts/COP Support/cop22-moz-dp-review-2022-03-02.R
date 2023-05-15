# PROJECT:  moz-cop22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Review MOZ DP
# LICENSE:  MIT
# DATE:     2022-03-02
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

path <- "C:/Users/ksrikanth/Documents/Datapack/Mozambique_datapack_Finalized_20220415.xlsx"

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

#read in Data Pack & tidy
df_dp <- tame_dp(path)

df_dp_mech <- tame_dp(path, type = "PSNUxIM")

df_plhiv <- tame_dp(path, type = "PLHIV")

df_plhiv %>%  filter(numeratordenom == "N") %>% 
  group_by(fiscal_year, indicator, numeratordenom) %>% 
  summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop")

df_psnu <- si_path() %>% 
  return_latest("PSNU_IM_FY20-22_20220211_v1_1_Mozambique") %>% 
  read_msd()


# MUNGE --------------------------------------------

#Get FY21 historical targets
fy21 <- df_psnu %>% 
  filter(fiscal_year == 2021,
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(fiscal_year, indicator, standardizeddisaggregate) %>% 
  summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>%
  rename(FY21_results = cumulative,
         FY21_target = targets,
         fy21_disagg = standardizeddisaggregate) %>% 
  select(-c(fiscal_year)) 


#FY22 and FY23 Targets
dp_target <- df_dp %>% 
  filter(numeratordenom == "N") %>% 
  group_by(fiscal_year, indicator, standardizeddisaggregate, numeratordenom) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = fiscal_year, values_from = targets) %>% 
  rename(FY22_target = `2022`,
         FY23_target = `2023`) %>% 
  select(-c(`2021`)) %>% 
  mutate(indicator = recode(indicator, "PREP_CURR" = "PrEP_CURR",
                            "PREP_NEW" = "PrEP_NEW",
                            "PREP_CT" = "PrEP_CT"))

#Filter to cascade indicators and join dfs
dp_cascade <- dp_target %>% 
left_join(fy21, by = "indicator") %>% filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_PVLS"), standardizeddisaggregate != "KeyPop/HIVStatus")


#VIZ ---

dp_cascade %>% 
  pivot_longer(cols = c("FY21_target", "FY22_target", "FY23_target"), names_to = "target_year", values_to = "target_val") %>% 
  mutate(value_label = clean_number(target_val, 1)) %>% 
  ggplot() +
  geom_col(aes(x = target_year, y = target_val, fill = fct_reorder(indicator, target_val, .desc = TRUE)), position = "dodge") +
  si_style_ygrid() +
  scale_y_continuous(labels = label_number_si(), expand = c(.005, .005)) +
  scale_fill_manual(values = c(scooter, golden_sand, moody_blue)) + 
  geom_text(aes(x = target_year, y = target_val, label = value_label), family = "Source Sans Pro", color = trolley_grey,
            hjust = 0, vjust = -1, na.rm = TRUE)+
  labs(x = NULL, y = NULL, title = "Cascade Targets FY21 to FY23",
       subtitle = "",
       caption = "") +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

si_save("Documents/moz-dp-tx.svg")



# Targets by PSNU ----------------------------------

psnu_target <- df_dp %>%
  filter(numeratordenom == "N") %>% 
  group_by(snu1, psnu, psnuuid, fiscal_year, indicator, standardizeddisaggregate, numeratordenom) %>% 
  summarise(across(c(targets), sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = fiscal_year, values_from = targets) %>% 
  rename(FY22_target = `2022`,
         FY23_target = `2023`) %>% 
  select(-c(`2021`)) %>% 
  mutate(indicator = recode(indicator, "PREP_CURR" = "PrEP_CURR",
                            "PREP_NEW" = "PrEP_NEW",
                            "PREP_CT" = "PrEP_CT"))

#Exploratory Viz - should do a comparison by FY22/FY23 targets across indicators

psnu_target %>% 
  pivot_longer(cols = c("FY22_target", "FY23_target"), names_to = "target_year", values_to = "target_val") %>%
  filter(indicator %in% c("TX_CURR"), standardizeddisaggregate != "KeyPop/HIVStatus",
         target_year == "FY23_target") %>% 
ggplot() +
  geom_col(aes(x = psnu, y = target_val,  fill = scooter)) +
  coord_flip() +
  facet_wrap(~snu1, scales = "free_y") +
  si_style_ygrid() +
  scale_y_continuous(labels = label_number_si(), expand = c(.005, .005)) +
  scale_fill_identity() + 
  # geom_text(aes(x = target_year, y = target_val, label = target_val), family = "Source Sans Pro", color = trolley_grey,
  #           hjust = 0, vjust = -1, na.rm = TRUE)+
  labs(x = NULL, y = NULL, title = "TX_CURR FY23 Targets by PSNU",
       subtitle = "",
       caption = "") +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))


# df_dp %>% 
#   filter(numeratordenom == "N",
#          indicator == "OVC_SERV") %>% 
#   group_by(fiscal_year, indicator, standardizeddisaggregate, otherdisaggregate) %>% 
#   summarise(across(c(targets), sum, na.rm = TRUE), .groups = "drop") %>% 
#   pivot_wider(names_from = fiscal_year, values_from = targets) %>% 
#   rename(FY22_target = `2022`,
#          FY23_target = `2023`) %>% 
#   select(-c(`2021`)) %>% 
#   mutate(indicator = recode(indicator, "PREP_CURR" = "PrEP_CURR",
#                             "PREP_NEW" = "PrEP_NEW",
#                             "PREP_CT" = "PrEP_CT"))




