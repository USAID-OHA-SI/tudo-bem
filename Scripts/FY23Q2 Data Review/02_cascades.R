# PROJECT: Here-it-goes-again-reprise
# PURPOSE: Analysis of FY23Q2 data for cascade plots
# AUTHOR: K. Srikanth | SI
# REF ID:   3067f005
# LICENSE: MIT
# DATE: 2023-05-11
# UPDATED: 2024-06-12

# LOCALS & SETUP ============================================================================

# Load Q1 functions
#devtools::install_github(repo = "USAID-OHA-SI/cascade", ref = "dev")
source("Scripts/helper-call_all_helpers.R")

# Libraries
library(gagglr)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(gt)
library(gtExtras)
library(selfdestructin5)
library(cascade)


# SI specific paths/functions  
load_secrets()

# REF ID for plots
ref_id <- "3067f005"

# Functions  


# LOAD DATA ============================================================================ 

# SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata, pattern = "PSNU_IM_FY22.*South Africa")

df_msd <- read_psd(file_path) 

df_msd <- df_msd  %>% 
  clean_agency()

disag_peds <- c("Modality/Age Aggregated/Sex/Result", "Modality/Age/Sex/Result","Age/Sex/HIVStatus",
                "Age/Sex/CD4/HIVStatus","Age/Sex/Indication/HIVStatus")

metadata <- get_metadata(file_path)

df_msd %>% 
  filter(fiscal_year == 2024,
         funding_agency == "USAID", 
         indicator == "HTS_TST_POS",
         standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
  count(fiscal_year, indicator, sex, wt = qtr2)
# CREATE USAID CASCADE ============================================================================

#  USING DEV VERSION to get around TX_CURR_LAG2 missing

# All of PEPFAR MOZ cascade
return_cascade(df_msd %>% filter(funding_agency == "USAID"), 1) %>% prinf()

# Generate plots for all agencies
batch_cascade_plot(df_msd, imgpath = "Images/Cascade/MOZ_PEPFAR", imgtype = ".png")

batch_cascade_plot(df_msd %>% filter(funding_agency == "USAID"), 
                   imgpath = "Graphics/Cascade", imgtype = ".SVG")

# MECHANISM CASCADES ============================================================================

# Loop over mechs

df_msd %>% filter(fiscal_year == 2023,
                  indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST", "HTS_TST_POS")) %>% count(mech_name, funding_agency)

batch_cascade_plot(df_msd %>% filter(mech_name == "Efficiencies for Clinical HIV Outcomes (ECHO)"),
                   imgpath = "Images/Cascade/ECHO", imgtype = ".png")


# Loop over other snus
batch_cascade_plot(df_msd %>% filter(snu1 == "Manica"), 
                   imgpath = "Images/Cascade/Manica", imgtype = ".png")

batch_cascade_plot(df_msd %>% filter(snu1 == "Nampula"), 
                   imgpath = "Images/Cascade/Nampula", imgtype = ".png")


# Sparkline summaries  ============================================================================

df_spark <- return_cascade(df_msd %>% filter(funding_agency == "USAID", snu1 == "Manica"), 1)
# mutate(results = case_when(
#   indicator == "TX_NET_NEW" & period == "FY22Q1" ~ 0,
#   TRUE ~ results
# ))
# 
df_spark %>% 
  mutate(start_point = case_when(
    period == min(period) ~ 1,
    TRUE ~ 0
  ),
  end_point = case_when(
    period == max(period) ~ 1,
    TRUE ~ 0
  ),
  ends = ifelse(start_point == 1 | end_point == 1, 1, 0)
  ) %>% group_by(indicator) %>% 
  mutate(min_results = min(results),
         indic_colors = dplyr::case_when(indicator == "HTS_TST" ~ "#877ec9", 
                                         indicator == "HTS_TST_POS" ~ "#b5aaf9", 
                                         indicator == "TX_NEW" ~ glitr::golden_sand_light, 
                                         indicator == "TX_NET_NEW" ~ glitr::golden_sand_light, 
                                         indicator == "TX_CURR" ~ glitr::golden_sand, 
                                         indicator == "TX_PVLS_D" ~ glitr::scooter_med, 
                                         indicator == "TX_PVLS" ~ glitr::scooter),
         indic_group = case_when(
           indicator %in% c("HTS_TST", "HTS_TST_POS") ~ "Testing",
           indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW") ~ "Treatment",
           indicator %in% c("TX_PVLS", "TX_PVLS_D") ~ "Viral Load"
         )
  ) %>% 
  ungroup() %>% 
  filter(indicator != "TX_CURR_Lag2") %>% 
  ggplot(aes(x = period, y = results, group = indicator)) +
  geom_ribbon(aes(ymin = min_results, ymax = results, fill = indic_colors), alpha = 0.25) +
  geom_line(size = 0.75, color = grey70k) +
  geom_point(data = . %>% filter(end_point == 1), shape = 19, color = grey80k, size = 3) +
  geom_point(data = . %>% filter(end_point == 0), shape = 19, color = grey80k, size = 1.5) +
  geom_text(data = . %>% filter(ends == 1), aes(label = label_number_si(accuracy = 1)(results), 
                                                vjust = -1, size = 12/.pt, 
                                                family = "Source Sans Pro"))+
  
  facet_wrap(~indicator, scales = "free_y", ncol = 2) +
  si_style_nolines() +
  scale_y_continuous(labels =  label_number_si(), expand = c(0.5, 0.5)) +
  labs(x = NULL, y = NULL,
       title = "CASCADE TRENDS FOR USAID",
       caption = glue("{metadata$caption}")) +
  scale_x_discrete(expand = c(0.05, 0)) +
  theme(axis.text.y = element_blank(), 
        strip.text = element_text(size = 15),
        legend.position  = "none") +
  scale_fill_identity()

