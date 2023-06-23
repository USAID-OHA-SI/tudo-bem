# PROJECT: Here-it-goes-again
# PURPOSE: Prep files for FY23 Q1 Review
# AUTHOR: Tim Essam | SI
# REF ID:   0b2168d3
# LICENSE: MIT
# DATE: 2023-01-20
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

# # Libraries
# library(gagglr)
# library(tidyverse)
# library(scales)
# #library(sf)
# library(extrafont)
# library(tidytext)
# library(patchwork)
# library(ggtext)
# library(glue)
# 
# 
# # SI specific paths/functions  
# load_secrets()
# merdata <- file.path(glamr::si_path("path_msd"))
# file_path <- return_latest(folderpath = merdata, pattern = "Genie-PSNU.*Zambia")
# site_path <- return_latest(folderpath = merdata, pattern = "Genie-Site.*Zambia")
# 
# msd_path <- return_latest(folderpath = merdata, pattern = "PSNU_IM_FY21-23_20230512.*Zambia")
# 
# df_genie <- read_psd(file_path)
# df_msd <- read_psd(msd_path) %>% filter(operatingunit == "Zambia")
# 
# # Grab metadata
# get_metadata(file_path)
# 
# # REF ID for plots
# ref_id <- "0b2168d3"
# 
# # Functions  
# source("Scripts/helper-cw_mechs.R")
# source("Scripts/helper-swap_targets.R")
# source("Scripts/helper-create_vl_df.R")




# Augment mech names ============================================================================  

# Replaces mechanism names with a shorter version
fix_mech_names <- function(.data) {
  .data %>%
    dplyr::left_join(mech_names_cw, by = c("mech_code", "mech_name")) %>%
    dplyr::mutate(mech_name = ifelse(
      !is.na(mech_name_short),
      mech_name_short,
      mech_name
    ))
} 


# GT functions ============================================================================

# Given two metrics, formats achievement
format_achv <- function(x, y){
  str_c(scales::label_number(accuracy = 1, scale_cut = cut_short_scale())(x), 
        scales::label_number(accuracy = 1, scale_cut = cut_short_scale())(y), sep = " / "
  )
}


# Formats an indicator for a table  
format_indicator <- function(x, y, z){
  name <- stringr::word(x, 1)
  name2 <- stringr::word(y, start = 1, end = 3)
  color <- stringr::word(z)
  
  glue::glue("<div style='line-height:10px'<span style='font-weight:bold;font-size:14px;color:{color}'>{name}</div>
             <div><span style='font-weight:regular;font-size:11px'>{name2}</br></div>")
}


# Create a gt loop to format indicators
legend_chunk <- gt::md(glue::glue("Achievement legend: <img src= '{selfdestructin5::legend_snapshot}' style='height:15px;'> "))

# Index Testing Modalities ============================================================================

munge_modality_mech <- function(df, begin_pd = "FY20Q1", ...){   
  df_hts_full <- df %>% 
    filter(indicator == "HTS_TST_POS",
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           fiscal_year <= metadata$curr_fy) %>% 
    mutate(mod_type = case_when(
      str_detect(modality, "Index") ~ "Index",
      str_detect(modality, "OtherPITC") ~ "Other PITC",
      str_detect(modality, "PMTCT") ~ "PMTCT",
      modality == "VCT" ~ "VCT",
      str_detect(modality, "SNSMod") ~ "Community SNS",
      TRUE ~ "Other")
    ) %>%
    group_by(fiscal_year, mod_type, mech_name) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    reshape_msd() %>%
    select(-period_type) %>%
    group_by(period) %>%
    mutate(contribution = value/sum(value)) %>%
    ungroup() %>%
    mutate(start = case_when(period == min(period) ~ contribution),
           end = case_when(period == max(period) ~ contribution)) %>%
    mutate(mod_order = fct_reorder(mod_type, value, .desc = T)) %>% 
    complete(mod_type, period, mech_name) %>% 
    group_by(mod_type, mech_name) %>% 
    fill(mod_order, .direction = "up") %>% 
    group_by(period, mech_name) %>% 
    mutate(pd_tot = sum(value, na.rm = T), 
           pd_25 = pd_tot * 0.25, 
           pd_50 = pd_tot * 0.5,
           pd_75 = pd_tot * 0.75) %>% 
    ungroup() %>% 
    mutate(mod_color = case_when(
      mod_type == "Index" ~ "#855C75", 
      mod_type == "VCT" ~ "#D9AF6B",
      mod_type == "Other PITC" ~ "#AF6458",
      mod_type == "PMTCT"~ "#736F4C",
      mod_type == "Community SNS" ~ "#526A83",
      TRUE ~ "#7C7C7C"
    ),
    note = case_when(
      mod_type == "Index" & period == begin_pd ~ "HTS_TST_POS",
      TRUE ~ NA_character_
    )) %>% 
    filter(!is.na(mod_order))
  return(df_hts_full)
}


# Index testing versus all other ------------------------------------------

munge_modality_index <- function(df, ...){   
  df_hts_full <- df %>% 
    filter(indicator == "HTS_TST_POS",
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           fiscal_year <= metadata$curr_fy) %>% 
    mutate(mod_type = case_when(
      str_detect(modality, "Index") ~ "Index",
      TRUE ~ "Other")
    ) %>%
    group_by(fiscal_year, mod_type, ...) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    reshape_msd() %>%
    select(-period_type) %>%
    group_by(period, ...) %>%
    mutate(contribution = value/sum(value)) %>%
    ungroup() %>%
    mutate(start = case_when(period == min(period) ~ contribution),
           end = case_when(period == max(period) ~ contribution)) %>%
    mutate(mod_order = fct_reorder(mod_type, value, .desc = T)) %>% 
    complete(mod_type, period, ...) %>% 
    group_by(mod_type, ...) %>% 
    fill(mod_order, .direction = "up") %>% 
    group_by(period, ...) %>% 
    mutate(pd_tot = sum(value, na.rm = T), 
           pd_25 = pd_tot * 0.25, 
           pd_50 = pd_tot * 0.5,
           pd_75 = pd_tot * 0.75) %>% 
    ungroup() %>% 
    mutate(mod_color = case_when(
      mod_type == "Index" ~ "#855C75", 
      TRUE ~ "#D9AF6B"
    ),
    note = case_when(
      mod_type == "Index" & period == "FY20Q1" ~ "HTS_TST_POS",
      TRUE ~ NA_character_
    )) %>% 
    filter(!is.na(mod_order))
  return(df_hts_full)
}  


# Every Nth ---------------------------------------------------------------

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}  

# COLORS ------------------------------------------------------------------

#color options
#eacbd2, #dfaeb4, #be3455, #32213a, #383b53
#be3455, #6f9283, #8d9f87, #cdc6a5, #f0dcca


# Check intersection of two columns
# Function to compare two dataframe columns, return a count of differences
compare_vars <- function(x, y) {
  
  # Compare each variable, both ways
  xy <- length(setdiff(x, y))
  yx <- length(setdiff(y, x))
  
  if(xy == 0 & yx == 0){
    return("There are no differences between the columns")
  }
  
  print(str_c(xy, " differences between x and y"))
  print(str_c(yx, " differences between y and x"))
}


# Shrink size of rows in GT tables  
shrink_rows <- function(gt_obj){
  gt_obj %>% 
    tab_options(
      data_row.padding = px(1),
      row_group.padding = px(2),
      heading.padding = px(1)
    ) 
}  


# Creates a wide viral load coverage data frame for use in basic plots
create_vl_df <- function(df, ...) {
  df <- df %>%
    filter(
      indicator %in% c("TX_CURR", "TX_PVLS"),
      standardizeddisaggregate %in% c(
        "Age/Sex/HIVStatus",
        "Age/Sex/Indication/HIVStatus"
      )
    ) %>%
    gophr::clean_indicator() %>%
    group_by(indicator, fiscal_year, ...) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
              .groups = "drop") %>%
    reshape_msd(include_type = FALSE) %>%
    pivot_wider(
      names_from = indicator,
      names_glue = "{tolower(indicator)}"
    ) %>%
    group_by(...) %>% 
    mutate(
      tx_curr_lag2 = lag(tx_curr, n = 2),
      vlc = tx_pvls_d / tx_curr_lag2,
      vls = tx_pvls / tx_pvls_d,
      vls_adj = tx_pvls / tx_curr_lag2
    ) %>% 
    ungroup()
  return(df)
}  