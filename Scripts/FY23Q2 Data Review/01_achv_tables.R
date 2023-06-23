# PROJECT: FY23 Q2 Review of Moz Data
# PURPOSE: Munge and Analysis of MSD for Moz
# AUTHOR: Karishma Srikanth | SI
# REF ID:   3b00def6
# LICENSE: MIT
# DATE: 2023-06-15
# NOTES: adapted from Here-it-goes-again-reprise/Scripts/01_achv_tables.R (from TE)

# LOCALS & SETUP ============================================================================

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

# SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata, pattern = "MER_Structured_Datasets_PSNU_IM_FY21-23_20230512_v1_1_Mozambique")

# Grab metadata
get_metadata(file_path)

# REF ID for plots
ref_id <- "3b00def6"

# Load functions    
source("Scripts/99_utilities.R")


# CUSTOM IP TABLES --------------------------------------------------------

mk_ptr_tbl <- function(df, mech_id)  {    
  ip_mdb <- 
    df %>% 
    filter(mech_code == mech_id) %>% 
    make_mdb_df() %>% 
    reshape_mdb_df(., metadata$curr_pd) 
  
  mech_name <-  
    df %>% 
    filter(mech_code == mech_id) %>%
    distinct(mech_name) %>% 
    pull(mech_name)
  
  ip_mdb %>%   
    create_mdb(ou = "Mozambique", type = "main", metadata$curr_pd, metadata$source) %>% 
    tab_header(
      title = glue::glue("{mech_name} PERFORMANCE SUMMARY")
    ) %>% 
    gtsave(path = "Images", filename = glue::glue("{mech_name}_mdb_main.png"))
}


# LOAD DATA ============================================================================  

df_genie <- read_psd(file_path) 

df_genie %>% filter(fiscal_year == 2023, funding_agency == "USAID") %>% count(prime_partner_name, mech_name, mech_code) %>% prinf()

# MUNGE ============================================================================

# Check if data is in yet 
df_genie %>% filter(fiscal_year == 2023, indicator %in% cascade_ind) %>% View()

df_pepfar <- df_genie %>% mutate(funding_agency = "PEPFAR")

# SUMMARY TABLES PEPFAR ===================================================================

mdb_df   <- make_mdb_df(df_pepfar)
mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd) %>%  
  mutate(agency = "PEPFAR") 

# Create the treatment data frame needed for derived indicators
mdb_df_tx    <- make_mdb_tx_df(df_pepfar)
mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd) %>% 
  mutate(agency = "PEPFAR") 

mdb_tbl %>% 
  # filter(indicator != "GEND_GBV") %>%
  create_mdb(ou = "Mozambique", type = "main", metadata$curr_pd, metadata$source) %>%
  shrink_rows() %>% 
  gtsave_extra(path = "Images", filename = glue::glue("Mozambique_PEPFAR_{metadata$curr_pd}_mdb_main.png"))  


create_mdb(mdb_tbl_tx, ou = "Mozambique", type = "treatment", metadata$curr_pd, metadata$source) %>% 
  bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
  embiggen() %>% 
  tab_options(
    data_row.padding = px(1),
    row_group.padding = px(2),
    heading.padding = px(1)
  ) %>% 
  gtsave_extra(., path = "Images", filename = glue::glue("{metadata$curr_pd}_Mozambique_PEPFAR_MMD_VL_MD.png"))   

# SUMMARY TABLES BY USAID OTHER AGENCIES

# SUMMARY TABLES BY AGENCY ------------------------------------------------


mdb_df   <- make_mdb_df(df_genie)
mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd)  

# Create the treatment data frame needed for derived indicators
mdb_df_tx    <- make_mdb_tx_df(df_genie)
mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)

legend_chunk_q2 <- gt::md(glue::glue("Legend: Cumulative <img src= '{legend_q2}' style='height:15px;'>    &emsp;
                                       Snapshot (TX_CURR) <img src= '{legend_snapshot}' style='height:15px;'> "))  

mdb_tbl %>% 
  filter(indicator %ni% c("GEND_GBV", "KP_PREV", "TB_PREV")) %>%
  create_mdb(ou = "Mozambique", type = "main", metadata$curr_pd, metadata$source,
             legend = legend_chunk_q2) %>% 
  shrink_rows() %>% 
  cols_width(
    indicator2 ~ px(200),
    contains("achv") ~ px(5),
    contains("present_z") ~ px(10)
  ) %>% 
  gtsave_extra(path = "Images", filename = glue::glue("Mozambique_{metadata$curr_pd}_mdb_main_AMARA.png"))  


create_mdb(mdb_tbl_tx, ou = "Mozambique", type = "treatment", metadata$curr_pd, metadata$source) %>% 
  bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
  embiggen() %>% 
  shrink_rows() %>% 
  tab_source_note(
    source_note = md("**TX_CURR NOTE:** South Africa has no MMD program and has been excluded from TX_CURR and MMMD calculations")
  ) %>% 
  gtsave_extra(., path = "Images", filename = glue::glue("{metadata$curr_pd}_Mozambique_MMD_VL_MD.png"))  

# Partner Tables ============================================================================

# Loop over function and create tables for each of the main C&T mechs
mech_list <- c(82075, 17413, 82086, 17399)
map(mech_list, ~mk_ptr_tbl(df_genie, .x))





