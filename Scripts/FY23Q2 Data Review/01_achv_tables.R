# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  quarterly achv table functions - iterate function
# REF ID:   3e49ddd2 
# LICENSE:  MIT
# DATE:     2024-06-12
# UPDATED:  

# DEPENDENCIES ------------------------------------------------------------

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

# GLOBAL VARIABLES --------------------------------------------------------

# SI specific paths/functions  
load_secrets()
ref_id <- "3e49ddd2"

# Load functions    
source("Scripts/99_utilities.R")


make_achv_table <- function(ou, tbl, save = F) {
  
  #get filepath
  filepath <- si_path() %>% 
    return_latest(glue::glue("PSNU_IM_FY22.*{ou}"))
  
  metadata <- get_metadata(filepath) 
  
  df_msd <- read_psd(filepath)
  
  #store legend
  
  if (metadata$curr_qtr == 1) {
    legend  <- legend_q1
  } else if (metadata$curr_qtr == 2) {
    legend  <- legend_q2
  } else if (metadata$curr_qtr == 3) {
    legend  <- legend_q3
  } else {
    legend  <- legend_snapshot
  }
  
  legend_chunk <- gt::md(glue::glue("Legend: Cumulative <img src= '{legend}' style='height:15px;'>    &emsp;
                                       Snapshot (TX_CURR) <img src= '{legend_snapshot}' style='height:15px;'> "))  
  
  
  if (tbl == "main") {
    
    mdb_df   <- make_mdb_df(df_msd)
    mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd)
    
    tbl_viz <- mdb_tbl %>% 
      filter(indicator %ni% c("GEND_GBV", "KP_PREV", "TB_PREV")) %>%
      create_mdb(ou = ou, type = "main", metadata$curr_pd, metadata$source,
                 legend = legend_chunk) %>% 
      shrink_rows() %>% 
      cols_width(
        indicator2 ~ px(200),
        contains("achv") ~ px(5),
        contains("present_z") ~ px(10)
      ) 
    
    if (save == T) {
      tbl_viz %>%
        gtsave_extra(path = "Images", filename = glue::glue("{ou}_{metadata$curr_pd}_mdb_main.png"))
    }
    
  } else if (tbl == "treatment") {
    
    # Create the treatment data frame needed for derived indicators
    mdb_df_tx    <- make_mdb_tx_df(df_msd)
    mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)
    
    tbl_viz <- create_mdb(mdb_tbl_tx, ou = ou, type = "treatment", metadata$curr_pd, metadata$source) %>% 
      bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
      embiggen() %>% 
      shrink_rows() %>% 
      tab_source_note(
        source_note = md("**TX_CURR NOTE:** South Africa has no MMD program and has been excluded from TX_CURR and MMMD calculations")
      ) 
    
    tbl_viz %>%
      gtsave_extra(., path = "Images", filename = glue::glue("{ou}_{metadata$curr_pd}_mdb_TX.png"))  
    
  }
  
  return(tbl_viz)
  
}

make_achv_table("Mozambique", tbl = "treatment")
make_achv_table("Mozambique", tbl = "main", save = T)
make_achv_table("South Africa", tbl = "main", save = T)

# IMPORT ------------------------------------------------------------------

return_data
msd

# MUNGE -------------------------------------------------------------------






