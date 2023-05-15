# Libraries

library(tidyverse)
library(glamr)
library(Interesting)
library(googledrive)
library(googlesheets4)


# SETUP

# Processing folder
proc_folder <- "cirg-submissions"

# Processing Date
proc_date <- glamr::curr_date()
proc_date <- "2022-09-12"

#cir_setup(folder = proc_folder, dt = proc_date)
cir_setup(folder = proc_folder, dt = proc_date)


pull_submissions <- function(curr_pd, ou_sel) {
  
  cir_subm_sheet <<- "1amabNYu1HF9rZ1Y-Hy5p8lQ73Ynll744HZxPyZuVEgI"
  
  cir_subm_id <- as_id(cir_subm_sheet)
  
  df_cir_subm <- googlesheets4::read_sheet(cir_subm_id)
  
  df_cir_subm <- df_cir_subm %>%
    janitor::clean_names() %>%
    select(subm_time = timestamp,
           subm_poc = email_address,
           subm_ou = operating_unit_country,
           subm_period = custom_indicator_fy_and_period,
           subm_type = what_type_of_submission_is_this,
           subm_tmp_type = which_template_s_are_you_submitting,
           subm_tech_areas = what_technical_area_s_are_you_submitting_data_for,
           subm_files = upload_your_custom_indicator_template_file_s_here_excel_sheets_only_no_google_sheets,
           subm_processed = processed_by_cirg) %>%
    rowwise() %>%
    mutate(subm_count = length(unlist(str_split(subm_files, ", "))),
           subm_id = as.numeric(lubridate::as_datetime(subm_time, tz = "EST"))) %>%
    ungroup() %>%
    relocate(subm_id, .before = 1) %>%
    relocate(subm_processed, .after = last_col())
  
  #if country is specified, filter to that country
  if (!is.null(ou_sel)) {
    df_cir_subm <- df_cir_subm %>% 
      filter(subm_ou %in% ou_sel)
  }
  

  df_cir_files <- df_cir_subm %>%
    filter(subm_period == curr_pd) %>%
    select(subm_id, subm_file = subm_files) %>%
    separate_rows(subm_file, sep = ",\\s") %>%
    mutate(subm_file_id = str_extract(subm_file, "(?<=\\?id\\=).*"),
           subm_filename = pull(drive_get(as_id(subm_file_id)), name),
           subm_file_valid = str_detect(subm_filename, ".xlsx$")) %>%
    relocate(subm_file_id, .after = subm_id)
  
  dir_raw <- cir_folder(type = "raw", dt = proc_date)
  
  df_cir_files %>%
    filter(subm_file_valid = TRUE) %>%
    select(subm_file_id, subm_filename) %>%
    #filter(row_number() == 1) %>%
    #pull(subm_file_id) %>% first() %>%
    pwalk(~drive_download(file = as_id(.x),
                          path = file.path(dir_raw, .y),
                          overwrite = T))
  
  
}

# GOOGLE DRIVE - TODO Function to pull latest/resubmisions



googledrive::drive_get(cir_subm_id)

#googlesheets4::gs4_browse(cir_subm_id)
googledrive::drive_browse(cir_subm_id)

df_cir_subm <- googlesheets4::read_sheet(cir_subm_id)

df_cir_subm %>% glimpse()

# CIR Submissions

df_cir_subm <- df_cir_subm %>%
  janitor::clean_names() %>%
  select(subm_time = timestamp,
         subm_poc = email_address,
         subm_ou = operating_unit_country,
         subm_period = custom_indicator_fy_and_period,
         subm_type = what_type_of_submission_is_this,
         subm_tmp_type = which_template_s_are_you_submitting,
         subm_tech_areas = what_technical_area_s_are_you_submitting_data_for,
         subm_files = upload_your_custom_indicator_template_file_s_here_excel_sheets_only_no_google_sheets,
         subm_processed = processed_by_cirg) %>%
  rowwise() %>%
  mutate(subm_count = length(unlist(str_split(subm_files, ", "))),
         subm_id = as.numeric(lubridate::as_datetime(subm_time, tz = "EST"))) %>%
  ungroup() %>%
  relocate(subm_id, .before = 1) %>%
  relocate(subm_processed, .after = last_col())

# CIR Submission files



# DOWNLOAD Submissions

dir_raw <- cir_folder(type = "raw", dt = proc_date)

df_cir_files %>%
  filter(subm_file_valid = TRUE) %>%
  select(subm_file_id, subm_filename) %>%
  #filter(row_number() == 1) %>%
  #pull(subm_file_id) %>% first() %>%
  pwalk(~drive_download(file = as_id(.x),
                        path = file.path(dir_raw, .y),
                        overwrite = T))

# LOCAL FILES

#subm <- fs::dir_ls("~/Downloads", regexp = "CIRG_.*.xlsx$")
subm <- fs::dir_ls(dir_raw, regexp = "CIRG_.*.xlsx$")

subm %>% 
  lapply(readxl::read_excel)

# Metadata
# subm %>%
#   dplyr::first() %>%
#   cir_extract_meta()
#
# subm %>%
#   dplyr::first() %>%
#   check_meta()
#
# subm %>%
#   dplyr::first() %>%
#   check_tabs()

# Initial Validation
meta <- subm %>%
  dplyr::first() %>%
  validate_initial()

metas <- subm %>%
  map_dfr(validate_initial)

# Import & 2nd round of Validation
df_subm <- subm %>%
  dplyr::first() %>%
  cir_import(template = meta$type)



cir_import <- function(filepath, template = NULL){
  
  #tmp <- ifelse(is.null(template) | is.na(template), "[Not provided]", template)
  tmp <- meta$type
  
  checks <- tibble::tibble()
  
  logger::log_info("\nSubmission template: {null_to_chr(tmp)} ...")
  
  df <- filepath %>%
    readxl::excel_sheets() %>% # TODO - Use sheets_valid from `vinit`
    stringr::str_subset("CIRG") %>%
    purrr::map_dfr(function(.x) {
      
      # Notification
      if (interactive()) {
        cat("\n---- IMPORTING SHEET ----",
            "\nCIRG sheet name: ", paint_blue(.x),
            "\n")
      }
      
      logger::log_info("\nImporting data from sheet: {.x} ...")
      
      # Read data from excel sheet
      df_tab <- readxl::read_excel(filepath,
                                   sheet = .x,
                                   skip = 2,
                                   col_types = "text")
      
      # Notification
      if (interactive()) {
        cat("\nRows count: ", paint_blue(nrow(df_tab)),
            "\n")
      }
      
      logger::log_info("Rows count = {nrow(df_tab)}")
      
      if("mechanismid" %in% names(df)) {
        logger::log_info("\nRenaming `mechanismid` to `mech_code`")
        df <- dplyr::rename(df, mech_code = mechanismid)
      }
      
      # Skip import validations
      if (is.null(tmp)) {
        logger::log_info("\nSkipping import validations ...")
        
        df_tab <- df_tab %>%
          dplyr::mutate(temp_type = NA_character_,
                        sheet = .x,
                        row_id = dplyr::row_number() + 2)
        
        return(df_tab)
      }
      
      # Validate only if template is provided
      vimp <- validate_import(df_tab, template = tmp)
      
      vimp$checks <- vimp$checks %>%
        dplyr::mutate(filename = basename(filepath), sheet = .x)
      
      checks <<- dplyr::bind_rows(checks, vimp$checks)
      
      vimp$data <- vimp$data %>%
        dplyr::mutate(temp_type = template,
                      sheet = .x,
                      row_id = dplyr::row_number() + 2)
      
      #return(df_tab)
      return(vimp$data)
    })
  
  return(list(
    "checks" = checks,
    "data" = df
  ))
}

df_subm$checks
df_subm$data

# Import all
df_imp_checks <- metas %>%
  filter(subm_valid == TRUE) %>%
  select(filename, type) %>%
  pmap_dfr(function(filename, type) {
    subm <- cir_import(filepath = file.path(dir_raw, filename), template = type)
    return(subm$checks)
  })

df_imp_data <- metas %>%
  filter(subm_valid == TRUE) %>%
  select(filename, type) %>%
  pmap_dfr(function(filename, type) {
    subm <- cir_import(filepath = file.path(dir_raw, filename), template = type)
    return(subm$data)
  })

# Import, Validations & Transformations
df_subm <- subm %>%
  dplyr::first() %>%
  cir_processing()

