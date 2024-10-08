# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  SBC exploration
# REF ID:   998c2d49 
# LICENSE:  MIT
# DATE:     2024-05-30
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
library(cascade)

# INITIALIZE API CONNECTION ------------------------------------------------

get_data_from_remote <- function(period, country, indicator, age, sex) {
  url <- paste0("https://naomi2023.azurewebsites.net/api/v1/data?period=",
                period,
                "&country=",
                country, 
                "&areaLevel=0&areaLevel=1&areaLevel=2&areaLevel=3", 
                "&indicator=", 
                indicator, 
                "&ageGroup=", 
                age, 
                "&sex=", 
                sex)
  print(url)
  data_pull <- read.csv(as.character(url)) %>%
    mutate(country = country) %>%
    mutate(indicator = indicator) %>%
    mutate(age = age) %>%
    mutate(sex = sex)
  
  return(data_pull)
}

#get_data_from_remote(period = "2022-4", country = "MOZ", indicator = "aware_plhiv_num", age = "Y000_014", sex = "male")


map_naomi <- function(indic, sex_param, age_param) {
  
  #indic <- "unaware_plhiv_num"
  # indic <- 'aware_plhiv_prop'
  
  # indic <- indicator
  #
  age <- age_param # 15+
  # period <- "2022-4" #dont change
  sex <- sex_param
  # area_level <- "3"
  
  df_all <- purrr::map_dfr(.x = "MOZ",
                           .f = ~ get_data_from_remote(period = "2022-4", country= .x, indicator = indic,
                                                       age = age, sex = sex)
  )
  
  df_all <-  df_all %>%
    mutate(sex = sex_param,
           age = age_param)
  
  return(df_all)
}

age_list <- c("Y000_014", "Y000_014", "Y000_014",
              "Y015_024", "Y015_024", "Y015_024",
              "Y025_034", "Y025_034", "Y025_034",
              "Y035_049", "Y035_049", "Y035_049")
gend_list <- c("male", "female", "both",
               "male", "female", "both",
               "male", "female", "both",
               "male", "female", "both")

df_final_num <- map2_dfr(.x = gend_list, .y = age_list,
                         .f = ~map_naomi(indic = c("aware_plhiv_num"),
                                         sex_param = .x, age_param = .y))


# get_data_from_remote <- function(period, country, indicator, age, sex) {
#   url <- paste0("https://naomi2023.azurewebsites.net/api/v1/data?period=",
#                 period,
#                 "&country=",
#                 country, 
#                 "&areaLevel=0&areaLevel=1&areaLevel=2&areaLevel=3", 
#                 "&indicator=", 
#                 indicator, 
#                 "&ageGroup=", 
#                 age, 
#                 "&sex=", 
#                 sex)
#   print(url)
#   data_pull <- read.csv(as.character(url)) %>%
#     mutate(country = country) %>%
#     mutate(indicator = indicator) %>%
#     mutate(age = age) %>%
#     mutate(sex = sex)
#   
#   return(data_pull)
# }
# 
# #get_data_from_remote(period = "2022-4", country = "MOZ", indicator = "aware_plhiv_num", age = "Y000_014", sex = "male")
# 
# 
# map_naomi <- function(indic, sex_param, age_param) {
#   
#   #indic <- "unaware_plhiv_num"
#   # indic <- 'aware_plhiv_prop'
#   
#   # indic <- indicator
#   #
#   age <- age_param # 15+
#   # period <- "2022-4" #dont change
#   sex <- sex_param
#   # area_level <- "3"
#   
#   df_all <- purrr::map_dfr(.x = "MOZ",
#                            .f = ~ get_data_from_remote(period = "2022-4", country= .x, indicator = indic,
#                                                        age = age, sex = sex)
#   )
#   
#   df_all <-  df_all %>%
#     mutate(sex = sex_param,
#            age = age_param)
#   
#   return(df_all)
# }
# 
# age_list <- c("Y000_014", "Y000_014", "Y000_014",
#               "Y015_024", "Y015_024", "Y015_024",
#               "Y025_034", "Y025_034", "Y025_034",
#               "Y035_049", "Y035_049", "Y035_049")
# gend_list <- c("male", "female", "both",
#                "male", "female", "both",
#                "male", "female", "both",
#                "male", "female", "both")
# 
# df_final_num <- map2_dfr(.x = gend_list, .y = age_list,
#                          .f = ~map_naomi(indic = c("aware_plhiv_num"),
#                                          sex_param = .x, age_param = .y))




# GLOBAL VARIABLES --------------------------------------------------------

  data_folder <- "Data"
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "998c2d49"

# IMPORT ------------------------------------------------------------------
  
  #from JR
df_naomi <- data_folder %>% 
    return_latest("naomi") %>% 
    read_csv() %>% 
    filter(str_detect(code, "MOZ"))
  
  
  df_final_num <- map2_dfr(.x = gend_list, .y = age_list,
                           .f = ~map_naomi(indic = c("aware_plhiv_num"),
                                           sex_param = .x, age_param = .y))
  
  df_final_prop <- map2_dfr(.x = gend_list, .y = age_list,
                            .f = ~map_naomi(indic = c("aware_plhiv_prop"),
                                            sex_param = .x, age_param = .y))
  
  df_final_plhiv <- map2_dfr(.x = gend_list, .y = age_list,
                             .f = ~map_naomi(indic = c("plhiv"),
                                             sex_param = .x, age_param = .y))
  
  df_first_95 <-   bind_rows(df_final_num, df_final_prop, df_final_plhiv) %>% 
    filter(indicator %in% c("plhiv", "aware_plhiv_num", "aware_plhiv_prop"),
           level %in% c(0,1)) %>% 
    mutate(age = str_remove(age, "Y0"),
           age = str_replace(age, "_0", "-")) %>% 
    mutate(age = ifelse(age =="15_999", "15+", age))

# COMPARE 1ST 95 BY AGE/SEX -------------------------------------------------------------------
  
 # df_first_95 <- df_naomi %>% 
 #    filter(indicator %in% c("plhiv", "aware_plhiv_num", "aware_plhiv_prop"),
 #           areaLevel %in% c(0,1)) %>% 
 #    mutate(age_group = str_remove(age_group, "Y0"),
 #           age_group = str_replace(age_group, "_0", "-")) %>% 
 #    mutate(age_group = ifelse(age_group =="15_999", "15+", age_group))
  
  
  df_first_95_viz <- df_first_95 %>% 
   # count(age_group)
    filter(sex %ni% "both") %>% 
    select(-c("lower", "upper")) %>% 
    pivot_wider(names_from = "indicator", values_from = "mean", values_fn = min) %>% 
    mutate(plhiv_marker = case_when(aware_plhiv_num > plhiv ~ plhiv),
           fill_color = case_when(sex == "male" & level != 0~ hw_hunter,
                                  sex == "female" & level!= 0 ~ hw_lavender_haze,
                                  sex == "female" & level == 0 ~ "#946297",
                                  sex == "male" & level == 0 ~ "#347450"))  %>% 
    group_by(area) %>% 
    mutate(snu_name = glue("{area}<br>{label_number(accuracy = 0.1, scale_cut = scales::cut_short_scale())(sum(aware_plhiv_num, na.rm = TRUE))}/{label_number(accuracy = 0.1, scale_cut = scales::cut_short_scale())(sum(plhiv, na.rm = TRUE))}")) %>% 
    ungroup()
  
    df_first_95_viz %>% 
      mutate(opacity = ifelse(level == 0, 1, 0.6)) %>% 
   # mutate(fill_color = ifelse(sex == "male", hw_hunter, hw_lavender_haze)) %>% 
    ggplot(aes(plhiv, age, fill = fill_color, color = fill_color, alpha = opacity)) +
    geom_blank(aes(plhiv*1.1)) +
    geom_col(fill = NA, width = .8, alpha = .8) +
    geom_col(aes(aware_plhiv_num), width = .8, alpha = .8) +
    geom_errorbar(aes(xmin = plhiv_marker, xmax = plhiv_marker),
                  na.rm = TRUE, color = "white", linetype = "dotted") +
    geom_text(aes(label = percent(aware_plhiv_prop, 1)), na.rm = TRUE,
              family = "Source Sans Pro", color = "black",
              size = 10/.pt, hjust = -.2) +
    facet_grid(sex ~ fct_reorder(snu_name, plhiv, sum, na.rm = TRUE, .desc = TRUE),
               switch = "y", scales = "free_x"
    ) +
      scale_x_continuous(label = label_number(scale_cut = cut_short_scale()),  expand = c(.005, .005)) +
      
    scale_fill_identity(aesthetics = c("fill", "color")) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL,
         title = "The largest gaps to the first 95" %>% toupper,
         subtitle = "PLHIV Aware / PLHIV, Ages 0-49",
         caption = glue("Source: Naomi Spectrum Estimates - 2022Q4 | Ref id: {ref_id}")) +
    coord_cartesian(clip = "off") +
    si_style_xgrid() +
    theme(strip.text.x = element_markdown())
    
    si_save("Graphics/01_1st95.svg")
# 1ST 95 MAP ----------------------------------------------------------------------
    
   df_art_age <- df_naomi %>% 
      filter(indicator == "art_coverage",
             areaLevel %in% c(0,1),
             sex != "both") %>% 
      select(name, areaLevel, mean, indicator, age_group, period, sex) %>% 
      mutate(age = str_remove(age_group, "Y0"),
             age = str_replace(age, "_0", "-")) %>% 
      mutate(age = ifelse(age =="15_999", "15+", age)) %>% 
      mutate(fill_color = case_when(sex == "male" & areaLevel != 0~ hw_hunter,
                                    sex == "female" & areaLevel!= 0 ~ hw_lavender_haze,
                                    sex == "female" & areaLevel == 0 ~ "#946297",
                                    sex == "male" & areaLevel == 0 ~ "#347450"))
    
  
    df_art_age %>% 
      filter(age %ni% c("15+", "15-49")) %>% 
      ggplot(aes(mean, age, fill = fill_color, color = fill_color)) +
      #geom_blank(aes(plhiv*1.1)) +
      geom_col(width = .8, alpha = 1) +
      geom_text(aes(label = percent(mean, 1)), na.rm = TRUE,
                family = "Source Sans Pro", color = "black",
                size = 10/.pt, hjust = -.2) +
      facet_grid(sex ~ fct_reorder(name, mean, sum, na.rm = TRUE, .desc = FALSE),
                 switch = "y", scales = "free_x"
      ) +
      scale_x_continuous(label = percent,  expand = c(.005, .005)) +
      
      scale_fill_identity(aesthetics = c("fill", "color")) +
      #scale_alpha_identity() +
      labs(x = NULL, y = NULL,
           title = "ART Coverage Gaps by Age/Sex/Province" %>% toupper,
           subtitle = "Ages 0-49",
           caption = glue("Source: Naomi Spectrum Estimates - 2022Q4 | Ref id: {ref_id}")) +
      coord_cartesian(clip = "off") +
      si_style_nolines() +
      theme(strip.text.x = element_markdown(),
            axis.text.x = element_blank())
      
      
      
      
      #geom_blank(aes(y = mean * 1.08)) +
      geom_col()
      
      geom_linerange(aes(xmin = gap_bar, xmax = goal), color = "gray90",
                     size = 2.5, na.rm = TRUE) +
      geom_point(size = 4, na.rm = TRUE) 
      
      geom_segment(aes(xend = mean, yend = age), linewidth = .9) +
      geom_point(size = 3) 
      
      geom_h
     # geom_blank(aes(plhiv*1.1)) +
      geom_col(fill = NA, width = .8, alpha = .8)
      
    
