# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  PHIA data tables
# REF ID:   7f5da595 
# LICENSE:  MIT
# DATE:     2022-12-05
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  data_folder <- "Data/"
  
  ref_id <- "7f5da595"

# IMPORT ------------------------------------------------------------------
  
 file_path <-  data_folder %>% 
    return_latest("INSIDA 2021 Results Tables_PEPFAR")
  
 readxl::excel_sheets(file_path) 
 
 xl.read.file(file_path, password = "COP23.MZ", xl.sheet = "5.2", top.left.cell = "A3", na = "NA")
 
 

# 5.1 INCIDENCE TABLES ----------------------------------------------------------

 grab_gender <- function(gender) {
   
   df_5_1_names <- readxl::read_excel(file_path, sheet = "5.1", range = "A3:W11") %>% 
     names()
   
   if (gender == "Male"){
     gend <- df_5_1_names[2]
   } else if (gender == "Female") {
     gend <- df_5_1_names[5]
   } else if (gender == "Total") {
     gend <- df_5_1_names[8]
   }
  
   return(gend) 
 }
 
 munge_incidence_5_1 <- function(gender) {
   
   df_5_1 <- suppressMessages(
     readxl::read_excel(file_path, sheet = "5.1", range = "A4:W11")
   )
   
   
   if(gender == "Male") {
     df_munge <- df_5_1 %>% 
       select(1:3) %>% 
       rename(`95% CI` = `95% CI...3`)
   } else if (gender == "Female") {
     df_munge <- df_5_1 %>% 
       select(1, 5:6) %>% 
       rename(`95% CI` = `95% CI...6`)
   } else if (gender == "Total") {
     df_munge <- df_5_1 %>% 
       select(1, 8:9) %>% 
       rename(`95% CI` = `95% CI...9`)
   }
   
   conf_int <- str_split(df_munge$`95% CI`, ' - ', simplify = TRUE) %>% as_tibble() %>% 
     mutate_if(is.character, as.numeric) %>% 
     rename(lower_bound = V1,
            upper_bound = V2)
   
   gend <- grab_gender(gender)
   
   df_final <- df_munge %>% 
     mutate(sex = gend,
            indicator = "Percent Annual Incidence",
            source = "Table 5.1") %>% 
     rename_if(stringr::str_detect(names(.), "^Percent"), ~paste0("value")) %>% 
     cbind(conf_int) %>% 
     filter(!is.na(Age)) %>% 
     janitor::clean_names() %>% 
     select(indicator, sex, age, value, lower_bound, upper_bound,source)

   return(df_final)
 }
 
 df_male_5_1 <- munge_incidence_5_1("Male")
 df_female_5_1 <- munge_incidence_5_1("Female")
 df_total_5_1 <- munge_incidence_5_1("Total")
 
df_5_1_final <- bind_rows(df_male_5_1, df_female_5_1, df_total_5_1)

# 5.1 A --------------------------------------------------------------------

munge_5_1_A <- function(gender) {
  
  if(gender == "Male") {
    name_range <<- "A3:E11"
    df_range <<- "A4:E11"
  } else if (gender == "Female") {
    name_range <<- "A12:E20"
    df_range <<-"A13:E20"
  } else if (gender == "Total") {
    name_range <<- "A21:E29"
    df_range <<-"A22:E29"
  }
  
  df_5_1_A_names <- readxl::read_excel(file_path, sheet = "5.1.A", range = name_range) %>% 
    names()
  
  gend <- df_5_1_A_names[2]
  
  #read in 
  
 df_final <-  readxl::read_excel(file_path, sheet = "5.1.A", range = df_range) %>%
    mutate(sex = gend) %>% 
    pivot_longer(cols = -c("Age", "sex"), names_to = "indicator", values_to = "value") %>% 
    mutate(indicator = case_when(str_detect(indicator, "HIV negative") ~ "Number HIV negative",
                                 str_detect(indicator, "HIV positive") ~ "Number HIV positive",
                                 str_detect(indicator, "LAg assay1") ~ "Number tested on lag assay",
                                 str_detect(indicator, "HIV recent") ~ "Number HIV recent")) %>% 
    mutate(lower_bound = NA,
           upper_bound = NA,
           source = "Table 5.1.A") %>% 
    janitor::clean_names() %>% 
    select(indicator, sex, age, value, lower_bound, upper_bound, source)  
 
 return(df_final)
}

df_male_5_1_A <- munge_5_1_A("Male")
df_female_5_1_A <- munge_5_1_A("Female")
df_total_5_1_A <- munge_5_1_A("Total")

df_5_1_A_final <- bind_rows(df_male_5_1_A, df_female_5_1_A, df_total_5_1_A)

# 5.2 ---------------------------------------------------------------------

#confidence intervals not calculating
df_5_2_final <- readxl::read_excel(file_path, sheet = "5.2", range = "A3:F10") %>% 
  select(1:2, 5) %>%  #add in CIs if they calculate correctly
  mutate(`People living with HIV1` = str_remove_all(`People living with HIV1`, ","),
         `Number of new infections per year` = str_remove_all(`Number of new infections per year`, ",")) %>% 
  pivot_longer(cols = -c("Age"), names_to = "indicator", values_to = "value") %>% 
  filter(!is.na(Age)) %>% 
  mutate(sex = NA,
         lower_bound = NA,
         upper_bound = NA,
         indicator = case_when(indicator == "People living with HIV1" ~ "Number PLHIV", TRUE ~ indicator),
         value = as.numeric(value),
         source = "Table 5.2") %>% 
  janitor::clean_names() %>% 
  select(indicator, sex, age, value, lower_bound, upper_bound, source)  
  

# BIND ALL INCIDENCE TOGETHER
chapter_5_final <- bind_rows(df_5_1_final, df_5_1_A_final, df_5_2_final)

write_csv(chapter_5_final, "Dataout/PHIA_Ch5_Incidence_Tables.csv")

# 6.1 PREVALENCE TABLES ----------------------------------------------------------

#6.1 is adults 15-49, 6.2 is 15+
# MUST FILTER ON TYPE

get_df_ch6 <- function(gender, sheet_name) {

  df_6_1 <- suppressMessages(
    readxl::read_excel(file_path, sheet = sheet_name, range = "A4:I45")
  )
  
  if(gender == "Male") {
    df_munge <- df_6_1 %>% 
      select(1:3)
  } else if (gender == "Female") {
    df_munge <- df_6_1 %>% 
      select(1, 5:6)
  } else if (gender == "Total") {
    df_munge <- df_6_1 %>% 
      select(1, 8:9) 
  }
  
  return(df_munge)
}

munge_ch6 <- function(gender, sheet_name) {
  
 df <- get_df_ch6(gender, sheet_name)
 
 if (sheet_name %in% c("6.1", "6.2")) {
   
   select_vars <- c("type", "characteristic",
                    "indicator", "indic_type", "age", "sex", "value", "source")
   
   age_grab <- 
     filter(df, str_detect(Characteristic, "Total")) %>% 
     pull(Characteristic)
   
   age_band <-  str_split(age_grab, ' ', simplify = TRUE)[,2]
   
 df_tidy <- df %>% 
     mutate(type = case_when(Characteristic == "Residence" ~ "Residence",
                             Characteristic == "Province" ~ "Province",
                             Characteristic == "Marital status" ~ "Marital status",
                             Characteristic == "Education" ~ "Education",
                             Characteristic == "Wealth quintile" ~ "Wealth quintile",
                             Characteristic == "Pregnancy status" ~ "Pregnancy status",
                             str_detect(Characteristic, "Total") ~ "Total")) %>% 
     
     fill(type) %>% 
     janitor::clean_names() %>% 
     filter(type != characteristic) %>% 
   pivot_longer(cols = c(2:3), names_to = "indicator", values_to = "value") %>% 
     mutate(age = age_band)
 
 } else if (sheet_name == "6.3") {
   
   select_vars <- c("indicator", "indic_type", "age", "sex", "value", "source")
   
   df_tidy <- df %>% 
     slice_head(n = 16) %>% 
     janitor::clean_names() %>% 
     mutate(mutate(across(c(2:3), as.numeric))) %>% 
     pivot_longer(cols = c(2:3), names_to = "indicator", values_to = "value") %>%
     filter(!is.na(age))

 }
 
 df_final <- df_tidy %>% 
   mutate(value = str_remove_all(value, ","),
          value = as.numeric(value),
          source = glue("Table {sheet_name}"),
          sex = gend,
          indic_type = case_when(str_detect(indicator, "percentage") ~ "Percent",
                                 str_detect(indicator, "number") ~ "Number"),
          indicator = case_when(str_detect(indicator, "percentage") ~ "Percent HIV Positive",
                                str_detect(indicator, "number") ~ "Number HIV Positive")) %>% 
   select(c(select_vars))
 
 return(df_final)
  
}


chapter_6_demo <- bind_rows(
          munge_ch6("Male", "6.1"),
           munge_ch6("Male", "6.2"),
           munge_ch6("Female", "6.1"),
           munge_ch6("Female", "6.2"),
           munge_ch6("Total", "6.1"),
           munge_ch6("Total", "6.2"),
          ) %>% 
  mutate(description = "HIV prevalence by demographic characteristics")

chapter_6_age_sex <- bind_rows(
  munge_ch6("Male", "6.3"),
  munge_ch6("Female", "6.3"),
  munge_ch6("Total", "6.3"),
) %>% 
  mutate(description = "HIV prevalence by age and sex")
  
  
 write_csv(chapter_6_demo, "Dataout/PHIA_Ch6_Prev_Demographics_Tables.csv")
 write_csv(chapter_6_age_sex, "Dataout/PHIA_Ch6_Prev_AgeSex_Tables.csv")
 
 # 9.1 95s TABLES ----------------------------------------------------------
 
 # 9.1.A, 9.1.B, 9.2.A, 9.2.B, (9.3.A, 9.3.B)
 
 munge_ch9 <- function(status, gender, sheet_name) {
   
   if(status == "Diagnosed") {
     name_range <<- "A3:I12"
     sex_range <<- "A4:I12"
     df_range <<- "A5:I12"
   } else if (status == "On Treatment") {
     name_range <<- "A13:I22"
     sex_range <<-"A14:I22"
     df_range <<-"A15:I22"
   } else if (status == "VLS") {
     name_range <<- "A23:I32"
     sex_range <<-"A24:I32"
     df_range <<-"A25:I32"
     
   }
   
   #grab status
   df_9_status <- readxl::read_excel(file_path, sheet = sheet_name, range = name_range) %>%
     names()
   
   status <- df_9_status[2]
   
   #grab gender
   gend <- grab_gender(gender)
   
   #read in 
   df_9_1_A <-  readxl::read_excel(file_path, sheet = sheet_name, range = df_range)
   
   if(gender == "Male") {
     df_munge <- df_9_1_A %>% 
       select(1:3) 
   } else if (gender == "Female") {
     df_munge <- df_9_1_A %>% 
       select(1, 5:6)
   } else if (gender == "Total") {
     df_munge <- df_9_1_A %>% 
       select(1, 8:9)
   }
   
   df_final <- df_munge %>% 
     mutate(sex = gend,
            indicator = status) %>% 
     mutate(mutate(across(c(2:3), as.numeric))) %>% 
     filter(!is.na(Age)) %>% 
     pivot_longer(cols = -c("Age", "sex", "indicator"), names_to = "indic_type", values_to = "value") %>% 
     mutate(indic_type = case_when(str_detect(indic_type, "Percentage") ~ "Percent",
                                   str_detect(indic_type, "Number") ~ "Number")) %>% 
     mutate(source = glue("Table {sheet_name}"),
            description = case_when(sheet_name == "9.1.A" ~ "Adult 95s overall percentages (self-reported and antiretroviral biomarker data)",
                                    sheet_name == "9.1.B" ~ "Adult 95s conditional percentages  (self-reported and antiretroviral biomarker data)",
                                    sheet_name == "9.2.A" ~ "Adult 95s overall percentages  (self-reported data adjusted for a viral load < 200 HIV RNA copies per milliliter)",
                                    sheet_name == "9.2.B" ~ "Adult 95s conditional percentages  (self-reported data adjusted for a viral load < 200 HIV RNA copies per milliliter)")) %>% 
     janitor::clean_names() %>% 
     select(indicator, indic_type, sex, age, value, source, description)  
   
   return(df_final)
 }

 status_param <- c("Diagnosed", "Diagnosed", "Diagnosed",
                   "On Treatment", "On Treatment","On Treatment",
                   "VLS", "VLS", "VLS",
                   "Diagnosed", "Diagnosed", "Diagnosed",
                   "On Treatment", "On Treatment","On Treatment",
                   "VLS", "VLS", "VLS",
                   "Diagnosed", "Diagnosed", "Diagnosed",
                   "On Treatment", "On Treatment","On Treatment",
                   "VLS", "VLS", "VLS",
                   "Diagnosed", "Diagnosed", "Diagnosed",
                   "On Treatment", "On Treatment","On Treatment",
                   "VLS", "VLS", "VLS")
 gender_param <- c("Male", "Female", "Total",
                   "Male", "Female", "Total",
                   "Male", "Female", "Total",
                   "Male", "Female", "Total",
                   "Male", "Female", "Total",
                   "Male", "Female", "Total",
                   "Male", "Female", "Total",
                   "Male", "Female", "Total",
                   "Male", "Female", "Total",
                   "Male", "Female", "Total",
                   "Male", "Female", "Total",
                   "Male", "Female", "Total")
 sheet_param <- c("9.1.A", "9.1.A", "9.1.A",
                  "9.1.A", "9.1.A", "9.1.A",
                  "9.1.A", "9.1.A", "9.1.A",
                  "9.1.B", "9.1.B", "9.1.B",
                  "9.1.B", "9.1.B", "9.1.B",
                  "9.1.B", "9.1.B", "9.1.B",
                  "9.2.A", "9.2.A", "9.2.A",
                  "9.2.A", "9.2.A", "9.2.A",
                  "9.2.A", "9.2.A", "9.2.A",
                  "9.2.B", "9.2.B", "9.2.B",
                  "9.2.B", "9.2.B", "9.2.B",
                  "9.2.B", "9.2.B", "9.2.B")
 
 chapter_9_final <- data.frame(status_param, gender_param, sheet_param) %>% 
   pmap_dfr(~munge_ch9(..1, ..2,..3))
 
 write_csv(chapter_9_final, "Dataout/PHIA_Ch9_95s_Tables.csv")
 
   
 
 # working code ------------------------------------------------------------
 
