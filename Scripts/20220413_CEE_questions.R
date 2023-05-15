# PROJECT:  moz-support
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  CEE R loop
# LICENSE:  MIT
# DATE:     2021-04-13
# UPDATED:  
# Note: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(janitor)


# IMPORT ------------------------------------------------------------------

df <- si_path() %>% 
  return_latest("Inquerito_Customizado_-_Ciceros_Test") %>% 
  read_xlsx() 

answer <- si_path() %>% 
  return_latest("moz_CEE_sample_key") %>% 
  read_xlsx() 

#GSM_01_017 Question Block -----------------------------------------------

q_block <- "gsm_01_017"

df_gsm_0117 <- df %>%
  clean_names() %>% 
  select(1:7, contains(q_block)) %>% 
  rename(date = data_e_local_da_visita_data_da_visita, #confirm this
         snu = data_e_local_da_visita_provincia,
         psnu = data_e_local_da_visita_distrito,
         sitename = data_e_local_da_visita_us,
         question_block = contains("standard"),
         notes = contains("internal_notes")) %>%
  mutate(question_block = q_block) %>% 
  select(1:9, c(contains(c("does_the_hf_have_a_functional_art_committee_that_meets_2_times_a_month",
                         "all_sites_general_ticked",
                         "does_the_hf_have_a_community_representative_not_just_a_mentor_mother_participating_in_at_least_1_art_committee_session_per_month"
                         )))) %>% #only select relevant question columns
  rename(q1 = 10,
         q2 = 11,
         q3 = 12) %>% 
  mutate(q2 = as.character(q2)) %>% 
pivot_longer(cols = 10:12, names_to = "question", values_to = "answer")

df_gsm_0117 %>% 
  left_join(answer, by = c("question_block", "question", "answer")) %>%
 # view()
  group_by(sitename, question_block) %>% 
    slice_max(score, n = 1) %>% view()

#GSM_01_021 Question Block -----------------------------------------------

q_block <- "gsm_01_021"

df_gsm_0121 <- df %>%
  clean_names() %>% 
  select(1:7, contains(q_block)) %>% 
  rename(date = data_e_local_da_visita_data_da_visita, #confirm this
         snu = data_e_local_da_visita_provincia,
         psnu = data_e_local_da_visita_distrito,
         sitename = data_e_local_da_visita_us,
         question_block = contains("standard"),
         notes = contains("internal_notes")) %>%
  mutate(question_block = q_block) %>% 
  select(1:9, c(contains(c("qual_calculation_001"
  )))) %>% #only select relevant question columns
  #view()
  rename(q1 = 10) %>% 
 # mutate(q2 = as.character(q2)) %>% 
  pivot_longer(cols = 10, names_to = "question", values_to = "answer") 

#how to handle the joins for the continous variable? 

# GSM_04_012 ---------------------------------------------------

q_block <- "gsm_04_012"

df_gsm_04012 <- df %>%
  clean_names() %>% 
  select(1:7, contains(q_block)) %>% 
  rename(date = data_e_local_da_visita_data_da_visita, #confirm this
         snu = data_e_local_da_visita_provincia,
         psnu = data_e_local_da_visita_distrito,
         sitename = data_e_local_da_visita_us,
         question_block = contains("hei_standard"),
         notes = contains("internal_notes")) %>%
  mutate(question_block = q_block) %>% 
  select(1:9, c(contains(c("have_all_nurses_in_the_children_at_risk_service_ccr_been_trained",
                           "availability_of_reference_material_hei_ticked"
  )))) %>% #only select relevant question columns
  select(-11) %>% 
  rename(q1 = 10,
         q2 = 11) %>% 
  mutate(q2 = as.character(q2)) %>% 
  pivot_longer(cols = 10:11, names_to = "question", values_to = "answer")


df_gsm_04012 %>% 
  left_join(answer, by = c("question_block", "question", "answer")) %>%
  # view()
  group_by(sitename, question_block) %>% 
  slice_max(score, n = 1) 



# -------------------------------------------


# for (i in length(df_gsm_0117)){
#   if (q1 = "No") {
#     dftest <- df_gsm_0117 %>% 
#       mutate(score = "Red")
#   } else if (q2 <= 2) {
#     dftest <- df_gsm_0117 %>% 
#       mutate(score = "Red")
#   }
# }
# 
# df_gsm_0117 %>% 
#   mutate(score = ifelse(q1 == "No", "Red", NA),
#          score = ifelse(q2 <= 2, "Red", score)) %>% view()
