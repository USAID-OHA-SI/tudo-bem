# PROJECT:  moz-cop22
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Review Approval Memo
# LICENSE:  MIT
# DATE:     2022-04-25
# UPDATED:  


#DEPENDENCIES ----------------------------------------------------------------------

library(tameDP)
library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(glue)
library(scales)
 

df_dp_agyw <- tame_dp(path, type="AGYW")

df_dp_kp_mat <- tame_dp(path, type="KP_MAT")

df_dp_full <- tame_dp(path, type="PSNUxIM")
df_dp_full <- get_names(df_dp_full, datim_user = datim_user(), datim_password = datim_pwd())


df_dp_clean<-df_dp_full %>% 
  filter(standardizeddisaggregate!="KeyPop") %>%
  mutate(relevant_value=if_else(indicator=="TX_TB", "N", numeratordenom)) %>%
  filter(relevant_value=="N",
         !is.na(ageasentered)
         ) %>% 
  mutate(age_agg=if_else(ageasentered %in% 
                           c("<01","01-04","05-09","10-14", "<15"),
                         "<15","15+")) %>%
  mutate(age_agg_18=if_else(ageasentered %in% c("18-20","18+"),"18+","<18"))

df_dp_table<-df_dp_clean %>% 
          filter(!str_detect(fundingagency, "Dedupe"),
                 # indicator == "OVC_SERV",
                 # standardizeddisaggregate == "Age/Sex/ProgramStatus"
                 ) %>% 
          group_by(indicator,fundingagency, age_agg) %>%
          summarize(targets=sum(targets, na.rm=T),.groups = "drop") %>%
          print()
View(df_dp_table)

#agyW

df_dp_agyw %>%
  filter(standardizeddisaggregate == "Age/Sex/DREAMSPrimaryComplete") %>% 
#  filter(!str_detect(fundingagency, "Dedupe")) %>%
  group_by(indicator,snuprioritization) %>%
  summarize(targets=sum(targets, na.rm=T),.groups = "drop") %>%
  print()
  

#OVC
df_ovc <- df_dp_full %>%
 # filter(standardizeddisaggregate!="KeyPop") %>%
 # mutate(relevant_value=if_else(indicator=="TX_TB", "N", numeratordenom)) %>%
  filter(
        # !is.na(ageasentered),
         indicator == "OVC_SERV",
         relevant_value=="N",
                !is.na(ageasentered)) %>% 
#filter(standardizeddisaggregate == "Age/Sex/Program") %>% 
  mutate(age_agg_18=if_else(ageasentered %in% 
                           c("<01","01-04","05-09","10-14", "15-17"),
                         "<18","18+"))
  
  df_dp_table_ovc<-df_ovc %>%
  #  filter(!str_detect(fundingagency, "Dedupe")) %>%
    group_by(indicator,snuprioritization, age_agg_18) %>%
    summarize(targets=sum(targets, na.rm=T),.groups = "drop") %>%
    print()
  View(df_dp_table_ovc)
  
#
          
df_dp_table %>%
         group_by(indicator, snuprioritization) %>%
         summarize(targets=sum(targets, na.rm=T),.groups = "drop") %>%
         View()



## Table one: 

mech_list<-c("160443","160451", "160449", "160452", "81776", "160453",
             "160457", "160459", "160454", "160460", '106101', "70212",
             "81790", "81791", "18103", "80032", "160466", "160467", '160472')

df_dp_crosstabs<-
           df_dp_clean %>% filter(mech_code %in% mech_list) %>%
           select(primepartner, mech_code, indicator, targets,fundingagency, age_agg) %>%
           group_by(primepartner, mech_code, indicator, fundingagency, age_agg) %>%
           #group_by(primepartner, mech_code, indicator) %>%
           summarize(targets=sum(targets, na.rm=T),.groups = "drop") %>%
           pivot_wider(names_from=c(indicator,age_agg), values_from=targets) %>%
           #pivot_wider(names_from=c(indicator), values_from=targets) %>%
           # select(primepartner, mech_code,`TX_NEW_<15`, `TX_NEW_15+` ,`TX_CURR_<15`, `TX_CURR_15+`,
           #        `TX_PVLS_<15`, `TX_PVLS_15+`)
           # select(primepartner, mech_code, `HTS_TST_<15`, `HTS_TST_15+`,
           #        `HTS_TST_POS_<15`,`HTS_TST_POS_15+`, `HTS_INDEX_<15`,`HTS_INDEX_15+`)
           select(primepartner, mech_code, `TB_PREV_<15`,`TB_PREV_15+`,
                 `TX_TB_<15`,`TX_TB_15+`)
           #select(primepartner, mech_code, `PREP_NEW`,`PREP_CT`, OVC_HIVSTAT, GEND_GBV)
           select(primepartner, mech_code, `PP_PREV_<15`,`PP_PREV_15+`)

View(df_dp_crosstabs)  
  
colSums(df_dp_crosstabs[,3:4],na.rm=TRUE)









     