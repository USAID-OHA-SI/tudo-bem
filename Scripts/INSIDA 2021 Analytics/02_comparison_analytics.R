# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  INSIDA / DHS comparison visuals
# REF ID:   be0fb189 
# LICENSE:  MIT
# DATE:     2023-09-30
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
library(gt)
library(gtExtras)


# GLOBAL VARIABLES --------------------------------------------------------

# SI specific paths/functions  
load_secrets()

ref_id <- "be0fb189"

data_folder <- "Data"
dataout_folder <- "Dataout"

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

source("Scripts/INSIDA 2021 Analytics/00_utilities") #run source manually if this doesnt work

# IMPORT ------------------------------------------------------------------

#demographic prevalance
chapter_6_demo <- dataout_folder %>% 
  return_latest("Ch6_Prev_Demographics") %>% 
  read_csv()

#age/sex prevalence
chapter_6_age_sex <- dataout_folder %>% 
  return_latest("Ch6_Prev_AgeSex") %>% 
  read_csv()

#self reported testing
ch7_total <- dataout_folder %>% 
  return_latest("Ch7_Testing") %>% 
  read_csv()

#dhs KIR data
df_dhs <- data_folder %>% 
  return_latest("dhs_2022_hiv_datatable") %>% 
  read_excel()

# PREVALENCE INSIDA 2021 ViZ --------------------------------------------------

df_prev_gap <- chapter_6_demo %>% 
  filter(type == "Province",
         age == "15+",
         # sex == "Total",
         indicator == "Percent HIV Positive") %>%
  mutate(value = value / 100) %>% 
  # select(-psnu_prev) %>% 
  mutate(sex = tolower(sex)) %>% 
  pivot_wider(names_from = sex,
              values_from = value) %>% 
  mutate(color_gap = grey30k)



chapter_6_demo %>% 
  filter(type == "Province",
         age == "15+",
          sex != "Total",
         indicator == "Percent HIV Positive") %>% 
  mutate(value = value / 100) %>% 
  left_join(df_prev_gap,
            by = c("type","characteristic", "indicator", 
                   "indic_type", "age", "source", "description")) %>% 
  mutate(
    color_sex = case_when(
      sex == "Female" ~ moody_blue,
      sex == "Male" ~ genoa,
      TRUE ~ grey30k
    )) %>% 
  ggplot(aes(x = reorder(characteristic, female), 
             y = value,
             fill = color_sex)) +
  geom_segment(aes(xend = reorder(characteristic, female),
                   y = female, 
                   yend = male,
                   color = color_gap),
               linewidth = 2) +
  geom_point(shape = 21, size = 5, color = grey10k) +
  ggplot2::geom_text(aes(label = percent(value, 1)), family = "Source Sans Pro", color = grey70k) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(labels = percent, position = "right") +
  coord_flip() +
  labs(
       title = "Percent HIV Positive by sex and Province" %>% toupper(),
       subtitle =  "INSIDA 2021 Estimates",
       x = NULL, y = NULL, fill = NULL,
       caption =
         glue(" INSIDA 2021 Estimates |  Ref id: ")) +
  si_style_nolines() +
  theme(plot.subtitle = element_markdown(),
        axis.text.y = element_markdown())

si_save("Graphics/06_pct_pos.svg")

# HIV prevalence by AGE

chapter_6_age_sex %>%
  filter(indicator == "Number HIV Positive",
         sex != "Total",
         !str_detect(age, "Total")) %>% 
  mutate(val_lab = clean_number(value)) %>% 
  dplyr::mutate(value = if_else(sex == "Male", -value, value)) %>% 
  ggplot2::ggplot(aes(value, age, fill = sex)) +
##  ggplot2::geom_blank(aes(axis_max)) +
#  ggplot2::geom_blank(aes(axis_min)) +
  ggplot2::geom_text(aes(label = val_lab), family = "Source Sans Pro", color = grey70k) +
  ggplot2::geom_col(alpha = .8, na.rm = TRUE) +
  ggplot2::geom_vline(aes(xintercept = 0), color = "white", linewidth = 1.1)+
#  ggplot2::facet_wrap(~facet_grp, scales = "free_x", nrow = f_nrow) +
  ggplot2::scale_fill_manual(values = c("Male" = glitr::genoa, 
                                        "Female" = glitr::moody_blue)) +
  ggplot2::scale_x_continuous(
    labels = function(x) {glue("{label_number(scale_cut = cut_short_scale())(abs(x))}")}, 
  ) +
  ggplot2::labs(title = "Prevalence of HIV among adults aged 15+ years by sex and age" %>% toupper(),
                subtitle =  "INSIDA 2021 Estimates",
                x = NULL, y = NULL, fill = NULL,
                caption =
                  glue(" INSIDA 2021 Estimates |  Ref id: ")) +
  glitr::si_style_yline() +
  ggplot2::theme(
    legend.position = "none",
    strip.text = element_text(hjust = .5),
    plot.subtitle = element_markdown(),
    panel.spacing = unit(.2, "picas"))

si_save("Graphics/06_adult_prev.svg")


# HIV TESTING TABLES AGE: MUNGE ---------------------------------------------------------------------

viz_ch7_age <- ch7_total %>% 
  mutate(across(c(2:4), as.numeric)) %>%
  mutate(`Percentage who had ever received an HIV test` = `Percentage who had ever received an HIV test`/100,
         `Percentage who received an HIV test in the 12 months before the survey`=`Percentage who received an HIV test in the 12 months before the survey`/100) %>% 
  #count(characteristic, type)
  filter(type == "Age",
         # sex == "Male",
         population == "Among all") 

viz_dhs_age <- df_dhs %>% 
  filter(type == "Age") %>% 
  select(-c(4:6)) %>% 
  rename(`DHS: Percentage who had ever received an HIV test` = `pct ever tested`,
         `DHS: Percentage who received an HIV test in the 12 months before the survey` = `pct tested in last 12 months and received results of last test`,
         `DHS: Total number tested` = `total number tested`,
         sex = gender) %>% 
  mutate(source = "DHS 2022", 
         population = "Among all") %>% 
  select(characteristic, `DHS: Percentage who had ever received an HIV test`, `DHS: Percentage who received an HIV test in the 12 months before the survey`,
         `DHS: Total number tested`, type, population, sex, source)

# VIZ ---

#INSIDA
viz_ch7_age %>% 
  select(-c(type, population)) %>% 
  gt() %>% 
  sub_missing(missing_text = ".",
  ) %>%  
  fmt_percent(columns = c(2:3),
              decimals = 0) %>%
  fmt_number(columns = c(4), 
             decimals = 0) %>% 
  cols_hide(sex) %>% 
  cols_hide(source) %>% 
  tab_row_group(
    label = "Male",
    rows = sex %in% c("Male")
  ) %>%
  tab_row_group(
    label = "Female",
    rows = sex %in% c("Female")
  ) %>% 
  cols_label(characteristic = "Age Bands") %>% 
  tab_options(
    source_notes.font.size = px(10)) %>% 
  tab_header(
    title = glue("INSIDA 2021: Self-reported HIV testing by age bands" %>% toupper())
  ) %>% 
  tab_source_note(
    source_note = gt::md(glue("INSIDA 2021 Results: Table 7.1"))) %>% 
  gt_theme_phc() %>% 
  shrink_rows() %>% 
  tab_style(
    style = list(
      cell_text(weight = 600)
    ),
    locations = cells_body(
      columns = c(1)
    )
  ) %>% 
  gt_color_rows(columns = c(2), na.color = "white", 
                palette = c("#f7f7f7", scooter_med)) %>% 
  gtsave_extra(filename = glue("Images/table2_hiv_testing_by_age_insida.png"))  

#DHS
viz_dhs_age %>% 
  select(-c(type, population)) %>% 
  gt() %>% 
  sub_missing(missing_text = ".",
  ) %>%  
  fmt_percent(columns = c(2:3),
              decimals = 0) %>%
  fmt_number(columns = c(4), 
             decimals = 0) %>% 
  cols_hide(sex) %>% 
  cols_hide(source) %>% 
  tab_row_group(
    label = "Male",
    rows = sex %in% c("Male")
  ) %>%
  tab_row_group(
    label = "Female",
    rows = sex %in% c("Female")
  ) %>% 
  cols_label(characteristic = "Age Bands") %>% 
  tab_options(
    source_notes.font.size = px(10)) %>% 
  tab_header(
    title = glue("DHS 2022: Self-reported HIV testing by age bands" %>% toupper())
  ) %>% 
  tab_source_note(
    source_note = gt::md(glue("DHS 2022 Estimate: Key Indicator Report"))) %>% 
  gt_theme_phc() %>% 
  shrink_rows() %>% 
  tab_style(
    style = list(
      cell_text(weight = 600)
    ),
    locations = cells_body(
      columns = c(1)
    )
  ) %>% 
  gt_color_rows(columns = c(2), na.color = "white", 
                palette = c("#f7f7f7", scooter_med)) %>% 
  gtsave_extra(filename = glue("Images/table2_hiv_testing_by_age_dhs.png"))  


# BOTH DHS AND INSIDA FOR AGE (only few age bands)
viz_ch7_age %>% 
  inner_join(viz_dhs_age, by = c("characteristic", "type", "population", "sex")) %>% 
  select(-c(type, population, source.x, source.y)) %>% 
  gt() %>% 
  sub_missing(missing_text = ".",
  ) %>%  
  fmt_percent(columns = c(2:3, 6:7),
              decimals = 0) %>%
  fmt_number(columns = c(4,8), 
             decimals = 0) %>% 
  cols_hide(sex) %>% 
  #cols_hide(source) %>% 
  tab_row_group(
    label = "Male",
    rows = sex %in% c("Male")
  ) %>%
  tab_row_group(
    label = "Female",
    rows = sex %in% c("Female")
  ) %>% 
  cols_label(characteristic = "Age Bands") %>% 
  tab_spanner(
    label = "INSIDA 2021 Estimates",
    columns = c(2:4)) %>% 
  tab_spanner(
    label = "DHS 2022 Estimates",
    columns = c(6:8)) %>% 
  tab_options(
    source_notes.font.size = px(10)) %>% 
  tab_header(
    title = glue("self-reported HIV testing by age bands and data source" %>% toupper())
  ) %>% 
  tab_source_note(
    source_note = gt::md(glue(""))) %>% 
  gt_theme_phc() %>% 
  shrink_rows() %>% 
  tab_style(
    style = list(
      cell_text(weight = 600)
    ),
    locations = cells_body(
      columns = c(1)
    )
  ) %>% 
  gt_color_rows(columns = c(2,6), na.color = "white", 
                palette = c("#f7f7f7", scooter_med)) %>% 
  gtsave_extra(filename = glue("Images/table2_hiv_testing_by_source.png"))  

# HIV TESTING TABLES province: MUNGE ---------------------------------------------------------------------

viz_ch7_province <- ch7_total %>% 
  mutate(across(c(2:4), as.numeric)) %>%
  mutate(`Percentage who had ever received an HIV test` = `Percentage who had ever received an HIV test`/100,
         `Percentage who received an HIV test in the 12 months before the survey`=`Percentage who received an HIV test in the 12 months before the survey`/100) %>% 
  filter(type == "Province",
         population == "Among all") %>% 
  mutate(characteristic = case_when(characteristic == "Zambézia" ~ "Zambezia",
                                    TRUE ~ characteristic))

viz_dhs_province <- df_dhs %>% 
  filter(type == "Province") %>% 
  select(-c(4:6)) %>% 
  rename(`DHS: Percentage who had ever received an HIV test` = `pct ever tested`,
         `DHS: Percentage who received an HIV test in the 12 months before the survey` = `pct tested in last 12 months and received results of last test`,
         `DHS: Total number tested` = `total number tested`,
         sex = gender) %>% 
  mutate(source = "DHS 2022", 
         population = "Among all") %>% 
  select(characteristic, `DHS: Percentage who had ever received an HIV test`, `DHS: Percentage who received an HIV test in the 12 months before the survey`,
         `DHS: Total number tested`, type, population, sex, source) %>% 
  mutate(characteristic = case_when(characteristic == "Imhambane" ~ "Inhambane",
                                    characteristic == "Maputo" ~ "Maputo Província",
                                    characteristic == "City of Maputo" ~ "Maputo Cidade",
                                    TRUE ~ characteristic))

#table with INSIDA and DHS
viz_ch7_province %>% 
  left_join(viz_dhs_province, by = c("characteristic", "type", 
                                     "sex", "population")) %>% 
  select(-c(type, population, source.x, source.y)) %>% 
  gt() %>% 
  sub_missing(missing_text = ".",
  ) %>%  
  fmt_percent(columns = c(2:3, 6:7),
              decimals = 0) %>%
  fmt_number(columns = c(4,8), 
             decimals = 0) %>% 
  cols_hide(sex) %>% 
  tab_row_group(
    label = "Male",
    rows = sex %in% c("Male")
  ) %>%
  tab_row_group(
    label = "Female",
    rows = sex %in% c("Female")
  ) %>% 
  tab_spanner(
    label = "INSIDA 2021 Estimates",
    columns = c(2:4)) %>% 
  tab_spanner(
    label = "DHS 2022 Estimates",
    columns = c(6:8)) %>% 
  tab_options(
    source_notes.font.size = px(10)) %>% 
  tab_header(
    title = glue("Self-reported HIV testing by Province and data source" %>% toupper())
  ) %>% 
  tab_source_note(
    source_note = gt::md(glue("INSIDA 2021 Results: Table 7.1 & Demographics and Health Survey 20223 Key Indicator Report"))) %>% 
  gt_theme_phc() %>% 
  shrink_rows() %>% 
  tab_style(
    style = list(
      cell_text(weight = 600)
    ),
    locations = cells_body(
      columns = c(1)
    )
  ) %>% 
  tab_options(
    source_notes.font.size = px(10),
    row_group.font.weight = "bold",
    data_row.padding = px(1),
    column_labels.font.size = px(15)) %>%
  gt_color_rows(columns = c(2,6), na.color = "white", 
                palette = c("#f7f7f7", golden_sand)) %>% 
  gtsave_extra(filename = glue("Images/table1_hiv_testing_by_source.png"))  


# 95s ----------------------------------------------------

chapter_9_final %>% 
  filter(description == "Adult 95s overall percentages (self-reported and antiretroviral biomarker data)")
