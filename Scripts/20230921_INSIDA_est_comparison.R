#VIZ FOR INSIDA 2021

df_prev_gap <- chapter_6_demo %>% 
  filter(type == "Province",
         age == "15+",
         # sex == "Total",
         indicator == "Percent HIV Positive") %>% 
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
  # geom_hline(yintercept = seq(from = 0, 
  #                             to = gap_max, 
  #                             by = gap_step),
  #            linewidth = .8, linetype = "dashed", color = grey20k) +
  # geom_vline(xintercept = ref_psnu,
  #            linewidth = .8, linetype = "dashed", color = usaid_darkgrey) +
  geom_segment(aes(xend = reorder(characteristic, female),
                   y = female, 
                   yend = male,
                   color = color_gap),
               linewidth = 2) +
  geom_point(shape = 21, size = 5, color = grey10k) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(labels = percent, position = "right") +
  coord_flip() +
  # labs(x = "", y = "", 
  #      title = {q},
  #      subtitle = glue::glue("{toupper(unique(df$country))} - {unique(df$fiscal_year)} HIV Prevalence Gap between <span style='color:{genoa}'>Male</span> & <span style='color:{moody_blue}'>Female</span> by PSNU"),
  #      caption = glue::glue("{cap_note}{metadata_natsubnat$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
  si_style_nolines() +
  theme(plot.subtitle = element_markdown(),
        axis.text.y = element_markdown())



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
