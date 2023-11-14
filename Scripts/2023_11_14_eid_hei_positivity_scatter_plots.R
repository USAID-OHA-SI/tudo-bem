# PROJECT: Analysis of early infant diagnosis
# PURPOSE: Munge and Analysis of Moz EID data
# AUTHOR:  Tim Esssam | SI \ K. Srikanth
# REF ID:  8790175c
# LICENSE: MIT
# DATE:   2023-11-14
# NOTES:     

# LOCALS & SETUP ============================================================================
http://127.0.0.1:29739/graphics/plot_zoom_png?width=1459&height=898
  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(readxl)
    
    
  # SI specific paths/functions  
    file_path <- "Data/USAID Moz by PSNU_EID coverage and HEI positivity_11.13.23_v2.xlsx"
  
  # REF ID for plots
    ref_id <- "8790175c"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df <- read_excel(file_path, sheet = "EID", skip = 3) %>% 
      janitor::clean_names() %>% 
      select(-c(x8, x9))

# MUNGE ============================================================================
  
 df %>% names()  
  
# VIZ ============================================================================

  # Base plot  
    df %>% 
      ggplot(aes(x = eid_testing_coverage_2m, y = proxy_pos_rate_hei_pos_2_months)) +
      geom_rect(aes(xmin = 1, xmax = Inf, ymin = 0, ymax = Inf), fill = grey10k, alpha = 0.05) +
      geom_vline(xintercept = 1, linetype = "dotted", linewidth = .5, color = grey80k) +
      geom_hline(yintercept = 0, linetype = "solid", linewidth = .5, color = grey80k) +
      geom_point(color = scooter, size = 3, alpha = .85) +
      geom_point(shape = 1, size = 3, color = "black") +
      ggrepel::geom_text_repel(aes(label = psnu), size = 8/.pt, family = "Source Sans Pro") +
      scale_x_continuous(labels = percent, lim = c(.5, 1.3), breaks = seq(.5, 1.3, .1)) +
      scale_y_continuous(labels = percent, lim = c(-0.0, 0.05)) +
      si_style() +
      labs(x = "EID 2 mo testing coverage", y = "HEI positivity by 2 mo of age", 
           title = "Relationship between EID 2 mo coverage and proxy positivity by PSNU",
           subtitle = "USAID Mozambique FY23Q3 Cumulative",
           caption = "Source: USAID Tableau Server XX Workbook, 2023-11-13")
    si_save("Images/MZB_eid_testing_hei_pos_scatter_base.png")
    


# REQUEST 1: diff sized bubbles -------------------------------------------

    # Request: different sized bubbles based on number of HEI_POS (N) by 2 mo of age 
    df %>% 
      ggplot(aes(x = eid_testing_coverage_2m, y = proxy_pos_rate_hei_pos_2_months, )) +
      geom_rect(aes(xmin = 1, xmax = Inf, ymin = 0, ymax = Inf), fill = grey10k, alpha = 0.05) +
      geom_vline(xintercept = 1, linetype = "dotted", linewidth = .5, color = grey80k) +
      geom_hline(yintercept = 0, linetype = "solid", linewidth = .5, color = grey80k) +
      geom_point(aes(size = hei_pos_2m), color = scooter, alpha = .85) +
      geom_point(aes(size = hei_pos_2m), shape = 1, color = "black") +
      ggrepel::geom_text_repel(aes(label = psnu), size = 8/.pt, family = "Source Sans Pro") +
      scale_x_continuous(labels = percent, lim = c(.5, 1.3), breaks = seq(.5, 1.3, .1)) +
      scale_y_continuous(labels = percent, lim = c(-0.0, 0.05)) +
      si_style() +
      scale_color_si(palette = "scooters") +
      labs(x = "EID 2 mo testing coverage", y = "HEI positivity by 2 mo of age", 
           title = "Relationship between EID 2 mo coverage and proxy positivity by PSNU",
           subtitle = "USAID Mozambique FY23Q3 Cumulative",
           caption = "Source: USAID Tableau Server XX Workbook, 2023-11-13") +
      guides(size = guide_legend(title = "HEI Positive (numerator) 2 months of age"))
    si_save("Images/MZB_eid_testing_hei_pos_scatter_plot1.png")
    
    
    # Request: different sized bubbles based on number of HEI_POS (N) by 2 mo of age 
    df %>% 
      mutate(hei_pos_group = case_when(
        hei_pos_2m < 10 ~ "Fewer than 10 HEI Positives",
        hei_pos_2m >=10  ~ "More than 10",
      )) %>% 
      filter(!is.na(hei_pos_2m)) %>% 
      ggplot(aes(x = eid_testing_coverage_2m, y = proxy_pos_rate_hei_pos_2_months, )) +
      geom_rect(aes(xmin = 1, xmax = Inf, ymin = 0, ymax = Inf), fill = grey10k, alpha = 0.05) +
      geom_vline(xintercept = 1, linetype = "dotted", linewidth = .5, color = grey80k) +
      geom_hline(yintercept = 0, linetype = "solid", linewidth = .5, color = grey80k) +
      geom_point(aes(size = hei_pos_2m), color = scooter, alpha = .85) +
      geom_point(aes(size = hei_pos_2m), shape = 1, color = "black") +
      ggrepel::geom_text_repel(aes(label = psnu), size = 8/.pt, family = "Source Sans Pro") +
      scale_x_continuous(labels = percent, lim = c(.5, 1.3), breaks = seq(.5, 1.3, .1)) +
      scale_y_continuous(labels = percent, lim = c(-0.0, 0.05)) +
      si_style() +
      facet_wrap(~hei_pos_group, drop = T) +
      scale_color_si(palette = "scooters") +
      labs(x = "EID 2 mo testing coverage", y = "HEI positivity by 2 mo of age", 
           title = "Relationship between EID 2 mo coverage and proxy positivity by PSNU",
           subtitle = "USAID Mozambique FY23Q3 Cumulative",
           caption = "Source: USAID Tableau Server XX Workbook, 2023-11-13") +
      guides(size = guide_legend(title = "HEI Positive (numerator) 2 months of age"))
    si_save("Images/MZB_eid_testing_hei_pos_scatter_plot1_alt.png")
    

# REQUEST 2: different sized bubbles based on number of HEI_POS (N --------
    
    # Request 2: different sized bubbles based on number of HEI_POS (N) by 2 mo of age AND 
    # different color based on OVC PSNU footprint (please see column Q and tab for source data)
    
    df %>% 
      mutate(ovc_color = case_when(
        is.na(ovc_defined_by_ovc_serv_fy23_results) ~ golden_sand,
        TRUE ~ denim
      )) %>% 
      filter(!is.na(hei_pos_2m)) %>% 
      ggplot(aes(x = eid_testing_coverage_2m, y = proxy_pos_rate_hei_pos_2_months, color = ovc_color)) +
      geom_rect(aes(xmin = 1, xmax = Inf, ymin = 0, ymax = Inf), color = grey10k, fill = grey10k, alpha = 0.05) +
      geom_vline(xintercept = 1, linetype = "dotted", linewidth = .5, color = grey80k) +
      geom_hline(yintercept = 0, linetype = "solid", linewidth = .5, color = grey80k) +
      geom_point(aes(size = hei_pos_2m, color = ovc_color), alpha = .85) +
      geom_point(aes(size = hei_pos_2m), shape = 1, color = "black") +
      ggrepel::geom_text_repel(aes(label = psnu, color = ifelse(ovc_color == denim, denim, grey90k)), size = 8/.pt, family = "Source Sans Pro") +
      scale_x_continuous(labels = percent, lim = c(.5, 1.3), breaks = seq(.5, 1.3, .1)) +
      scale_y_continuous(labels = percent, lim = c(-0.0, 0.05)) +
      si_style() +
      scale_color_identity() +
      labs(x = "EID 2 mo testing coverage", y = "HEI positivity by 2 mo of age", 
           title = "Relationship between EID 2 mo coverage and proxy positivity by PSNU",
           subtitle = "USAID Mozambique FY23Q3 Cumulative | OVC Supported PSNUs in Blue",
           caption = "Source: USAID Tableau Server XX Workbook, 2023-11-13") +
      guides(size = guide_legend(title = "HEI Positive (numerator) 2 months of age"))
    si_save("Images/MZB_eid_testing_hei_pos_scatter_plot2.png")
    
    
    df %>% 
      mutate(ovc_color = case_when(
        is.na(ovc_defined_by_ovc_serv_fy23_results) ~ golden_sand,
        TRUE ~ denim
      ), 
      ovc_label = case_when(
        is.na(ovc_defined_by_ovc_serv_fy23_results) ~ "Non-OVC Supported PSNU",
        TRUE ~ "OVC Supported PSNU"
      )) %>% 
      filter(!is.na(hei_pos_2m)) %>% 
      ggplot(aes(x = eid_testing_coverage_2m, y = proxy_pos_rate_hei_pos_2_months, color = ovc_color)) +
      geom_rect(aes(xmin = 1, xmax = Inf, ymin = 0, ymax = Inf), color = grey10k, fill = grey10k, alpha = 0.05) +
      geom_vline(xintercept = 1, linetype = "dotted", linewidth = .5, color = grey80k) +
      geom_hline(yintercept = 0, linetype = "solid", linewidth = .5, color = grey80k) +
      geom_point(aes(size = hei_pos_2m, color = ovc_color), alpha = .85) +
      geom_point(aes(size = hei_pos_2m), shape = 1, color = "black") +
      ggrepel::geom_text_repel(aes(label = psnu, color = ifelse(ovc_color == denim, denim, grey90k)), size = 8/.pt, family = "Source Sans Pro") +
      scale_x_continuous(labels = percent, lim = c(.5, 1.3), breaks = seq(.5, 1.3, .1)) +
      scale_y_continuous(labels = percent, lim = c(-0.0, 0.05)) +
      si_style() +
      facet_wrap(~ovc_label) +
      scale_color_identity() +
      labs(x = "EID 2 mo testing coverage", y = "HEI positivity by 2 mo of age", 
           title = "Relationship between EID 2 mo coverage and proxy positivity by PSNU",
           subtitle = "USAID Mozambique FY23Q3 Cumulative | OVC Supported PSNUs in Blue",
           caption = "Source: USAID Tableau Server XX Workbook, 2023-11-13") +
      guides(size = guide_legend(title = "HEI Positive (numerator) 2 months of age"))
    si_save("Images/MZB_eid_testing_hei_pos_scatter_plot2_alt.png")

# REQUEST 3: View 3: different sized bubbles based on number of HEI_POS (N) by 2 mo of age AND different color based on KP PSNU footprint  -----
    
    df %>% 
      mutate(kp_color = case_when(
        is.na(kp_defined_by_kp_prev_fy23_results) ~ "#5bb5d5",
        TRUE ~ "#d57b5b"
        )
      ) %>%  
      filter(!is.na(hei_pos_2m)) %>% 
      ggplot(aes(x = eid_testing_coverage_2m, y = proxy_pos_rate_hei_pos_2_months, color = ovc_color)) +
      geom_rect(aes(xmin = 1, xmax = Inf, ymin = 0, ymax = Inf), color = grey10k, fill = grey10k, alpha = 0.05) +
      geom_vline(xintercept = 1, linetype = "dotted", linewidth = .5, color = grey80k) +
      geom_hline(yintercept = 0, linetype = "solid", linewidth = .5, color = grey80k) +
      geom_point(aes(size = hei_pos_2m, color = kp_color), alpha = .85) +
      geom_point(aes(size = hei_pos_2m), shape = 1, color = "black") +
      ggrepel::geom_text_repel(aes(label = psnu, color = ifelse(kp_color == denim, denim, grey90k)), size = 8/.pt, family = "Source Sans Pro") +
      scale_x_continuous(labels = percent, lim = c(.5, 1.3), breaks = seq(.5, 1.3, .1)) +
      scale_y_continuous(labels = percent, lim = c(-0.0, 0.05)) +
      si_style() +
      scale_color_identity() +
      labs(x = "EID 2 mo testing coverage", y = "HEI positivity by 2 mo of age", 
           title = "Relationship between EID 2 mo coverage and proxy positivity by PSNU",
           subtitle = "USAID Mozambique FY23Q3 Cumulative | KP Supported PSNU in Orange",
           caption = "Source: USAID Tableau Server XX Workbook, 2023-11-13") +
      guides(size = guide_legend(title = "HEI Positive (numerator) 2 months of age"))
    si_save("Images/MZB_eid_testing_hei_pos_scatter_plot3.png")
    
    df %>% 
      mutate(kp_color = case_when(
        is.na(kp_defined_by_kp_prev_fy23_results) ~ "#5bb5d5",
        TRUE ~ "#d57b5b"
      ), 
      kp_label = case_when(
        is.na(kp_defined_by_kp_prev_fy23_results) ~ "Non-KP Supported PSNU",
        TRUE ~ "KP Supported PSNU"
      )) %>% 
      filter(!is.na(hei_pos_2m)) %>% 
      ggplot(aes(x = eid_testing_coverage_2m, y = proxy_pos_rate_hei_pos_2_months, color = ovc_color)) +
      geom_rect(aes(xmin = 1, xmax = Inf, ymin = 0, ymax = Inf), color = grey10k, fill = grey10k, alpha = 0.05) +
      geom_vline(xintercept = 1, linetype = "dotted", linewidth = .5, color = grey80k) +
      geom_hline(yintercept = 0, linetype = "solid", linewidth = .5, color = grey80k) +
      geom_point(aes(size = hei_pos_2m, color = kp_color), alpha = .85) +
      geom_point(aes(size = hei_pos_2m), shape = 1, color = "black") +
      ggrepel::geom_text_repel(aes(label = psnu, color = ifelse(kp_color == denim, denim, grey90k)), size = 8/.pt, family = "Source Sans Pro") +
      scale_x_continuous(labels = percent, lim = c(.5, 1.3), breaks = seq(.5, 1.3, .1)) +
      scale_y_continuous(labels = percent, lim = c(-0.0, 0.05)) +
      si_style() +
      facet_wrap(~kp_label) +
      scale_color_identity() +
      labs(x = "EID 2 mo testing coverage", y = "HEI positivity by 2 mo of age", 
           title = "Relationship between EID 2 mo coverage and proxy positivity by PSNU",
           subtitle = "USAID Mozambique FY23Q3 Cumulative | KP Supported PSNU in Orange",
           caption = "Source: USAID Tableau Server XX Workbook, 2023-11-13") +
      guides(size = guide_legend(title = "HEI Positive (numerator) 2 months of age"))
    si_save("Images/MZB_eid_testing_hei_pos_scatter_plot3_alt.png")

