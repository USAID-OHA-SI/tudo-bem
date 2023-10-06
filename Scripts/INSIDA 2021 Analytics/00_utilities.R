# Custom theme for tables
gt_theme_phc <- function (gt_object, ...) 
{
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in% 
              class(gt_object))
  
  gt_object %>%
    tab_options(
      heading.align = "left", 
      #column_labels.border.top.style = "none",
      table.border.top.style = "none", 
      #column_labels.border.bottom.style = "none",
      column_labels.border.bottom.width = 1, 
      column_labels.border.bottom.color = "#334422",
      #table_body.border.top.style = "none", 
      table_body.border.bottom.color = "white",
      #heading.border.bottom.style = "none", 
      data_row.padding = px(7),
      column_labels.font.size = px(12), ...
    ) %>%
    tab_style(
      style = cell_text(
        color = "darkgrey",
        font = google_font("Source Sans Pro"), transform = "uppercase"
      ),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_text(
        color = grey60k,
        weight = "600",
        font = google_font("Source Sans Pro"), transform = "uppercase"
      ),
      locations = cells_column_spanners(spanners = everything())
    ) %>%
    tab_style(style = cell_text(
      font = google_font("Libre Franklin"),
      weight = 800
    ), 
    locations = cells_title(groups = "title")) %>%
    tab_style(style = cell_text(
      font = google_font("Source Sans Pro"),
      weight = 400
    ), 
    locations = cells_body()
    # ) %>% 
    # tab_style(style = cell_text(
    #   font = google_font("Source Sans Pro"),
    #   weight = 400
    # ), 
    # locations = cells_summary()
    ) 
}


# Shrink size of rows in GT tables  
shrink_rows <- function(gt_obj){
  gt_obj %>% 
    tab_options(
      data_row.padding = px(1),
      row_group.padding = px(2),
      heading.padding = px(1)
    ) 
}  
