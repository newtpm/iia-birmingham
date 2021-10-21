fn_easy_gt <- function(data,
                    gt_title = NULL, gt_subtitle = NULL,
                    num_col_pattern = NULL, num_decimal = 0,
                    curr_col_pattern = NULL, curr_decimal = 0, curr_rows = TRUE,
                    perc_col_pattern = NULL, perc_decimal = 1, perc_rows = TRUE,
                    fx = "USD", row_striping = TRUE,
                    border_cols = NULL, border_weight = 1.5,
                    col_name_weight = "normal",
                    color_theme_list = NULL) {
  
  # Some data prep_________________________________
  
  total_col <- names(data)[1]
  
  if(is.null(color_theme_list)) { # default is the the grant mgt theme
    color_theme_list <-  list(
      bg_col = "#0B1924",
      text_col = "#B3E7DF", # text = fg
      sec_col = "#324043"
    )
  }
  
  
  # Start on the numeric formats________________________
  
  gt1 <- data %>%
    gt()
  
  if(!is.null(num_col_pattern)){
    gt1 <- gt1 %>%
      fmt_number(
        columns = contains(num_col_pattern),
        decimals = num_decimal,
        use_seps = TRUE
      )
  }
  
  if(!is.null(curr_col_pattern)){
    gt1 <- gt1 %>%
      fmt_currency(
        columns = contains(curr_col_pattern),
        rows = curr_rows,
        decimals = curr_decimal,
        currency = fx
      )
  }
  
  if(!is.null(perc_col_pattern)){
    gt1 <- gt1 %>%
      fmt_percent(
        columns = contains(perc_col_pattern),
        rows = perc_rows,
        decimals = perc_decimal
      )
  }
  
  # Generic formatting____________________
  
  gt2 <- gt1 %>%
    tab_header(title = gt_title,
                   subtitle = gt_subtitle) %>%
    tab_style(
      style = list(
        cell_fill(color = color_theme_list$sec_col),
        cell_text(color = "white",
                  weight = NULL)
      ),
      locations = cells_body(
        rows = .data[[total_col]] == "TOTAL")
    ) %>%
    tab_options(
      table.font.color.light = color_theme_list$text_col,
      table.background.color = color_theme_list$bg_col,
      row.striping.include_table_body = row_striping,
      table.border.top.color = "white",
      table.border.top.width = 3,
      column_labels.background.color = color_theme_list$sec_col,
      column_labels.border.top.width = 1,
      column_labels.border.top.color = "white",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "white"
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = col_name_weight,
                  color = color_theme_list$text_col) ),
      locations = cells_column_labels(everything())
    )
  
  
  for (i in border_cols) {
    if(i %in% names(data)) {
      gt2 <- gt2 %>%
        tab_style(
          style = list(
            cell_borders(
              sides = c("left"),
              color = "white",
              weight = px(border_weight)
            )
          ),
          locations = cells_body(
            columns = all_of(i)
          ))
    } }
  
  
  return(gt2)
}

