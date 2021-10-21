# User Functions for PCard Analysis 

# Pivot table

fn_annual_pivot <- function(data, column){ # we want to be able to specify the name of the column
  
  data %>%
    group_by({{column}}, Year) %>% #{{column}} is how R recognizes this is supposed to be a specified column name
    summarize(Amount = sum(Amount),
              .groups = "drop") %>%
    pivot_wider(names_from = Year, values_from = Amount, values_fill = 0) %>%
    arrange(desc(`2021`)) %>%
    select({{column}}, CY19 = `2019`, CY20 = `2020`, CY21 = `2021`)
  
}

fn_annual_filterpivot <- function(data, filter_col, filter_text, pivot_col){
  data %>%
    filter({{filter_col}} == filter_text) %>%
    fn_annual_pivot({{pivot_col}})
}


