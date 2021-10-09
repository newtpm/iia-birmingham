
clean_merchants <- function(data) {
  
  removes <- c("www." = "", "ww " = "", "/" = "", "http:" = "", "-" = "", " #" = "", "#" = "",
               "tst\\* " = "", "Sp *" = "", "intuit \\*in \\*" = "")
  
  tmp <- data %>%
    filter(!is.na(ID)) %>%
    mutate(Merchant2 = str_to_lower(Merchant),
           Merchant2 = str_replace_all(Merchant2, removes),
           Merchant2 = ifelse(str_detect(Merchant2, "sq *"), str_replace(Merchant2, "sq \\*", ""), Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "in *"), str_replace(Merchant2, "in \\*", ""), Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "paypal *"), str_replace(Merchant2, "paypal \\*", ""), Merchant2),
           Merchant2 = str_remove_all(Merchant2, "[:digit:]"),
           Merchant2 = ifelse(str_detect(Merchant2, "uber"), "Uber", Merchant2),
           Merchant2 = ifelse(str_detect(str_to_lower(Merchant), "eats") & Merchant2 == "Uber", "Uber Eats", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "lyft"), "Lyft", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "google"), "Google", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "walmart"), "Walmart", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "wm supercenter"), "Walmart", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "amazon"), "Amazon", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "amzn"), "Amazon", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, " dell "), "Dell", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "amtrak"), "Amtrak", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "apl ") | str_detect(Merchant2, "apl\\*")  , "Apple", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "starbucks"), "Starbucks", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "shopify *"), "Shopify", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "airbnb"), "Airbnb", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "facebk"), "Facebook", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, " att ") | str_detect(Merchant2, "at&t"),
                              "AT&T", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, " att "),  "AT&T", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "vzwrlss") | str_detect(Merchant2, "verizon"),
                              "Verizon", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "staples"), "Staples", Merchant2),
           Merchant2 = ifelse(str_detect(Merchant2, "sysco"), "Sysco", Merchant2),
           Merchant2 = str_to_title(Merchant2),
           Merchant2 = str_replace(Merchant2, "At&T", "AT&T"),
           Merchant2 = str_replace(Merchant2, "Usa", "USA"),
           MCCDesc = str_to_title(MCCDesc)
    )
}

fn_expand_date <- function(data, date_col) {
  data %>% 
    mutate(Year = year({{date_col}}),
    Month = month({{date_col}}, label = TRUE),
    Day = wday({{date_col}}, label = TRUE))
}


# Build a wrapper function for the other functions


pcard_load <- function(excel_path){
  
  
  # Names of Sheets
  shts <- excel_sheets(excel_path)
  
  # Combine sheets into single table
  cards0 <- tibble() # create empty table
  
  for(sheetname in shts){
    tmp <- read_excel(xls, sheet = sheetname )
    
    cards0 <- cards0 %>%
      bind_rows(tmp)
  }
  
  cards1 <- cards0 %>%
    rename(ID = OBJECTID, Agency = AGENCY, TrxDate = TRANSACTION_DATE, Amount = TRANSACTION_AMOUNT,
           Merchant = VENDOR_NAME, VendorState = VENDOR_STATE_PROVINCE, MCCDesc = MCC_DESCRIPTION) %>%
    fn_expand_date(TrxDate) %>%
    clean_merchants()
  
  return(cards1)
}

# Pivot table

annual_pivot <- function(data, column){
  
  data %>%
    group_by({{column}}, Year) %>%
    summarize(Amount = sum(Amount)) %>%
    pivot_wider(names_from = Year, values_from = Amount, values_fill = 0) %>%
    arrange(desc(`2021`))
  
}


  