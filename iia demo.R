# SETUP ENVIRONMENT ----

# Install Needed Packages (Onetime)
#install.packages('tidyverse')
#install.packages('readxl')
#install.packages('lubridate')
#install.packages('gt')
#install.packages('janitor')

# Load the packages we will be using
library(tidyverse)
library(readxl)
library(lubridate)
library(gt)
library(janitor)

# Set Working Director to this file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Bit too much to remember so let's create our first function

fn_setwd_here <- function(){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

fn_setwd_here()
# LOAD ----

# Location of excel file with a year for each sheet
xls <- "./input/DC PCards.xlsx"

# Names of Sheets in Excel File
?excel_sheets
shts <- excel_sheets(xls)

# Combine sheets into single table
card0 <- tibble()
str(card0)

for(sheetname in shts){
  tmp <- read_excel(xls, sheet = sheetname)
  
  card0 <- card0 %>%
    bind_rows(tmp)
}

summary(card0)

# Tidy & Introduce Pipes

cards1 <- card0 %>% # %>% is a pipe
  rename(ID = OBJECTID, Agency = AGENCY, TrxDate = TRANSACTION_DATE, Amount = TRANSACTION_AMOUNT,
         Merchant = VENDOR_NAME, VendorState = VENDOR_STATE_PROVINCE, MCCDesc = MCC_DESCRIPTION) 


# Now we create our second function

cards2 <- cards1 %>%
  fn_clean_merchants() %>%
  fn_expand_date(TrxDate)


# PRE-BUILT FUNCTIONS ----

source("./R/iia_load_functions.R")

cards <- fn_pcard_load("./input/DC PCards.xlsx")

#Let's look at our created functions for this 



# Single line load

cards <- fn_pcard_load("./input/DC PCards.xlsx")


# ANALYSIS ----

# Tidy data good for pivot tables
cards %>%
  group_by(Agency, Year) %>%
  summarise(Amount = sum(Amount),
            .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = Amount, values_fill = 0) %>%
  arrange(desc(`2021`))

cards %>%
  group_by(Merchant2, Year) %>%
  summarize(Amount = sum(Amount),
            .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = Amount, values_fill = 0) %>%
  arrange(desc(`2021`))

fn_annual_pivot(cards, Merchant2)

dcps_merch <- cards %>%
  filter(Agency == "District of Columbia Public Schools") %>%
  fn_annual_pivot(Merchant2)

dcps_merch
?pivot_longer
dcps_merch %>%
  pivot_longer(2:4, names_to = "Year", values_to = "Amount")

# pivot_wider spreads tidy data into columns

source("./R/iia_analysis_functions.R")


# pivot_longer gathers columns like monthly or annual cols into tidy format


source("./R/iia_table_functions.R")

dcps_desc <- cards %>%
  fn_annual_filterpivot(Agency, "District of Columbia Public Schools", MCCDesc) %>%
  head(10) %>%
  adorn_totals(name = "TOTAL") %>%
  fn_easy_gt(gt_title = "DCPS High Spending",
             gt_subtitle = "CY21 is through Aug 31st",
             num_col_pattern = "CY",
             border_cols = "CY21")


# AUTOMATED TESTING ----

# Test for Split Payments

# AUTOMATED TESTING ----

# Test for Split Payments

############### 1. Split Meals

split_test <- function(data){
  
  split1 <- data %>%
    filter(str_detect(str_to_lower(MCCDesc), "restaur| eating|food")) %>%
    group_by(TrxDate, Year, Merchant, MerchantState, MCCDesc) %>%
    summarise(Count = n(),
              Amount = sum(Amount),
              .groups = "drop") %>%
    arrange(desc(Amount)) %>%
    filter(Count > 1, 
           Amount > 1000) %>%
    mutate(Key = paste(TrxDate, Merchant))
  
  data %>%
    mutate(Key = paste(TrxDate, Merchant)) %>%
    filter(Key %in% split1$Key) %>%
    mutate(Test = "Split") %>%
    select(-Key)
}

split_test(cards)

############### 2. Multiples of Exactly $5,000 Spend

fiveK_test <- function(data){
  data %>%
    filter(Amount %% 5000 == 0) %>%
    arrange(desc(Amount)) %>%
    mutate(Test = "$5K")
}

fiveK_test(cards)

############### 3. Go Nats

sports_test <- function(data){
  data %>%
    filter(str_detect(str_to_lower(Merchant), "nats|wshgt|capital one arena")) %>%
    arrange(desc(Amount)) %>%
    mutate(Test = "Sports")
}

sports_test(cards)

############### 4. Weekend Meals

weekend_dining_test <- function(data){
  data %>%
    filter(str_detect(str_to_lower(MCCDesc), "restaur| eating|food")) %>%
    filter(Day == "Sat" | Day =="Sun") %>%
    arrange(desc(Amount)) %>%
    mutate(Test = "Weekend")
}

weekend_dining_test(cards) %>%
  filter(Year == 2020)

############### 5. Paypal and Similar

paypal_test <- function(data){
  data %>%
    filter(str_detect(str_to_lower(Merchant), "paypa|zelle|venmo|square")) %>%
    arrange(desc(Amount))%>%
    mutate(Test = "Paypal")
}
cards %>%
  paypal_test()

############### 6. Payments

pmt_test <- function(data){
  data %>%
    filter(str_detect(str_to_lower(Merchant), "payment"),
           Amount > 1000) %>%
    arrange(desc(Amount)) %>%
    mutate(Test = "Payments")
  
}

pmt_test(cards)    

############### 7. Universities

univ_test <- function(data){
  data %>%
    filter(str_detect(str_to_lower(MCCDesc), "univ"),
           Amount > 1000) %>%
    arrange(desc(Amount)) %>%
    mutate(Test = "Univ")
}

univ_test(cards)

############## Create Test Set

all_results <- function(data){
  tibble() %>%
    bind_rows(split_test(data)) %>%
    bind_rows(fiveK_test(data)) %>%
    bind_rows(sports_test(data)) %>%
    bind_rows(weekend_dining_test(data))  %>% 
    bind_rows(paypal_test(data)) %>%   
    bind_rows(pmt_test(data)) %>%  
    bind_rows(univ_test(data))  
}

set.seed(123)
cards %>%
  all_results() %>%
  slice_sample(n = 3)

select_results <- function(test_data, sample_per_grp = 3){
  test_data %>%
    group_by(Test) %>%
    slice_sample(n = sample_per_grp)
}


test_final <- fn_pcard_load("./input/DC PCards.xlsx") %>%
  all_results() %>%
  select_results(3)


######## Run From Top




