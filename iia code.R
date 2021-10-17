library(tidyverse)
library(readxl)
library(lubridate)

# LOAD ----
# Location of excel file with a year for each sheet
xls <- "./input/DC PCards.xlsx"

# Names of Sheets
shts <- excel_sheets(xls)

# Combine sheets into single table
cards0 <- tibble() # create empty table

for(sheetname in shts){
  tmp <- read_excel(xls, sheet = sheetname )
  
  cards0 <- cards0 %>%
    bind_rows(tmp)
}

summary(cards0)

# Tidy

cards1 <- cards0 %>%
  rename(ID = OBJECTID, Agency = AGENCY, TrxDate = TRANSACTION_DATE, Amount = TRANSACTION_AMOUNT,
         Merchant = VENDOR_NAME, VendorState = VENDOR_STATE_PROVINCE, MCCDesc = MCC_DESCRIPTION) 

# Let's create our first function

fn_simple <- function(data){
  data %>%
    rename(ID = OBJECTID, Agency = AGENCY, TrxDate = TRANSACTION_DATE, Amount = TRANSACTION_AMOUNT,
           Merchant = VENDOR_NAME, VendorState = VENDOR_STATE_PROVINCE, MCCDesc = MCC_DESCRIPTION) 
}

identical(cards1, cards0 %>%
  fn_simple())

summary(cards1)

#Let's look at our created functions for this 



# Single line load

cards <- pcard_load("./input/DC PCards.xlsx")


# ANALYSIS ----

cards %>%
  group_by(Agency, Year) %>%
  summarize(Count = n(),
            Amount = sum(Amount)) 

cards %>%
  group_by(Agency, Year) %>%
  summarize(Amount = sum(Amount)) %>%
  pivot_wider(names_from = Year, values_from = Amount, values_fill = 0) %>%
  arrange(desc(`2021`))

cards %>%
  group_by(Merchant2, Year) %>%
  summarize(Amount = sum(Amount)) %>%
  pivot_wider(names_from = Year, values_from = Amount, values_fill = 0) %>%
  arrange(desc(`2021`))

annual_pivot(cards, Agency)

cards %>%
  filter(Agency == "District of Columbia Public Schools") %>%
  annual_pivot(Merchant2)

cards %>%
  annual_filterpivot(Agency, "District of Columbia Public Schools", Merchant2)
