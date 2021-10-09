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
         Merchant = VENDOR_NAME, VendorState = VENDOR_STATE_PROVINCE, MCCDesc = MCC_DESCRIPTION) %>%
  # mutate(Year = year(TrxDate),
  #        Month = month(TrxDate, label = TRUE),
  #        Day = wday(TrxDate, label = TRUE))
  fn_expand_date(TrxDate) %>%
  clean_merchants()


summary(cards1)


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
  