library(readr)
library(tidyverse)
library(lubridate)

setwd("~/GitHub/Allegheny-County-Real-Estate2")

#https://data.wprdc.org/dataset/real-estate-sales/resource/5bbe6c55-bce6-4edb-9d04-68edeb6bf7b1
re_sales = read_csv("re_sales.csv")

#https://data.wprdc.org/dataset/property-assessments/resource/f2b8d575-e256-4718-94ad-1e12239ddb92
re_assessments = read_csv("re_assessments.csv")

#https://data.wprdc.org/dataset/allegheny-county-property-assessment-appeals/resource/67b18589-7bf5-4ca5-8767-3bceb318522c
re_appeals = read_csv("re_appeals.csv")

#Convert Hearing Data column to Date
re_appeals = re_appeals %>%
  mutate(`HEARING DATE` = dmy(`HEARING DATE`))

#Helper function to change dates in assessments data.
toDate <- function(year, month, day) {
  ISOdate(year, month, day)
}

#Date alteration from assessments data.
re_assessments_date = re_assessments %>% 
  filter(!is.na(SALEDATE), 
         grepl("[0-9]{2}\\-[0-9]{2}\\-[0-9]{4}", SALEDATE)) %>%
  mutate(SALEDATE_MONTH = as.numeric(substr(SALEDATE, 1, 2)), 
         SALEDATE_DAY = as.numeric(substr(SALEDATE, 4, 5)), 
         SALEDATE_YEAR = as.numeric(substr(SALEDATE, 7, 10)),
         SALEDATE_NEW = toDate(SALEDATE_YEAR, SALEDATE_MONTH, SALEDATE_DAY),
         PREVSALEDATE_MONTH = as.numeric(substr(PREVSALEDATE, 1, 2)), 
         PREVSALEDATE_DAY = as.numeric(substr(PREVSALEDATE, 4, 5)), 
         PREVSALEDATE_YEAR = as.numeric(substr(PREVSALEDATE, 7, 10)),
         PREVSALEDATE_NEW = toDate(PREVSALEDATE_YEAR, PREVSALEDATE_MONTH, PREVSALEDATE_DAY),
         PREVSALEDATE_MONTH2 = as.numeric(substr(PREVSALEDATE2, 1, 2)), 
         PREVSALEDATE_DAY2 = as.numeric(substr(PREVSALEDATE2, 4, 5)), 
         PREVSALEDATE_YEAR2 = as.numeric(substr(PREVSALEDATE2, 7, 10)),
         PREVSALEDATE_NEW2 = toDate(PREVSALEDATE_YEAR2, PREVSALEDATE_MONTH2, PREVSALEDATE_DAY2))

#Joining Sales and Appeals data.
re_sales_and_appeals = re_assessments_2015_plus %>%
  left_join(re_appeals, by=c("PARID"="PARCEL ID")) %>%
  mutate(assessed = ifelse(is.na(`TAX YEAR`), 0, 1))

#Finalizing Data:
final_data = re_sales_and_appeals %>%
  filter(grepl("REGULAR", OWNERDESC), 
         CLASSDESC == 'RESIDENTIAL',
         USEDESC == 'SINGLE FAMILY', 
  ) %>%
  select(assessed, PARID, MUNICODE, MUNIDESC, SCHOOL_CODE, SCHOOLDESC, COMPLAINANT,  PROPERTYHOUSENUM, PROPERTYADDRESS, SALEDATE_NEW, 
         SALEDATE_YEAR, SALEPRICE, 
         PREVSALEDATE_NEW, PREVSALEPRICE, PREVSALEDATE_NEW2, PREVSALEPRICE2,
         FAIRMARKETTOTAL, `PRE APPEAL TOTAL`, 
         `POST APPEAL TOTAL`, `HEARING CHANGE AMOUNT`, `HEARING DATE`) %>%
  mutate(appeal_type = case_when(
    `HEARING DATE` <= PREVSALEDATE_NEW2 ~ 'NA-Prior_Sale_Hearing', 
    `HEARING DATE` > PREVSALEDATE_NEW2 & `HEARING DATE` <= PREVSALEDATE_NEW ~ '2_Sales_Ago', 
    `HEARING DATE` > PREVSALEDATE_NEW & `HEARING DATE` <= SALEDATE_NEW ~ '1_Sale_Ago',
    `HEARING DATE` > SALEDATE_NEW ~ 'Last_Sale',
    TRUE ~ 'NA-No_Assessment'
  )) %>%
  mutate(price_diff_purchase = case_when(
    appeal_type == 'NA-No_Assessment' ~ SALEPRICE - PREVSALEPRICE,
    appeal_type == '1_Sale_Ago' ~ PREVSALEPRICE - PREVSALEPRICE2, 
    appeal_type == 'Last_Sale' ~ SALEPRICE - PREVSALEPRICE
  )) %>% 
  mutate(relevant_date = case_when(
    appeal_type == 'NA-No_Assessment' ~ SALEDATE_NEW,
    appeal_type == 'Last_Sale' ~ SALEDATE_NEW,
    appeal_type == '1_Sale_Ago' ~ PREVSALEDATE_NEW,
    appeal_type == '2_Sales_Ago' ~ PREVSALEDATE_NEW2
  )) %>%
  mutate(relevant_saleprice = case_when(
    appeal_type == 'NA-No_Assessment' ~ SALEPRICE,
    appeal_type == 'Last_Sale' ~ SALEPRICE,
    appeal_type == '1_Sale_Ago' ~ PREVSALEPRICE,
    appeal_type == '2_Sales_Ago' ~ PREVSALEPRICE2    
  )) %>%
  unique() %>%
  filter(SALEDATE_NEW<'2021-01-01') %>%
  filter(is.na(COMPLAINANT)|!grepl("Owner", COMPLAINANT)) %>%
  filter(appeal_type!= 'NA-Prior_Sale_Hearing') %>%
  filter(relevant_date >= '2015-01-01') %>%
  mutate(RELEVANT_YEAR = year(relevant_date)) %>%
  arrange(desc(SALEDATE_NEW), desc(`HEARING DATE`))

#Looking at days until assessment
final_data = final_data %>%
  mutate(day_diff = case_when(
    appeal_type == 'Last_Sale'  ~ as.numeric(difftime(`HEARING DATE`, SALEDATE_NEW, unit="days")), 
    appeal_type == '1_Sale_Ago' ~ as.numeric(difftime(`HEARING DATE`, PREVSALEDATE_NEW, unit="days")), 
    appeal_type == '2_Sales_Ago' ~ as.numeric(difftime(`HEARING DATE`, PREVSALEDATE_NEW2, unit="days")),
    appeal_type == 'NA-No_Assessment' ~ 0))

#Finalizing a couple other factors
final_data = final_data %>%
  mutate(FAIRMARKETTOTALFINAL = case_when(
    appeal_type == 'NA-No_Assessment' ~ FAIRMARKETTOTAL, 
    appeal_type == 'Last_Sale' ~ FAIRMARKETTOTAL, 
    appeal_type == '1_Sale_Ago' ~ `PRE APPEAL TOTAL`, 
    appeal_type == '2_Sales_Ago' ~ `PRE APPEAL TOTAL`
  )) %>%
  mutate(SALEPRICE_MINUS_ASSESSED = case_when(
    appeal_type == 'NA-No_Assessment' ~ SALEPRICE - FAIRMARKETTOTAL, 
    appeal_type == 'Last_Sale' ~ SALEPRICE - FAIRMARKETTOTAL, 
    appeal_type == '1_Sale_Ago' ~ PREVSALEPRICE - `PRE APPEAL TOTAL`, 
    appeal_type == '2_Sales_Ago' ~ PREVSALEPRICE2 - `PRE APPEAL TOTAL`    
  )) %>%
  select(MUNIDESC, SCHOOLDESC, relevant_saleprice, SALEPRICE_MINUS_ASSESSED, assessed) #82,415

final_data = final_data %>%
  filter(!is.na(relevant_saleprice))

#Remove outlier sale price #81,911
final_data = final_data %>% 
  filter(relevant_saleprice < 1000000)
