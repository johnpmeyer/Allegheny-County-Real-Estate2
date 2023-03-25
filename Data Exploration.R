library(readr)
library(tidyverse)
library(lubridate)
library(ggrepel)

setwd("~/GitHub/Allegheny County Real Estate")


#https://data.wprdc.org/dataset/real-estate-sales/resource/5bbe6c55-bce6-4edb-9d04-68edeb6bf7b1
re_sales = read_csv("re_sales.csv")

#https://data.wprdc.org/dataset/property-assessments/resource/f2b8d575-e256-4718-94ad-1e12239ddb92
re_assessments = read_csv("re_assessments.csv")

#https://data.wprdc.org/dataset/allegheny-county-property-assessment-appeals/resource/67b18589-7bf5-4ca5-8767-3bceb318522c
re_appeals = read_csv("re_appeals.csv")

re_appeals = re_appeals %>%
  mutate(`HEARING DATE` = dmy(`HEARING DATE`))

#Dates
toDate <- function(year, month, day) {
  ISOdate(year, month, day)
}

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

re_assessments_date_count = re_assessments_date %>%
  count(SALEDATE_YEAR)

plot(re_assessments_date_count$SALEDATE_YEAR, re_assessments_date_count$n)

#2023-2013

re_assessments_2015_plus = re_assessments_date %>%
  filter(SALEDATE_YEAR >= 2015, SALEDATE_YEAR < 2023)

nrow(re_assessments_2015_plus) / nrow(re_assessments_date)


#County Type of Sale


#Join Data
re_sales_and_appeals = re_assessments_2015_plus %>%
  left_join(re_appeals, by=c("PARID"="PARCEL ID")) %>%
  mutate(assessed = ifelse(is.na(`TAX YEAR`), 0, 1))


match_rate = re_sales_and_appeals %>%
  count(assessed)

match_rate

#Quick Test
test = re_sales_and_appeals %>%
  filter(PROPERTYHOUSENUM == 166, PROPERTYADDRESS == 'LLOYD AVE')

#Grouping Test
grouped_data = re_sales_and_appeals %>%
  group_by(OWNERDESC, CLASSDESC, SALEDESC.x, USEDESC) %>%
  summarise(count = n())

#Wilkinsburg Test
wilkinsburg_test = re_sales_and_appeals %>%
  filter(MUNICODE == 866, 
         grepl("REGULAR", OWNERDESC), 
         CLASSDESC == 'RESIDENTIAL',
         USEDESC == 'SINGLE FAMILY', 
  ) %>%
  select(assessed, COMPLAINANT,  PROPERTYHOUSENUM, PROPERTYADDRESS, SALEDATE_NEW, 
         SALEDATE_YEAR, SALEPRICE, 
         PREVSALEDATE, PREVSALEPRICE, FAIRMARKETTOTAL, `PRE APPEAL TOTAL`, 
         `POST APPEAL TOTAL`, `HEARING CHANGE AMOUNT`, `HEARING DATE`)

appeal_rate_by_sale_year = wilkinsburg_test %>%
  group_by(SALEDATE_YEAR) %>%
  summarise(assessed = sum(assessed, na.rm = TRUE), 
            total_sales = n()) %>%
  ungroup() %>%
  mutate(rate = assessed / total_sales)

#Pittsburgh Test
pgh = re_sales_and_appeals %>%
  filter(grepl("PITTSBURGH", MUNIDESC)) %>%
  count(MUNIDESC)

pittsburgh_test = re_sales_and_appeals %>%
  filter(grepl("PITTSBURGH", MUNIDESC), 
         grepl("REGULAR", OWNERDESC), 
         CLASSDESC == 'RESIDENTIAL',
         USEDESC == 'SINGLE FAMILY', 
  ) %>%
  select(assessed, PARID, MUNICODE, MUNIDESC, COMPLAINANT,  PROPERTYHOUSENUM, PROPERTYADDRESS, SALEDATE_NEW, 
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
    appeal_type == '1_Sale_Ago' ~ PREVSALEPRICE - PREVSALEPRICE2, 
    appeal_type == 'Last_Sale' ~ SALEPRICE - PREVSALEPRICE
  )) %>% unique() %>%
  filter(SALEDATE_NEW<'2021-01-01') %>%
  filter(is.na(COMPLAINANT)|!grepl("Owner", COMPLAINANT)) %>%
  arrange(desc(SALEDATE_NEW), desc(`HEARING DATE`))

pittsburgh_test_appeal_count = pittsburgh_test %>%
  group_by(appeal_type) %>%
  summarise(assessed_count = n())


appeal_rate_by_sale_year_pgh = pittsburgh_test %>%
  group_by(SALEDATE_YEAR) %>%
  summarise(assessed = sum(assessed, na.rm = TRUE), 
            total_sales = n()) %>%
  ungroup() %>%
  mutate(rate = assessed / total_sales)


#Agg Test
agg_test = re_sales_and_appeals %>%
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

agg_test_types = agg_test %>%
  count(appeal_type)

agg_test_types

complainant_types = agg_test %>%
  count(COMPLAINANT)

complainant_types

appeals_by_year = agg_test %>%
  group_by(SALEDATE_YEAR) %>%
  summarise(assessed = sum(assessed, na.rm = TRUE), 
            total_sales = n()) %>%
  ungroup() %>%
  mutate(pct_assessed = assessed / total_sales)

appeals_by_muni = agg_test %>%
  group_by(MUNIDESC) %>%
  summarise(assessed = sum(assessed, na.rm = TRUE), 
            total_sales = n(), 
            median_price = median(relevant_saleprice, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct_assessed = assessed / total_sales)

appeals_by_muni_assessed = agg_test %>%
  group_by(MUNIDESC, assessed) %>%
  summarise(assessed = sum(assessed, na.rm = TRUE), 
            total_sales = n(), 
            median_price = median(relevant_saleprice, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(was_assessed = ifelse(assessed==0, "Assessed", "Not"))

appeals_by_district = agg_test %>%
  group_by(SCHOOLDESC) %>%
  summarise(assessed = sum(assessed, na.rm = TRUE), 
            total_sales = n(), 
            median_price = median(relevant_saleprice, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct_assessed = assessed / total_sales)

appeals_by_district_assessed = agg_test %>%
  group_by(SCHOOLDESC, assessed) %>%
  summarise(assessed = sum(assessed, na.rm = TRUE), 
            total_sales = n(), 
            median_price = median(relevant_saleprice, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(was_assessed = ifelse(assessed!=0, "Assessed", "Not"))

ggplot(appeals_by_district_assessed, aes(fill=was_assessed, y=median_price, x=SCHOOLDESC)) + 
  geom_bar(position="dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# ggplot(appeals_by_district, aes(x=median_price, y=pct_assessed, label=SCHOOLDESC)) +
#   geom_point() + 
#   geom_text_repel() + 
#   geom_smooth(method = "lm")

# simple_lm = lm(pct_assessed ~ median_price, data = appeals_by_district)
# summary(simple_lm)

avg_assessment_lag = agg_test %>%
  filter(appeal_type != 'NA-No_Assessment') %>%
  mutate(day_diff = case_when(
    appeal_type == 'Last_Sale'  ~ as.numeric(difftime(`HEARING DATE`, SALEDATE_NEW, unit="days")), 
    appeal_type == '1_Sale_Ago' ~ as.numeric(difftime(`HEARING DATE`, PREVSALEDATE_NEW, unit="days")), 
    appeal_type == '2_Sales_Ago' ~ as.numeric(difftime(`HEARING DATE`, PREVSALEDATE_NEW2, unit="days"))))

avg_assessment_lag_no_outliers = avg_assessment_lag %>%
  filter(day_diff < 2000)

hist(avg_assessment_lag$day_diff, breaks = 50, 
     xlab = "Purchase to Assessment Lag", 
     main = "")

boxplot(avg_assessment_lag$day_diff)

avg_lag_by_district = avg_assessment_lag %>%
  group_by(SCHOOLDESC) %>%
  summarise(day_diff = median(day_diff, na.rm = TRUE))

agg_test = agg_test %>%
  mutate(day_diff = case_when(
    appeal_type == 'Last_Sale'  ~ as.numeric(difftime(`HEARING DATE`, SALEDATE_NEW, unit="days")), 
    appeal_type == '1_Sale_Ago' ~ as.numeric(difftime(`HEARING DATE`, PREVSALEDATE_NEW, unit="days")), 
    appeal_type == '2_Sales_Ago' ~ as.numeric(difftime(`HEARING DATE`, PREVSALEDATE_NEW2, unit="days")),
    appeal_type == 'NA-No_Assessment' ~ 0))

agg_test %>% count(assessed)

agg_test = agg_test %>%
  mutate(assessed = ifelse((day_diff > 365*2), 0, assessed)) 

agg_test %>% count(assessed)

#82,196 rows out of 82,415. Over 99% that are appealed happen within two years



#Current Thoughts: We have 83k rows of home assessments in allegheny county. We want to know which were appealed by the district or municipality.
#Can remove ones from 2_Sales_Ago, NA-Prior_Sale_Hearing for the purpose of this analysis. 
#We want to look at the cohort of homes that were sold between 2015-2020. Re-assessments are otherwise.
#Factors to include:
#MUNIDESC,
#If not assessed, SALEPRICE - FAIRMARKETTOTAL
#If assessed, SALEPRICE - ASSESSMENT AT THE TIME.
#SCHOOLCODE, 
#SALEPRICE
#SALEYEAR???? JUST BECAUSE MORE ASSESSMENTS RECENTLY...

#Remove data points with an appeal that came from a sale before 2015. Only 47 of these

final_predictors = agg_test %>%
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
  select(MUNIDESC, SCHOOLDESC, relevant_saleprice, SALEPRICE_MINUS_ASSESSED, RELEVANT_YEAR, assessed) %>%
  mutate(RELEVANT_YEAR = as.factor(RELEVANT_YEAR)) #82,415

final_predictors = final_predictors %>%
  filter(!is.na(relevant_saleprice))

#Remove outlier sale price #81,911
final_predictors = final_predictors %>% 
  filter(relevant_saleprice < 1000000)

hist(final_predictors$relevant_saleprice, breaks = 20, 
     xlab = "Sale Price", 
     main = "Sales Price Histogram")

appeals_by_year = final_predictors %>%
  group_by(RELEVANT_YEAR) %>%
  summarise(assessed = sum(assessed, na.rm = TRUE), 
            total_sales = n()) %>%
  ungroup() %>%
  mutate(pct_assessed = assessed / total_sales)

ggplot(appeals_by_year, aes(y=pct_assessed, x=RELEVANT_YEAR)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  coord_cartesian(ylim = c(0, 1))


#quick test

model_glm = glm(formula = assessed ~ MUNIDESC + SCHOOLDESC + relevant_saleprice + SALEPRICE_MINUS_ASSESSED, family = "binomial", data = final_predictors)

test_data = data.frame(MUNIDESC = character(), 
                       SCHOOLDESC = character(), 
                       relevant_saleprice = numeric(), 
                       SALEPRICE_MINUS_ASSESSED = numeric(), 
                       stringsAsFactors = FALSE)

test_data[1, ] = list("28th Ward - PITTSBURGH", "Pittsburgh", 169000, 100000)
test_data[2, ] = list("Avalon", "Northgate", 169000, 100000)
test_data[3, ] = list("Wilkinsburg", "Wilkinsburg Boro", 169000, 100000)

predict(model_glm, test_data, type="response")

model_glm_2 = glm(formula = assessed ~ SCHOOLDESC + relevant_saleprice + SALEPRICE_MINUS_ASSESSED, family = "binomial", data = final_predictors)

test_data_2 = data.frame(SCHOOLDESC = character(), 
                         relevant_saleprice = numeric(), 
                         SALEPRICE_MINUS_ASSESSED = numeric(), 
                         stringsAsFactors = FALSE)

test_data_2[1, ] = list("Pittsburgh", 169000, 90000)
test_data_2[2, ] = list("Northgate", 169000, 90000)
test_data_2[3, ] = list("Wilkinsburg Boro", 169000, 90000)
test_data_2[4, ] = list("Woodland Hills", 169000, 90000)
test_data_2[5, ] = list("Pittsburgh", 269000, 150000)
test_data_2[6, ] = list("Northgate", 269000, 150000)
test_data_2[7, ] = list("Wilkinsburg Boro", 269000, 150000)
test_data_2[8, ] = list("Woodland Hills", 269000, 150000)
test_data_2[9, ] = list("Woodland Hills", 195000, 86500)

predict(model_glm_2, test_data_2, type="response")



#More formal train / test attempt
smp_size <- floor(0.8 * nrow(final_predictors))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(final_predictors)), size = smp_size)

train <- final_predictors[train_ind, ]
test <- final_predictors[-train_ind, ]

model_glm_3 = glm(formula = assessed ~ SCHOOLDESC + relevant_saleprice + SALEPRICE_MINUS_ASSESSED, family = "binomial", data = train)

train_pred = predict(model_glm_3, train[2:4], type="response")
train_pred = ifelse(train_pred>=.5, 1, 0)
train_misclassifier = mean(train_pred!=train[,6], na.rm = TRUE)
sum(train$assessed) / nrow(train)
train_misclassifier

test_pred = predict(model_glm_3, test[2:4], type="response")
test_pred = ifelse(test_pred>=.5, 1, 0)
test_misclassifier = mean(test_pred!=test[,6], na.rm = TRUE)
sum(test$assessed) / nrow(test)
test_misclassifier

model_glm_4 = glm(formula = assessed ~ SCHOOLDESC + relevant_saleprice + SALEPRICE_MINUS_ASSESSED + RELEVANT_YEAR, family = "binomial", data = train)

train_pred = predict(model_glm_4, train[2:5], type="response")
train_pred = ifelse(train_pred>=.5, 1, 0)
train_misclassifier = mean(train_pred!=train[,6], na.rm = TRUE)
sum(train$assessed) / nrow(train)
train_misclassifier

test_pred = predict(model_glm_4, test[2:5], type="response")
test_pred = ifelse(test_pred>=.5, 1, 0)
test_misclassifier = mean(test_pred!=test[,6], na.rm = TRUE)
sum(test$assessed) / nrow(test)
test_misclassifier


