#############################################################
# global library imports we want to have everywhere go here #
#############################################################

library(tidyverse)

###########################################################
# global variables we might want to have in all notebooks #
###########################################################

# here we define, which months should be understood as "christmas months" to
# define "XMAS_donation"
XMAS_months = c(11,
                12,
                1)

# this date will be used as the reference for this analysis
reference_date <- lubridate::ymd("2021-12-17")

#######################################################
# functions for data reading and manipulation go here #
#######################################################

read_raw_customer_data <- function(path) {
  customer_segmentation_raw <- readr::read_csv2(
    "data/customer_segmentation_test.csv",
    col_types = list(col_integer(), col_character(), col_character(), col_character(),
                  col_double(), col_double(), col_character(), col_double(), col_double(),
                  col_character(), col_double(), col_double(), col_character(), col_double(),
                  col_double(), col_character(), col_double(), col_double(), col_character(),
                  col_character(), col_character()),
    guess_max = 400000
  ) %>%
    mutate(
      DateOfBirth = lubridate::dmy(`Date of Birth`),
      Gender = as.factor(Gender),
      MERCHANDISE2015 = as.factor(MERCHANDISE2015),
      MERCHANDISE2016 = as.factor(MERCHANDISE2016),
      MERCHANDISE2017 = as.factor(MERCHANDIESE2017),
      MERCHANDISE2018 = as.factor(MERCHANDIESE2018),
      MERCHANDISE2019 = as.factor(MERCHANDISE2019),
      LastPaymentDate = lubridate::dmy(LastPaymentDate),
      PenultimatePaymentDate = lubridate::dmy(PenultimatePaymentDate),
      ID = `Customer Number`
  ) %>%
    select(-c(MERCHANDIESE2017, MERCHANDIESE2018, `Date of Birth`, `Customer Number`))
  
  customer_segmentation_raw
}

enrich_with_postal_info <- function(customer_data, postal_data_path) {
  zip_code_list <- readxl::read_excel(postal_data_path)
  
  customer_data_with_zip <- customer_data %>%
    left_join(zip_code_list, by = c("Postcode" = "PLZ")) %>%
    select(-c(`gueltig ab`, `gueltig bis`, NamePLZTyp, intern_extern, adressierbar, Postfach)) %>%
    drop_na(Postcode, Ort, Bundesland) %>%
    mutate(Postcode = as.factor(Postcode),
           Bundesland = as.factor(Bundesland))
}

apply_feature_engineering <- function(customer_data) {
  feature_engineered_data <- customer_data %>%
    mutate(
      # year of customer's birthday
      year_born = lubridate::year(DateOfBirth),
      
      # age of donors at their last donation
      age_at_last_donation = lubridate::interval(DateOfBirth, LastPaymentDate) %>%
        as.numeric("years") %>%
        as.integer(),
      
      generation_moniker = case_when(
        year_born <= 1945 ~ "silent" ,
        year_born <= 1964 ~ "boomer",
        year_born <= 1980 ~ "x",
        year_born <= 1996 ~ "millennial",
        year_born <= 2012 ~ "z"
      ) %>% as_factor(),
  
      # total number of donations over all years
      COUNTtotal = COUNT2015+
                   COUNT2016+
                   COUNT2017+
                   COUNT2018+
                   COUNT2019,
  
      # total donation amount over all years
      SUMtotal = SUM2015+
                 SUM2016+
                 SUM2017+
                 SUM2018+
                 SUM2019,
  
      # average donation amount
      SUMaverage = SUMtotal / COUNTtotal,
      
      # average number of donations per year over observation period
      COUNTaverage = COUNTtotal / 5,
  
      # month of the last payment
      LastPaymentMONTH = lubridate::month(LastPaymentDate) %>% as.factor(),
  
      # month of second to last payment
      PenultimatePaymentMONTH = lubridate::month(PenultimatePaymentDate) %>% as.factor(),
  
      # year of the last payment
      LastPaymentYEAR = lubridate::year(LastPaymentDate),
  
      # year of second to last payment
      PenultimatePaymentYEAR = lubridate::year(PenultimatePaymentDate),
  
      # status as christmas donor if the last two payments were around christmas,
      # but we have to tweak the time interval (is Nov to Jan too large?)
      # also: what about people that only have one payment in total, that should be considered. The "maybe" status is shady at best
      XMAS_donor = as_factor(case_when(LastPaymentMONTH %in% XMAS_months & PenultimatePaymentMONTH %in% XMAS_months ~ "yes",
                                       LastPaymentMONTH %in% XMAS_months ~ "maybe",
                                       TRUE ~ "unlikely")),
  
      # days between last and second to last payment
      donation_interval = lubridate::day(lubridate::days(LastPaymentDate - PenultimatePaymentDate)),
      
      # days since the last payment in relation to our reference date
      days_since_last_payment = as.integer(LastPaymentDate - reference_date),
  
      # binary factor variable expressing if any merchandise was bought over the observation period (clumsily coded)
      merchandise_any = as_factor(if_else(
                                    !is.na(MERCHANDISE2015) & MERCHANDISE2015 != 0 |
                                    !is.na(MERCHANDISE2016) & MERCHANDISE2016 != 0 |
                                    !is.na(MERCHANDISE2017) & MERCHANDISE2017 != 0 |
                                    !is.na(MERCHANDISE2018) & MERCHANDISE2018 != 0 |
                                    !is.na(MERCHANDISE2019) & MERCHANDISE2019 != 0,
                                    1,
                                    0))) %>%

    # remove variables that have no further use or
    select(-c(DateOfBirth, PenultimatePaymentDate))
  
  # apparently this is more stable then using `sum`
  feature_engineered_data %>% mutate(
    count2015_f = COUNT2015 > 0,
    count2016_f = COUNT2016 > 0,
    count2017_f = COUNT2017 > 0,
    count2018_f = COUNT2018 > 0,
    count2019_f = COUNT2019 > 0,
    num_of_donation_years = count2015_f + count2016_f + count2017_f + count2018_f + count2019_f
  ) %>% select(-c(count2015_f, count2016_f, count2017_f, count2018_f, count2019_f))
}

get_preprocessed_data <- function(customer_data_path, postal_data_path) {
  preprocessed_data <- read_raw_customer_data(customer_data_path) %>%
    enrich_with_postal_info(postal_data_path) %>%
    apply_feature_engineering()
  
  preprocessed_data
}
