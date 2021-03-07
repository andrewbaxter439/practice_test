library(dplyr)
library(tidyr)
library(lubridate)

headers <- c("Date", "cum_person_neg", "cum_person_pos", "cum_person_tot",
             "new_cases", "new_cases_perc", "nhs_daily", "nhs_cum", "uk_daily", "uk_cum", 
             "tot_tests", "tot_pos", "pos_perc", "people_first_test_7days", "positive_cases_7days",
             "test_reported_7days", "pos_tests_7days", "pos_rate_7days", "tests_7days_per1000", "X1", "X2")

covid_data <- readxl::read_xlsx(
  "data/COVID-19+Daily+data+-+Trends+in+daily+COVID-19+data+-3+March+2021.xlsx",
  sheet = "Table 5 - Testing",
  skip = 4,
  col_names = headers,
  col_types = c("date", rep("guess", 20))
) %>%
  select(-X1, -X2) %>%
  mutate(across(-Date, replace_na, 0)) %>%
  filter(tot_tests != 0) %>%
  mutate(
    prop_pos = new_cases/tot_tests,
    perc_pos = 100*new_cases/tot_tests,
    Date = date(Date)
  )

save(covid_data, file = "data/covid_data.rda")
