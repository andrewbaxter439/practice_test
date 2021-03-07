library(dplyr)
library(tidyr)

load("data/covid_data.rda")
load("data/hospitalisations.rda")

tests_and_hospitalisations <- covid_data %>%
  left_join(hospitalisations, by = "Date") %>% 
  select(Date, prop_pos, tot_tests, new_cases, hospitalised = NumberAdmitted) %>%
  mutate(prev_test = lag(prop_pos)) %>%
  drop_na()

save(tests_and_hospitalisations, file = "data/tests_and_hospitalisations.rda")
