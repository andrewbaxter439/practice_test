library(dplyr)
library(lubridate)

hospitalisations <- readr::read_csv("data-raw/daily_covid_admissions_20210224.csv") %>%
  mutate(
    Date = ymd(Date)
  )

save(hospitalisations, file = "data/hospitalisations.rda")
