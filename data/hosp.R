library(dplyr)

hosp <- readr::read_csv("daily_covid_admissions_20210224.csv") %>%
  mutate(
    Date = ymd(Date)
  )

save(hosp, file = "data/hosp.rda")
