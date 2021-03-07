hosp <- readr::read_csv("daily_covid_admissions_20210224.csv")

save(hosp, file = "data/hosp.rda")
