setwd("C:/HackeR Tipz workshop file/my_proj")
.libPaths("C:/R Packages")

library(tidyverse)
library(lubridate)

hosp <- read_csv("daily_covid_admissions_20210224.csv")

hosp_rates <- hosp %>% 
  transmute(Date = ymd(Date),
            Hosp = NumberAdmitted) %>% 
  ggplot(aes(Date, Hosp)) +
  geom_bar(stat = "identity", fill = "#eeaa00") +
  ylab("Hospitalisations") +
  xlim(ymd("2020-04-01"), NA)

hosp_rates

ggsave("hospitalisations.png")