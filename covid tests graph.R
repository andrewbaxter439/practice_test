library(tidyverse)
library(patchwork)
library(lubridate)
library(dyn)
library(zoo)

load("data/covid_data.rda")
load("data/hospitalisations.rda")
load("data/tests_and_hospitalisations.rda")

# What date had the minimul positive test rate?
covid_data %>%
  filter(perc_pos == min(perc_pos)) %>%
  select(Date, new_cases, tot_tests)


# Create graphs of daily test results and numbers

per_rates <- covid_data %>% 
  ggplot(aes(Date, perc_pos)) +
  geom_bar(stat = "identity", fill = "#004499") +
  ylab("Percentage of tests reporting +ve")

ab_rates <- covid_data %>% 
  ggplot(aes(Date, new_cases)) +
  geom_bar(stat = "identity", fill = "#33cc22") +
  ylab("Number of new cases reported")

n_tests <- covid_data %>% 
  ggplot(aes(Date, tot_tests)) +
  geom_bar(stat = "identity", fill = "#ee2233") +
  ylab("Number of tests carried out")

ab_rates/n_tests/per_rates

# How did 'Eat out to help out' affect case rates?
covid_data %>% 
  ggplot(aes(Date, new_cases)) +
  ylab("Number of new cases reported") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, NA), expand = 0) +
  geom_rect(aes(xmin = as.POSIXct("2020-08-03"), xmax = as.POSIXct("2020-08-31"), ymin = 0, ymax = Inf),
            alpha = 0.3, fill = "#ee77dd") +
  geom_bar(stat = "identity", fill = "#33cc22") +
  geom_text(data = tibble(), aes(x = as.POSIXct("2020-08-16"), y = 1500, label = "Eat out to help out"),
            colour = "#9911aa",
            fontface = "bold", angle = 90,
            vjust = 0.5,
            size = 10)

#How did that correspond with hospital admissions?

hosp_rates <- hospitalisations %>% 
  transmute(Date = ymd(Date),
            Hosp = NumberAdmitted) %>% 
  ggplot(aes(Date, Hosp)) +
  geom_bar(stat = "identity", fill = "#eeaa00") +
  ylab("Hospitalisations") +
  xlim(ymd("2020-04-01"), NA)


(ab_rates/n_tests/per_rates/hosp_rates)




# time series analysis ---------------------------------------------------
# What previous day's tests best predicts current day's hospitalisations?

hospitalised <- zoo(tests_and_hospitalisations$hospitalised) 
prop_pos <- zoo(tests_and_hospitalisations$prop_pos) 

mod <- dyn$lm(hospitalised ~ prop_pos + stats::lag(prop_pos) + stats::lag(prop_pos, -1) + stats::lag(prop_pos, -2))

summary(mod)

# lag -1 seems best

# What is the correlation between test positive rates of hospitalisaitons on day = d
# and test positive rates on day = d-1?

# Does test positive rate of number of tests best predict hospitalisations?

tests_and_hospitalisations %>% 
  lm(hospitalised ~ prev_test, data = .) %>% 
  summary()

tests_and_hospitalisations %>% 
  lm(hospitalised ~ tot_tests, data = .) %>% 
  summary()

# Visualise across all metrics - which best predicts hospitalisations?

tests_and_hospitalisations %>% 
  select(-prev_test) %>%
  pivot_longer(-Date, names_to = "metric", values_to = "value") %>% 
  ggplot(aes(Date, value, fill = metric)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ metric, ncol = 1, scales = "free_y", strip.position = "left") +
  theme_minimal() +
  theme(legend.position = "none")

# Looks like test pos rate? Make a presentation graph

tests_and_hospitalisations %>%
  select(Date, hospitalised, prop_pos) %>%
  pivot_longer(-Date, names_to = "metric", values_to = "value") %>%
  mutate(
    metric = ifelse(
      metric == "hospitalised",
      "Number of hospital admissions",
      "Proportion of tests showing +ve"
    )
  ) %>%
  ggplot(aes(Date, value, fill = metric)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ metric,
              ncol = 1,
              scales = "free_y",
              strip.position = "right") +
  theme_minimal() +
  scale_y_continuous("") +
  theme(legend.position = "none",
        text = element_text(size = 14))
