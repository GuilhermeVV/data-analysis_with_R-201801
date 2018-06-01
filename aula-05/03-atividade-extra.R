if (!"Hmisc" %in% installed.packages()) install.packages("Hmisc")
if (!"ggcorrplot" %in% installed.packages()) install.packages("ggcorrplot")

library(tidyverse)
library(lubridate)
library(magrittr)
library(Hmisc)

ted_talks_recentes <- read_csv("aula-05/data/ted_main.csv.gz")

ted_talks_recentes %>%
  mutate(duration = as.duration(duration),
         film_date = as_datetime(film_date),
         published_date = as_datetime(published_date)
  ) -> ted_talks_recentes

#ted_talks_recentes %>%
#  mutate(published_date_year = year(published_date)) %>%
#  filter(published_date_year >= 2012 & published_date_year <= 2017) %>%
#  ggplot(aes(x = published_date_year, y = views)) +
#  geom_col(alpha = .6) +
#  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark=",", scientific = FALSE)) %>%
#  View()

ted_talks_recentes %>%
  mutate(published_date_year = year(published_date)) %>%
  filter(published_date_year >= 2012 & published_date_year <= 2017) %>%
  ggplot(aes(x = views)) +
  scale_x_continuous(labels = scales::format_format(big.mark = ".", decimal.mark=",", scientific = FALSE)) +
  geom_histogram(breaks = seq(from = 50000, to = 50000000, by = 100000)) +
  facet_wrap (~published_date_year, ncol = 2)