# https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials
library(dplyr)
library(ggplot2)
df <- read.csv("FORMATTED_DATA/Sessions.csv",
               stringsAsFactors = FALSE) %>%
  rename(Date = Day.Index)
df <- df %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Sessions = as.numeric(sub(",", "", Sessions))) %>%
  filter(Date > "2016-04-08") 
ggplot(df, aes(Date, Sessions)) + 
  geom_line(color = "red", size = 2) + 
  scale_x_date(date_breaks = "3 months",
               limits = c(as.Date("2016-01-01"),
                          as.Date("2018-03-01")),
               date_labels = "%b %Y") + 
  scale_y_continuous(labels = scales::comma) + 
  theme_bw()