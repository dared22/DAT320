setwd("/Users/pasha/Desktop/DAT320/CA1")
my_data <- read.csv("air_polution.csv")
head(my_data)

class(my_data)  

my_data$area    <- as.factor(my_data$area)
my_data$station <- as.factor(my_data$station)
my_data$component <- as.factor(my_data$component)

my_data$date <- as.Date(substr(my_data$dateTime, 1, 10))
my_data <- subset(my_data, select = -dateTime)

head(my_data)

unique(my_data$area)
unique(my_data$station)

agg <- aggregate(
  date ~ area + component,
  data = my_data[!is.na(my_data$date), ],
  FUN = function(x) c(start = min(x), end = max(x), n = length(x))
)
head(agg)

library(dplyr)

out <- my_data %>%
  filter(!is.na(date)) %>%
  group_by(area, component) %>%
  summarise(
    start_date = min(date),
    end_date   = max(date),
    n_obs      = n(),
    .groups = "drop"
  )

out
stats <- my_data %>%
  filter(!is.na(value)) %>%
  group_by(area, component) %>%
  summarise(
    mean   = mean(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    min    = min(value, na.rm = TRUE),
    max    = max(value, na.rm = TRUE),
    .groups = "drop"
  )

(stats)
###########d
library(ggplot2)

# PM2.5
ggplot(my_data %>% filter(component == "PM2.5"), 
       aes(x = date, y = value, color = area, group = area)) +
  geom_line() +
  labs(title = "PM2.5 by Area", x = "Date", y = "Concentration (µg/m³)") +
  theme_minimal() #missing data for oslo

# PM10
ggplot(my_data %>% filter(component == "PM10"), 
       aes(x = date, y = value, color = area, group = area)) +
  geom_line() +
  labs(title = "PM10 by Area", x = "Date", y = "Concentration (µg/m³)") +
  theme_minimal() #again missing some data from oslo

#########e
sum(is.na(my_data))# they probably didn't measure the concentration in those days in oslo and just started measureing after some time again


##########f

august_data <- my_data %>%
  filter(date >= as.Date("2025-08-01"),
         date <= as.Date("2025-08-31"))

# PM2.5
ggplot(august_data %>% filter(component == "PM2.5"), 
       aes(x = date, y = value, color = area, group = area)) +
  geom_line() +
  labs(title = "PM2.5 by Area", x = "Date", y = "Concentration (µg/m³)") +
  theme_minimal() #follow simillar trend but bergen is a bit more stochastic

# PM10
ggplot(august_data %>% filter(component == "PM10"), 
       aes(x = date, y = value, color = area, group = area)) +
  geom_line() +
  labs(title = "PM10 by Area", x = "Date", y = "Concentration (µg/m³)") +
  theme_minimal() #again missing some data from oslo

########g

ggplot(my_data, aes(x = area, y = value, fill = area)) +
  geom_boxplot() +
  facet_wrap(~ component, scales = "free_y") +
  labs(
    title = "Boxplots of Pollution Values by Area and Component",
    x = "Area",
    y = "Concentration (µg/m³)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#clean
cat("\014")
rm(list = ls())




