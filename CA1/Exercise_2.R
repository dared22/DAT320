setwd("/Users/pasha/Desktop/DAT320/CA1")
data <- read.csv2("temperature_data.csv")
#####a
head(data)

typeof(data$area)
data$area <- as.factor(data$area)
data$station <- as.factor(data$station)
data$date <- as.Date(as.POSIXct(data$dateTime, format = "%d.%m.%Y"))
data <- subset(data, select = -dateTime)
data$avgTemp <- as.double(data$avgTemp)

head(data)
#####b

stats <- data %>%
  filter(!is.na(avgTemp)) %>%
  group_by(area) %>%
  summarise(
    mean   = mean(avgTemp, na.rm = TRUE),
    sd     = sd(avgTemp, na.rm = TRUE),
    median = median(avgTemp, na.rm = TRUE),
    min    = min(avgTemp, na.rm = TRUE),
    max    = max(avgTemp, na.rm = TRUE),
    .groups = "drop"
  )
stats 

#The mean temp in Bergen, Drammen, Moss and Oslo is almost the same, 
#however mean temp in Tromso is much lower which makes sense. Same could be said 
#about median. Max temp is almost identical in everywhere. And min temp is much
#lower in Tromso obviously, and a bit higher in bergen as it is near the coast. 

####c
ggplot(data, aes(x = date, y = avgTemp, color = area, group = area)) +
  geom_line() +
  labs(title = "avgTemp", x = "Date", y = "Temperature (°C)") +
  theme_minimal() 

#seems like there are missing data in tromso, and the values for Oslo
#Moss and Drammen are pretty much the same, while Bergen is a little higher and 
#Tromso is a little lower. There is smaller variation in temp in Bergen, than other
#places so std makes sense aswell.

####d
# Drammen, Moss, Oslo together
ggplot(filter(data, area %in% c("Drammen", "Moss", "Oslo")),
       aes(x = avgTemp, fill = area)) +
  geom_density(alpha = 0.4) +
  labs(title = "Temperature Distribution: Drammen, Moss & Oslo",
       x = "Temperature (°C)", y = "Density") +
  theme_minimal()

# Bergen
ggplot(filter(data, area == "Bergen"),
       aes(x = avgTemp, fill = area)) +
  geom_density(alpha = 0.4) +
  labs(title = "Temperature Distribution: Bergen",
       x = "Temperature (°C)", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")

# Tromsø
ggplot(filter(data, area == "Tromsø"),
       aes(x = avgTemp, fill = area)) +
  geom_density(alpha = 0.4) +
  labs(title = "Temperature Distribution: Tromsø",
       x = "Temperature (°C)", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual(values = c("Tromso" = "steelblue"))

#Bergen looks like normal distribution Tromso and Moss
#has a potential double top but also looks like normal. Drammen and oslo has tripple
#tops for some reason, but also alomost normal dist. They are alsmost symmetrical

####e
library(dplyr)
polution_data <- read.csv("air_polution.csv")

# ensure both have Date type
data$date <- as.Date(data$date)  # or: as.Date(data$dateTime, format = "%d.%m.%Y")
polution_data$date <- as.Date(substr(polution_data$dateTime, 1, 10))

# join on BOTH keys
df_joined <- inner_join(
  data,
  polution_data,
  by = c("area", "date")
)


head(df_joined)

######f

df_drammen <- df_joined %>%
  filter(area == "Drammen") %>%
  group_by(date, component) %>%
  summarise(value = mean(value, na.rm = TRUE),
            avgTemp = mean(avgTemp, na.rm = TRUE),
            .groups = "drop")

head(df_drammen)

ggplot(df_drammen, aes(x = date)) +
  geom_line(aes(y = avgTemp, color = "Temperature"), linewidth = 1) +
  geom_line(aes(y = value,   color = component),  linewidth = 1) +
  labs(title = "Drammen: Temperature and Pollution",
       x = "Date", y = "Value")+
  theme_minimal()
######g
#looks like the polutions are correlating with eachother, but not so much with temp.
#####h
#Correlation = two variables move together (an association).
#Causation = changing X produces a change in Y
#Correlation does not imply causation? Does causation imply correlation? yes.

######i
install.packages("fastDummies")
library(fastDummies)

df_wide <- df_joined %>%
  pivot_wider(names_from = component, values_from = value) #expand df_joined


df_enc <- df_wide %>%
  dummy_cols(select_columns = c("area"),
             remove_selected_columns = TRUE, # drop original 'area
             remove_first_dummy = FALSE) # keep all dummy columns

num_cols <- df_enc %>% select(where(is.numeric)) %>% names() #keep only numeric cols
num_predictors <- setdiff(num_cols, "avgTemp") #Target

df_scaled <- df_enc %>%
  mutate(across(all_of(num_predictors), ~ as.numeric(scale(.)))) #standard scale

set.seed(123)  # for reproducibility
n <- nrow(df_scaled) #number of rows
idx_train <- sample.int(n, size = floor(0.75 * n)) #(total, 75%)

train_df <- df_scaled[idx_train, ]
test_df  <- df_scaled[-idx_train, ]

####j
train_df <- train_df %>%
  select(avgTemp, PM2.5, PM10, starts_with("area_")) #keep only area and pollution

test_df <- test_df %>%
  select(avgTemp, PM2.5, PM10, starts_with("area_"))  #keep only area and pollution


lm_model <- lm(avgTemp ~ PM2.5 + PM10 + area_Bergen + area_Drammen + area_Moss, data = train_df)

summary(lm_model)#bigger p -> lower significance
#Errors range roughly ±20 °C; median near 0 → model is centered but has wide spread.

#Residual SE = 7.13 °C → typical error around 7°C.

#R² = 0.091 (Adj. 0.087) → model explains ~9% of variance; weak predictive power.

#F-test p < 2e-16 → as a group, predictors have nonzero explanatory power (large n makes this easy).

####k

needed <- setdiff(names(test_df), "avgTemp") #remove NA
mask <- complete.cases(test_df[needed])

preds <- predict(lm_model, newdata = test_df[mask, , drop = FALSE])
y     <- test_df$avgTemp[mask]

residuals <- y - preds
RMSE <- sqrt(mean(residuals^2))
MAE  <- mean(abs(residuals))
R2   <- 1 - sum(residuals^2) / sum((y - mean(y))^2)

cat("n used:", sum(mask), "of", nrow(test_df), "\n")
cat("RMSE:", RMSE, "\nMAE:", MAE, "\nR²:", R2, "\n")

#RMSE: On average, our model predictions are off by about 7.4 °C, with bigger mistakes penalized heavily
#MSE: The average miss is about 5.9 °C
#R2: Only 3% of the variance in avgTemp in the test set is explained by PM2.5, PM10, and area.

#p-value (Pr(>|t|)): test of null hypothesis 

#(Intercept) = 8.38 → expected avgTemp for the baseline area.

#PM2.5 = -1.69 (p < 0.001) → holding everything else fixed, +1 unit PM2.5 associates with ~1.69°C lower avgTemp (statistically significant).

#PM10 = -0.64 (p = 0.026) → +1 unit PM10 associates with ~0.64°C lower avgTemp (weak/moderate significance).

#Area dummies shown are not significant vs the baseline (p ≫ 0.05)







