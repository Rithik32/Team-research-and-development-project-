# load tidyverse libary for data stuff
library(tidyverse)

# reading the csv file into R
data <- read.csv("1- total-deaths-comparison.csv")

# filering data for brazil only from 1990-2019
# selecting the colums we need and removing NA values
brazil_data <- data %>%
  filter(Entity == "Brazil", Year >= 1990, Year <= 2019) %>%
  select(Year, Deaths = Deaths.that.are.from.all.causes..in.both.sexes.aged.all.ages) %>%
  filter(!is.na(Deaths)) %>%
  arrange(Year)

# check if data looks ok
head(brazil_data)
summary(brazil_data)

# makeing scatter plot with regresssion line
# this shows the relashionship between year and deaths
png("scatter_plot.png", width = 800, height = 600, res = 100)
ggplot(brazil_data, aes(x = Year, y = Deaths)) +
  geom_point(size = 3, color = "#2c3e50") +
  geom_smooth(method = "lm", se = TRUE, color = "#e74c3c", fill = "#e74c3c", alpha = 0.2) +
  labs(
    title = "Relationship Between Year and Total Deaths in Brazil (1990-2019)",
    x = "Year",
    y = "Total Deaths"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
dev.off()

# histogram to see distribtion of deaths
# helps check if its normaly distributed
png("histogram.png", width = 800, height = 600, res = 100)
ggplot(brazil_data, aes(x = Deaths)) +
  geom_histogram(bins = 10, fill = "#3498db", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Total Deaths in Brazil (1990-2019)",
    x = "Total Deaths",
    y = "Frequency"
  ) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
dev.off()

# runing pearsons correlation test
# this will tell us if theres a significent relationship
correlation_test <- cor.test(brazil_data$Year, brazil_data$Deaths, method = "pearson")
print(correlation_test)

# shapiro wilk test to check normalty of residuals
# if p value is above 0.05 then its normal
model <- lm(Deaths ~ Year, data = brazil_data)
residuals <- residuals(model)
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

# print out all the importent results
cat("\n RESULTS SUMMERY OF CORRELATION TEST =====\n")
cat("Correlation coefficent (r):", round(correlation_test$estimate, 4), "\n")
cat("t-statistic:", round(correlation_test$statistic, 4), "\n")
cat("Degrees of freedom:", correlation_test$parameter, "\n")
cat("P-value:", format(correlation_test$p.value, scientific = TRUE), "\n")
cat("95% Confidance Interval:", round(correlation_test$conf.int[1], 4), "to", round(correlation_test$conf.int[2], 4), "\n")

# desicion based on p value
# reject null if p < 0.05
if (correlation_test$p.value < 0.05) {
  cat("\nDecision: REJECT the null hypotheis\n")
  cat("There is a statistically signficant correlation between Year and Total Deaths.\n")
} else {
  cat("\nDecision: FAIL TO REJECT the null hypotheis\n")
  cat("There is no statistically signficant correlation between Year and Total Deaths.\n")
}
