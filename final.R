library(ggplot2)
library(dplyr)
library(rpart)
library(pROC)
library(caret)


df = read.csv('new_charging.csv')
df_train <- read.csv('train.csv')
df_test <- read.csv('test.csv')

# Correlation Test: What is the relationship between energy consumed (kWh) and charging duration (hours)? 
correlation_result <- cor.test(df$Energy.Consumed..kWh., df$Charging.Duration..Minutes.)
correlation_result

#One-Sample Mean Test: Is the average charging cost (USD) significantly higher than $20?

t_test_result1 <- t.test(df$Charging.Cost..USD., mu = 20, alternative = 'greater')
t_test_result1

#Two-Sample Mean Test: Do commuters and Casual Driver differ in their average charging durations?
df_subset <- df %>% filter(User.Type %in% c('Commuter', 'Casual Driver')) # since we still have causal driver

t_test_result2 <- t.test(df_subset$Charging.Duration..Minutes. ~ df_subset$User.Type)
t_test_result2

#Simple Linear Regression: Can the charging rate (kW) predict the charging duration (minutes)?
model_lm <- lm(df$Charging.Duration..Minutes. ~ df$Charging.Rate..kW.)
summary(model_lm)

plot(df$Charging.Rate..kW., resid(model_lm), 
     main = "Residual Plot: Charging Rate vs Residuals",
     xlab = "Charging Rate (kW)",
     ylab = "Residuals",
     col = "blue",
     pch = 16)
abline(h = 0, col = "red", lwd = 2)

#Multiple Linear Regression: What factors (temperature, energy consumed, charging type) influence charging cost (USD)?
model_mlm <- lm(df$Charging.Cost..USD. ~ df$Temperature...C. + df$Energy.Consumed..kWh. + df$Charger.Type)
summary(model_mlm)

plot(fitted(model_mlm), resid(model_mlm),
     main = "Residual Plot: Fitted Values vs Residuals",
     xlab = "Fitted Values (Predicted Charging Cost)",
     ylab = "Residuals",
     col = "blue",
     pch = 16)
abline(h = 0, col = "red", lwd = 2)

#ANOVA: Does charging cost vary significantly between charging station locations?
anova_model <- aov(df$charging.cost.per.min ~ df$Charging.Station.Location)
summary(anova_model)

#Logistic Regression: What factors(energy consumed, distance driven, charger type, temperature, 
#Vehicle.Model, Battery.Capacity,  Vehicle.Age) influence whether a user is a long distance traveler or not?
df$LongDistance <- ifelse(df$User.Type == "Long-Distance Traveler", 1, 0)
logistic_model <- glm(LongDistance ~ Energy.Consumed..kWh. + Distance.Driven..since.last.charge...km. + 
                        Charger.Type + Temperature...C. + Vehicle.Model + Battery.Capacity..kWh. + Vehicle.Age..years., 
                      data = df, 
                      family = binomial)
summary(logistic_model)


df$PredictedProbability <- predict(logistic_model, newdata = df, type = "response")

df$PredictedClass <- ifelse(df$PredictedProbability > 0.5, 1, 0)


conf_matrix <- confusionMatrix(as.factor(df$PredictedClass), as.factor(df$LongDistance))
print(conf_matrix)

ggplot(df, aes(x = PredictedProbability)) +
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) +
  labs(title = "Predicted Probabilities of Long-Distance Traveler",
       x = "Predicted Probability",
       y = "Count") +
  theme_minimal()


roc_curve <- roc(df$LongDistance, df$PredictedProbability)
plot(roc_curve, main = "ROC Curve: Logistic Regression for Long-Distance Traveler",
     col = "blue", lwd = 2)
abline(a = 0, b = 1, col = "red", lty = 2)



#ANCOVA: Does charging duration vary across charger types after controlling for energy consumed?
ancova_model <- aov(df$Charging.Duration..hours. ~ df$Charger.Type + df$Energy.Consumed..kWh.)
summary(ancova_model)



#One Sample Test for Proportion: How often does the charging cost reach over 20 dollars
cost_threshold <- 30
cost_larger_20 <- sum(df$Charging.Cost..USD. > cost_threshold)
total <- length(df$Charging.Cost..USD.)
larger_20_proportion <- cost_larger_20 / total
alpha <- 0.05

test_result <- prop.test(x = cost_larger_20, n = total, p = 0.2, alternative = "two.sided")
test_result

#Two Sample Test for Proportion: Is the proportion of charging sessions with costs exceeding $20 different 
# for commuters compared to casual drivers?
commuters <- subset(df, User.Type == "Commuter")
casual_drivers <- subset(df, User.Type == "Casual Driver")

commuters_larger_20 <- sum(commuters$Charging.Cost..USD. > 20)
commuters_total <- nrow(commuters)
casual_driver_larger_20 <- sum(casual_drivers$Charging.Cost..USD. > 20)
casual_drivers_total <- nrow(commuters)

test_result <- prop.test(
  x = c(commuters_larger_20, casual_driver_larger_20), 
  n = c(commuters_total, casual_drivers_total),         
  alternative = "two.sided",                   
  conf.level = 0.95,                          
  correct = FALSE                                   
)
test_result

#Polynomial Regression: Can we model the relationship between Distance Driven (since last charge) and Energy Consumed (kWh)
poly_model <- lm(Charging.Rate..kW. ~ poly(Distance.Driven..since.last.charge...km., 2, raw = TRUE), data = df)
summary(poly_model)
coef(summary(poly_model))

df$PredictedEnergy <- predict(poly_model, newdata = df)

ggplot(df, aes(x = Distance.Driven..since.last.charge...km., y = Charging.Rate..kW.)) +
  geom_point(color = "blue", size = 2) +  # Scatter plot of actual data
  geom_line(aes(y = PredictedEnergy), color = "red", size = 1) +  # Polynomial regression fit
  labs(title = "Polynomial Regression: Distance Driven vs Charging Rate",
       x = "Distance Driven (km)",
       y = "Charging Rate (kWh)") +
  theme_minimal()


#Chi-Square Test for Independence: Is there an association between user type and kinds of cars?
contingency_table <- table(df$User.Type, df$Vehicle.Model)
contingency_table



chi_square_result <- chisq.test(contingency_table)
chi_square_result

#decision tree: what factors will influence the battery capacity
tree_model <- rpart(Battery.Capacity..kWh. ~ Charging.Rate..kW. + Temperature...C. + Vehicle.Age..years. +
                      Energy.Consumed..kWh. + Vehicle.Model + Charging.Duration..Minutes. + Charger.Type + 
                      Distance.Driven..since.last.charge...km., 
                    data = df, method = "anova", 
                    control = rpart.control(cp = 0.005))

summary(tree_model)
importance <- tree_model$variable.importance
importance

importance_df <- data.frame(Variable = names(importance), Importance = importance)
ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Variable Importance for Battery Capacity",
       x = "Variables", y = "Importance Score") +
  theme_minimal()

#relationship between battery capacity and vehicle model
ggplot(df_train, aes(x = Vehicle.Model, y = Battery.Capacity..kWh.)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Battery Capacity by Vehicle Model",
       x = "Vehicle Model",
       y = "Battery Capacity (kWh)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


anova_model <- aov(Battery.Capacity..kWh. ~ Vehicle.Model, data = df)
summary(anova_model)