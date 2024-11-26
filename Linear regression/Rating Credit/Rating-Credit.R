library(readr)
Credit = read_csv("C:/Users/ice_2/Downloads/Credit_Data.csv")
View(Credit)


##EDA
library(summarytools)
dfSummary(Credit)

#plot Histogram & Bar chart
library(dplyr)
library(tidyverse)

#histogram
Credit %>%
  select(c(2:7, 12)) %>% 
  mutate_all(as.numeric) %>% 
  select_if(is.numeric) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free")

#bar  chart
Credit %>%
  select(c(8:11)) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(x = value)) +
  geom_bar() +
  facet_wrap(~name, scales = "free")

#scatter plot
Credit %>%
  select(c(2:7, 12)) %>% 
  pivot_longer(cols = -Rating) %>% 
  ggplot(aes(x = value, y = Rating)) +
  geom_point() +
  facet_wrap(~name, scales = "free")

##Create LM
Rating.model = lm(Rating ~ Income + Limit + Cards + Age 
                  + Education + Gender + Student + Married + Ethnicity + Balance, data = Credit)
summary(Rating.model)
anova(Rating.model)

##Check Assumption
#1.linearity 
library(nortest)
residuals = Rating.model$resid
plot(fitted(Rating.model),residuals, col = "black" , pch = 20 ,main = "Ressiduals and Balance" 
     ,xlab = "Fitted" , ylab = "Residuals")

Credit %>%
  select(c(2:7, 12)) %>% 
  pivot_longer(cols = -Rating) %>% 
  ggplot(aes(x = value, y = Rating)) +
  geom_point() +
  facet_wrap(~name, scales = "free")

#2.Normality 
qqnorm(resid(Rating.model), main = "Normal Q-Q Plot of residual", col = 'black')
ad.test(residuals)


#3.Homoscedasticity 
plot(fitted(Rating.model),resid(Rating.model), col = "black" , pch = 20 , xlab = "Fitted Rating" , ylab = 
       "Residuals")
#4.Autocorrelation
plot(Credit$ID,resid(Rating.model), col = "black" , pch = 20 , xlab = "order" , ylab = 
       "Residuals")

library(car)
durbinWatsonTest(Rating.model)

#5.Multicollinearity
library(corrplot)
cor_matrix = cor(Credit[,c(2:3,5:7,12)])
corrplot(cor_matrix, method = "number",col = colorRampPalette(c("blue", "black", "red"))(200))
library(car)
vif(Rating.model)

##Find outlier with standarized residual
rstandard(Rating.model)[abs(rstandard(Rating.model)) > 3]
plot(fitted(Rating.model),rstandard(Rating.model))

##find Influential points
#1.leverage
leverage = hatvalues(Rating.model)
plot(leverage)
hat_values_outlier = leverage > (3*12/400)
outlier_positions = which(hat_values_outlier)

if (any(hat_values_outlier)) {
  print("Some data points have high leverage.(> 3p/n) :")
  print(outlier_positions)
} else {
  print("No data points have high leverage")
}

#2.Cook’s distance
cooks = cooks.distance(Rating.model)
plot(cooks)
which_abs_cooks_gt_1 = which(abs(cooks) > 1)
if (length(which_abs_cooks_gt_1) > 0) {
  cat("**Observations with a Cook's distance greater than 1:**\n")
  print(data.frame(obs = which_abs_cooks_gt_1,cooks = cooks[which_abs_cooks_gt_1]))
  } else { cat("**Observations don't have a Cook's distance greater than 1**\n")}


#3.dffits
h = hatvalues(Rating.model)
library(olsrr)
studres = rstandard(Rating.model)
dffts = studres^2 / (2 * (1 - h))
abs_dffts = abs(dffts)
which_abs_dffts_gt_1 = which(abs_dffts > 1)
if (length(which_abs_dffts_gt_1) > 0) {
  cat("**Observations with a dffits greater than 1**\n")
  print(data.frame(obs = which_abs_dffts_gt_1,
                   dffts = dffts[which_abs_dffts_gt_1]))} else {cat("**Observations don't have a dffits greater than 1**\n")}

##Variable Selection Methods
#1.Forword selection
Rating.start = lm(Rating ~ 1, data = Credit)
Rating.forw = step(Rating.start, scope = Rating ~ Income + Limit + Cards + Age 
                   + Education + Gender + Student + Married + Ethnicity + Balance,direction = "forward") 
coef(Rating.forw)
Rating.fw.lm = lm(Rating ~ Limit + Cards + Married + Student + Education,data = Credit)
summary(Rating.fw.lm)$r.squared


#2.Backward elimination
Rating.back = step(Rating.model, direction = "backward")
coef(Rating.back)
Rating.bw.lm = lm(Rating ~ Income + Limit + Cards + Education + Married + Balance,data = Credit)
summary(Rating.bw.lm)$r.squared

#3.Stepwise regression
Rating.step = step(Rating.start, scope = Rating ~ Income + Limit + Cards + Age 
                   + Education + Gender + Student + Married + Ethnicity + Balance,direction = "both") 
coef(Rating.step)
Rating.sw.lm = lm(Rating ~ Limit + Cards + Married + Student + Education,data = Credit)
summary(Rating.sw.lm)$r.squared

#Model comparison with R^2adj
summary(Rating.fw.lm)$adj.r.squared
summary(Rating.bw.lm)$adj.r.squared
summary(Rating.sw.lm)$adj.r.squared
