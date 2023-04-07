library(dplyr)  # To manipulate data
library(ggplot2)
library(lme4)  # To fit mixed models
library(lmerTest)  # To use the anova function for the object from lmer ftn 
################################################################################
# Name : LDA_Final
#
# Data: hprice.csv
# 1) the log-transformed average sale price in thousands of dollars (narsp)
# 2) average per capita income (ypc)
# 3) percentage growth in per capita income (perypc)
# 4) regulatory environment index in which high values mean more regulations (regtest)
# 5) rent control in which 0 means “No” and 1 means “Yes” (rcdum)
# 6) adjacent to a coastline in which 0 means “No” and 1 means “Yes” (ajwtr)
# 7) indicator for MSA (msa)
# 8) year 1 = 1986 to 9 = 1994 (time)
################################################################################

data <- read.csv("//Users//suyeon//Desktop//Yonsei//course2//LongiDataAnalysis//final//hprice.csv", header = T)
str(data) # 36*9 = 324 obs
head(data)
glimpse(data) # taking a quick look

## Y : narsp
## change columns name
names(data)
names(data) <- c("y","income","per_income","regu_index",
                 "rent", "adjacent", "msa", "time")

## change MSA and time to factor variables
data_fac <- data %>% within({
  msa <- factor(msa)
  time <- factor(time, label = c("1986", "1987", "1988", "1989",
                 "1990", "1991", "1992", "1993", "1994"))
})

################################################################################
# 1. Make a plot of the data on a single panel to show how housing prices increase by year
# Describe what can be seen in the plot

xyplot(y ~ time, data=data, type=c("p","l"), group=msa,
       xlab="Years", col= "gray20",
       ylab="Housing_Prices(log)", main = "housing prices increase by year")

################################################################################
# 2. Fit a linear model with the (log) house price as the response and all other variables (except MSA) as fixed effect predictors
# Which terms are statistically significant? Discuss the coefficient for time
lm2 <- lm(y ~ income + per_income + regu_index + rent + adjacent + time,
          data = data)
summary(lm2)

################################################################################
# 3. Make a plot that shows how per-capita income changes over time. What is the nature of the increase? 
# Make a similar plot to show how income growth changes over time. Comment on the plot
xyplot(ypc ~ time, data=data, type=c("p","l"), group=msa,
       xlab="Years", col= "gray20",
       ylab="per-capita income", main = "per-capita income changes over time")
xyplot(perypc ~ time, data=data, type=c("p","l"), group=msa,
       xlab="Years", col= "gray20",
       ylab="income_growth", main = "income growth changes over time")

## change regu_index, MSA and time to factor variables
data_fac <- data %>% within({
  regu_index <- factor(regu_index)
  msa <- factor(msa)
  time <- factor(time, labels = c("1986", "1987", "1988", "1989",
  "1990", "1991", "1992", "1993", "1994"))
})

## income (per capita income)
plot(income ~ time, data = data, pch = ".", cex = 3,
     xlab="year", ylab="personal income")

lm3 <- lm(income ~ time, data = data_fac)
summary(lm3)

## per_income (percentage growth)
plot(per_income ~ time, data = data, pch = ".", cex = 3,
     xlab="year", ylab="income growth")

lm33 <- lm(per_income ~ time, data = data_fac)
summary(lm33)

################################################################################
# 4. Create a new variable that is the per-capita income for the first time period for each MSA.
# Refit the same linear model but now using the initial income and not the income as it changes over time
# Compare the two models
## new variable 생성
matrix(data$income, nrow = 9) # 각 열별로 동일한 MSA

## 각 MSA에 대해 첫 번째 기간의 1인당 소득(income) 변수
first_income_MSA <- matrix(data$income, nrow = 9)[1,] # 첫 번째 행의 값 출력

## initial income 사용
initial_data <- data[,-2]
initial_data$initial_income <- first_income_MSA

lm4 <- lm(y ~ per_income + regu_index + rent + adjacent + time + initial_income,
          data = initial_data)
summary(lm4)

################################################################################
# 5. Fit a general linear mixed effects model that has a random intercept for each MSA. Why might this be reasonable?
# The rest of the model should have the same structure as in the previous question
# Make a numerical interpretation of the coefficient of time in your model
data_inter <- glmer(y ~ income + per_income + regu_index +
                      rent + adjacent + time + (1 | msa), data_fac)
summary(data_inter)

################################################################################
# 6. Make the following diagnostic plots and interpret
data_lme <- lme(y ~ -1 + income + per_income + regu_index +
                  rent + adjacent + time, data_fac,
                random = ~ 1 | msa, method = "ML")
data_lme_2 <- update(data_lme, weights = varIdent(form = ~ 1 | time))

# (1) Residuals vs. Fitted plot
plot(data_lme_2, resid(., type="p") ~ fitted(.) | msa, msa = 0.05,  
     xlab = "Fitted", ylab = "Residuals", 
     main = "House Price Model (i): Residuals vs. Fitted values")

# (2) QQ plot of the residuals
qqnorm(data_lme_2, ~ resid(.) | msa)

# (3) QQ plot of the random effects
qqnorm(data_lme_2, ~ ranef(.), msa = 0.1)

################################################################################
# 7. Fit a model that omits the adjacent to water and rent control predictors
# Test whether this reduction in the model can be supported
omit_data <- data[, -c(5,6)]

data_fac_o <- omit_data %>% within({
  msa <- factor(msa)
  time <- factor(time, labels = c("1986", "1987", "1988", "1989",
                                  "1990", "1991", "1992", "1993", "1994"))
})

full <- gls(y ~ -1 + income + per_income + regu_index + rent + adjacent + time,
            data = data_fac,
            correlation = corCompSymm(form = ~ 1 | msa),
            weights = varIdent(form = ~ 1 | time), method = "ML")

reduced <- gls(y ~ -1 + income + per_income + regu_index + time,
               data = data_fac_o,
               correlation = corCompSymm(form = ~ 1 | msa),
               weights = varIdent(form = ~ 1 | time), method = "ML")

anova(full, reduced) # Result : The LRT results favors the full model

# Testing Wald-type test
full.alt <- gls(y ~ -1 + income + per_income + regu_index + time, data = data_fac_o,
                correlation = corCompSymm(form = ~ 1 | msa),
                weights = varIdent(form = ~ 1 | time), method = "ML")
summary(full.alt)

################################################################################
# 8. It is possible that the increase in prices may not be linear in year
# Fit a model where year is treated as a factor rather than a linear term
# Is this a better model than the previous choice? 
# Make a plot of the coefficients of the time factor that shows how prices have increased over time
data$factor_time = as.factor(data$time)
data_factor_time <- lme(y ~ initial_income + per_income + regu_index +
                          rent + adjacent + time, data = initial_data,
                        random = ~ 1 | msa, method = "ML" )
summary(data_factor_time)
