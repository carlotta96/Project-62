birth_data <- read.csv("/Users/teng/Downloads/babies23.data", sep = "")
library(dplyr)

#Data cleaning=========================================================================================
with_invalid_9 <- birth_data[, c("sex", "ed", "ded", "smoke")]
with_invalid_9[with_invalid_9 == "9"] <-NA

with_invalid_99 <- birth_data[, c("parity", "race", "age", "ht", "drace", "dage", "dht")]
with_invalid_99[with_invalid_99 == "99"] <-NA

with_invalid_999 <- birth_data[, c("wt", "wt.1", "dwt")]
with_invalid_999[with_invalid_999 == "999"] <-NA

with_invalid_9899 <- data.frame(birth_data[, "inc"])
with_invalid_9899[with_invalid_9899 == "98" | with_invalid_9899 =="99"] <-NA
colnames(with_invalid_9899) <- "inc"

with_invalid_99899 <- birth_data[, c("time", "number")]
with_invalid_99899[with_invalid_99899 == "98" | with_invalid_99899 == "99" | with_invalid_99899 == "9"] <-NA


birth_data <- cbind(birth_data[, c(1:5, 19)], with_invalid_9, with_invalid_99, with_invalid_999,
                        with_invalid_9899, with_invalid_99899)

birth_data <- na.omit(birth_data)

#get rid of 999 in gestation
birth_data <- filter(birth_data, gestation !=999)

#get rid of 10 in race and drace, not defined, could be mistakes
birth_data <- filter(birth_data, race !=10)
birth_data <- filter(birth_data, drace !=10)

# 6 and 7 is the same for ed and ded
birth_data$ed[birth_data$ed == 7] <- 6
birth_data$ded[birth_data$ded == 7] <- 6

# combine race 0-5 = white into one level
birth_data$race[birth_data$race == 1 | birth_data$race == 2 | birth_data$race == 3 | birth_data$race == 4 |
                  birth_data$race == 5] <- 0
birth_data$drace[birth_data$drace == 1 | birth_data$drace == 2 | birth_data$drace == 3 | birth_data$drace == 4 |
                   birth_data$drace == 5] <- 0

# get rid of factor varaible that has only one level and somke & time duplicated.
birth_data <- birth_data[, !names(birth_data) %in% c("id", "pluralty", "outcome", "sex", "smoke")]

# change character variable to factor
birth_data <- birth_data %>% mutate(
  marital = factor(marital), ed = factor(ed), ded = factor(ded), parity = factor(parity),
  race = factor(race), drace = factor(drace), inc = factor(inc), time = factor(time), number = factor(number))

# deal with varaible date
library(lubridate)
beginning_date <- ymd("1961-01-01")
birth_data$date <- beginning_date + (birth_data$date - 1096) * days()

# combine factor variable "parity" into 5 levels
levels(birth_data$parity)[levels(birth_data$parity)%in% c("0")] <- "initial pregnancy"
levels(birth_data$parity)[levels(birth_data$parity)%in% c("1", "2")] <- "2 previous pregnancy"
levels(birth_data$parity)[levels(birth_data$parity)%in% c("3", "4", "5")] <- "3-5 previous pregnancy"
levels(birth_data$parity)[levels(birth_data$parity)%in% c("6", "7", "9")] <- "6-9 previous pregnancy"
levels(birth_data$parity)[levels(birth_data$parity)%in% c("10", "11")] <- "10-11 previous pregnancy"

# combine factor variable "number" into 
levels(birth_data$number)[levels(birth_data$number)%in% c("0")] <- "never smoker"
levels(birth_data$number)[levels(birth_data$number)%in% c("1", "2")] <- "light smoker"
levels(birth_data$number)[levels(birth_data$number)%in% c("3", "4","5")] <- "normal smoker"
levels(birth_data$number)[levels(birth_data$number)%in% c("6", "7")] <- "heavy smoker"

# adding new variable - BMI
birth_data <- birth_data %>% mutate(BMI = (wt.1 * 0.453592) / (ht * 0.0254)^2)
birth_data <- birth_data %>% mutate(dBMI = (dwt * 0.453592) / (dht * 0.0254)^2)

#Linear Regression=========================================================================================
#Using direction "both" for stepwise selection
full <- lm(wt ~ ., data = birth_data)
null <- lm(wt ~ 1, data = birth_data)
model_v1 <- step(null, scope = list(lower = null, upper = full), direction = 'both')
summary(model_v1)
library(car)
Anova(model_v1)


model_v1 <- lm(wt ~ gestation + parity + ht + drace + dwt + number + time, birth_data)
summary(model_v1) 

# coefficient of time8 is NA. 
alias(model_v1) #seems variable time and number have collinearity

# extract the dummy variables of time and number
dummy_variables <- data.frame(model.matrix(model_v1)[, 13:23])
dummy_variables_time1to7 <- dummy_variables[,4:10]
dummy_variables_number <- dummy_variables[,1:3]
difference <- rowSums(dummy_variables_number)-rowSums(dummy_variables_time1to7)
summary(dummy_variables$time8 - difference)
# there is linear relationship within our variables: 
#time8 = number_never smoker + number_light + number_normal smoker + number_heavy smoker
 # - (time1 + time2 + time3 + time4 + time5 + time6 + time7)


# drop number first (F statistic = 14.11)
model_v2 <- update(model_v1, .~. - number)
summary(model_v2) 

#drop time then (F statistic = 18.42), so eventually time is dropped.
model_v3 <- update(model_v1, .~. - time)
summary(model_v3) 


#Interaction  ================================================================================================
#interaction model number * gestation

model_v4 <- update(model_v3, .~. + number * gestation)
summary(model_v4)
Anova(model_v4) # p = 0.0001537 < 0.05, reject HO, the interaction term should keep
model_v5 <- step(model_v4)

#interaction model drace * ht
model_v6 <- update(model_v5, .~. + drace:ht)
summary(model_v6)
Anova(model_v6) # p = 0.069 slightly higher than 0.05, use AIC
model_v7 <- step(model_v6) # AIC keeps the interaction



# Validation Dataset and Mean Squared Error=================================================================
# Hold back random 20% of observations as Test Dataset and rest 80% as training dataset
train_size <- floor(0.8 * nrow(birth_data))
set.seed(416)
train_index <- sample(seq_len(nrow(birth_data)), size = train_size)
train_set <- birth_data[train_index,]
test_set <- birth_data[-train_index,]

# Suppose two of out best model is(just an example):
# modelA: wt ~ gestation + parity + ht + drace + dwt + number + gestation:number + ht:drace
# modelB: wt ~ gestation + parity + ht + drace + dwt + number + gestation:number

# calculate coefficients for ModelA using training dataset
modelA <- lm(wt ~ gestation + parity + ht + drace + dwt + number + gestation:number + ht:drace, train_set)
summary(modelA)

modelB <- lm(wt ~ gestation + parity + ht + drace + dwt + number + gestation:number, train_set)
summary(modelB)

# make predictions on Test Dataset
predictionA <- predict(modelA, newdata = test_set)
predictionB <- predict(modelB, newdata = test_set)

# calculate mean squared error for ModelA (MSE = mean of squared differences between prediction and observed value)
MSE_A <- mean((test_set$wt-predictionA)^2)
MSE_B <- mean((test_set$wt-predictionB)^2)

#since MSE_A > MSE_B, modelB is better! Choose Model B

#==bootstrapping==============================================================================
bootstrapping <- function(index, dataset){
  dataDim <- nrow(dataset)
  bootstrap_data <- dataset[sample(1 : dataDim, dataDim, replace = T),]
  mod <- lm(wt ~ gestation + parity + ht + drace + dwt + number + gestation:number, bootstrap_data)
  coef(mod)
}

Coeflist <- lapply(1:500, bootstrapping, dataset = birth_data)
Coef_set <- do.call(cbind, lapply(lapply(Coeflist, unlist), `length<-`, max(lengths(Coeflist))))
Coef_bootstrap<-t(Coef_set)

CI <- apply(Coef_bootstrap, 2, quantile,na.rm=T,probs=c(0.025, 0.975))

#Below is just for summarising data==================================================================

# draw some pics
library(ggplot2)
varname <- colnames(birth_data)
t <- 1
for (i in colnames(birth_data)){
  assign(varname[t], ggplot(data = birth_data) + geom_point(mapping = aes_string(x = i, y = "wt")))
  t <- t + 1
}

library(gridExtra)
grid.arrange(date, gestation, marital, ed, ded, parity, race, age, ht, 
             drace, dage, dht, wt, wt.1, dwt, inc, time, number, ncol = 4, nrow = 5) 

