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

#Linear Regression=========================================================================================

#put all varaibles in the model
model_v1 <- lm(wt ~ ., data = birth_data)
summary(model_v1) # coefficient of number 7 is NA; adj R squared = 0.2909
library(car)
Anova(model_v1)
model_v1 <- step(model_v1, direction = "both") #only 7 variables used using AIC
summary(model_v1)

model_v1 <- lm(wt ~ gestation + parity + ht + drace + dwt + time + number, birth_data)
summary(model_v1) 

# parameter of number 7 is NA. 
alias(model_v1) #seems variable time and number have collinearity

# extract the dummy variables of time and number
dummy_variables <- model.matrix(model_v1)[, 19:33]
dummy_variables <- data.frame(dummy_variables)
dummy_variables <- dummy_variables %>% mutate(sum_of_time = time1+time2+time3+time4+time5+time6+time7+time8,
                                              sum_of_number1_to_number6 = number1+number2+number3+number4+number5+number6)
dummy_variables <- dummy_variables %>% mutate(difference = sum_of_time - sum_of_number1_to_number6 - number7)
# there is linear relationship within our variables: 
#number 7 = time1 + time2 + time3 + time4 + time5 + time6 + time7 + time8 - number1 - number2 - number3 - number4 - number5 - number6


# drop number first
model_v2 <- lm(wt ~. - number, birth_data)
summary(model_v2) 
model_v2 <- step(model_v2)
summary(model_v2) #Adj R square = 0.2944

#drop time then
model_v3 <- lm(wt ~. - time, birth_data)
summary(model_v3) 
model_v3 <- step(model_v3)
summary(model_v3) #Adj R square = 0.2952. Time is dropped. wt ~ gestation + parity + ht + drace + dwt + number


# combine some levels in parity and adj R squared improves to 0.2991
levels(birth_data$parity)[levels(birth_data$parity)%in% c("0")] <- "initial pregnancy"
levels(birth_data$parity)[levels(birth_data$parity)%in% c("1", "2")] <- "2 previous pregnancy"
levels(birth_data$parity)[levels(birth_data$parity)%in% c("3", "4", "5")] <- "3-5 previous pregnancy"
levels(birth_data$parity)[levels(birth_data$parity)%in% c("10", "11")] <- "10-11 previous pregnancy"
model_v4 <- lm(wt ~ gestation + parity + ht + drace + dwt + number, birth_data)
summary(model_v4) # adj R squared = 0.2991
Anova(model_v4)

#Interaction  ================================================================================================
#interaction model number * gestation

model_v5 <- update(model_v4, .~. + number * gestation)
summary(model_v5)
Anova(model_v5) # p = 0.0049484 < 0.05, reject HO, the interaction term should keep
model_v6 <- step(model_v5)


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













# adding new variable - BMI
birth_data <- birth_data %>% mutate(BMI = (wt.1 * 0.453592) / (ht * 0.0254)^2)
birth_data <- birth_data %>% mutate(dBMI = (dwt * 0.453592) / (dht * 0.0254)^2)


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

