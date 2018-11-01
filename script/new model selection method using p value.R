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

Model<- lm(wt ~ wt.1 + dwt + ht + dht + gestation + number + parity + BMI + dBMI + race + drace, birth_data)
summary(Model)

#remove dBMI
Model <- update(Model, .~. - dBMI)
summary(Model) # R square improves and F statistics improves

#remove drace
Model <- update(Model, .~. - drace)
summary(Model) # F statitsics improves even though R square decreases a little bit

#remove dht
Model <- update(Model, .~. - dht)
summary(Model) # F statistics improves and R square improves

#remove wt.1
Model <- update(Model, .~. - wt.1)
summary(Model) # F statistics improves but R square decreases

#remove BMI
Model <- update(Model, .~. -BMI)
summary(Model) # F statistics improves but R square decreases a littl bit.

#all the remaining covariates are significnat now. 

#add interaction gestation:number
Model1<- update(Model, .~. + gestation:number)
summary(Model1) #R square improves 
require(car)
Anova(Model1)

# final model is Model1:  wt ~ dwt + ht + gestation + number + parity + race + gestation:number