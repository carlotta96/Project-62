birth_data <- read.csv("/Users/teng/Downloads/babies23.data", sep = "")
library(dplyr)

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

# 6 and 7 is the same for ed and ded
birth_data$ed[birth_data$ed == 7] <- 6
birth_data$ded[birth_data$ded == 7] <- 6



# get rid of factor varaible that has only one level and somke & time duplicated.
birth_data <- birth_data[, !names(birth_data) %in% c("id", "pluralty", "outcome", "sex", "smoke")]

# change character variable to factor
birth_data <- birth_data %>% mutate(
  marital = factor(marital), ed = factor(ed), ded = factor(ded), parity = factor(parity),
  race = factor(race), drace = factor(drace), inc = factor(inc), time = factor(time), number = factor(number))

# deal with varaibel date
beginning_date <- ymd("1961-01-01")
birth_data$date <- beginning_date + (birth_data$date - 1096) * days()

# adding new variable - BMI

birth_data <- birth_data %>% mutate(BMI = (wt.1 * 0.453592) / (ht * 0.0254)^2)
birth_data <- birth_data %>% mutate(dBMI = (dwt * 0.453592) / (dht * 0.0254)^2)

birth_data <- birth_data[, !names(birth_data) %in% c("ht", "dht", "wt.1", "dwt")]

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



model_v1 <- lm(wt ~ ., data = birth_data)
summary(model_v1)

library(car)
Anova(model_v1)
model_v1 <- step(model_v1) #only 7 variables used 

#interaction model time * gestation
interaction_model_1 <- lm(wt ~ time * gestation, data = birth_data)
summary(interaction_model_1)
Anova(interaction_model_1) #p = 0.004663 < 0.05, reject HO, the interaction term should keep

#add interaction to model
model_v2 <- lm(wt ~ .+ time * gestation, data = birth_data)
summary(model_v2)
model_v2 <- step(model_v2) #gestation + parity + ht + drace + dwt + time + number + gestation:time


model_v3 <- lm(wt ~ .+ time * gestation + time * number, data = birth_data)
summary(model_v3)
model_v3 <- step(model_v3) 
