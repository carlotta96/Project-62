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

#Linear Regression (Using AIC)=========================================================================================
#Using direction "both" for stepwise selection
full <- lm(wt ~ ., data = birth_data)
null <- lm(wt ~ 1, data = birth_data)
model_v1 <- step(null, scope = list(lower = null, upper = full), direction = 'both')
summary(model_v1)

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
#time8 = number_never smoker + number_light smoker + number_normal smoker + number_heavy smoker
 # - (time1 + time2 + time3 + time4 + time5 + time6 + time7)


# drop number first (F statistic = 14.11)
model_v2 <- update(model_v1, .~. - number)
summary(model_v2) 

#drop time then (F statistic = 18.42), so eventually time is dropped.
model_v3 <- update(model_v1, .~. - time)
summary(model_v3) 


#Interaction for AIC Model================================================================================================
#interaction model number * gestation
library(car)
model_v4 <- update(model_v3, .~. + number * gestation)
summary(model_v4)
Anova(model_v4) # p = 0.0001537 < 0.05, reject HO, the interaction term should keep
model_v5 <- step(model_v4)


install.packages("jtools")
library(jtools)
summ (model_v5, center = TRUE)
interact_plot(model_v5, pred ="gestation", modx = "number")

#Linear Regression (P-value) ==================================================================================
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

#Interaction (P-value Model)=================================================================================
#add interaction gestation:number
Model<- update(Model, .~. + gestation:number)
summary(Model) #R square improves 
require(car) 
Anova(Model) #significant, so keep the interaction



# Validation Dataset and Mean Squared Error=================================================================
# Hold back random 20% of observations as Test Dataset and rest 80% as training dataset
train_size <- floor(0.8 * nrow(birth_data))
set.seed(416)
train_index <- sample(seq_len(nrow(birth_data)), size = train_size)
train_set <- birth_data[train_index,]
test_set <- birth_data[-train_index,]

# Suppose two of out best model is:
# modelA: wt ~ gestation + parity + ht + drace + dwt + number + gestation:number (AIC Model)
# modelB: wt ~ dwt + ht + gestation + number + parity + race + gestation:number (P-value Model)

# calculate coefficients for ModelA using training dataset
modelA <- lm(wt ~ gestation + parity + ht + drace + dwt + number + gestation:number, train_set)
summary(modelA)

modelB <- lm(wt ~ gestation + parity + ht + race + dwt + number + gestation:number, train_set)
summary(modelB)

# make predictions on Test Dataset
predictionA <- predict(modelA, newdata = test_set)
predictionB <- predict(modelB, newdata = test_set)

# calculate mean squared error for ModelA (MSE = mean of squared differences between prediction and observed value)
MSE_A <- mean((test_set$wt-predictionA)^2)
MSE_B <- mean((test_set$wt-predictionB)^2)

#since MSE_A < MSE_B, modelA is better! Choose Model A
model_final <- modelA
summary(model_final)

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

# Checking for assumptions ===========================================================================

#Assumption 1: The regression model is linear in parameters
# wt ~ gestation + parity + ht + drace + dwt + number + gestation:number parameter is linear

#Assumption 2: The mean of residual is zero
mean(model_final$residuals)

#Assumtion 3: Homoscedasticity of residuals or equal variance
old.par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(model_final)
par(old.par)

#Assumption 4: No autocorrelation of residuals
library(ggplot2)
acf(model_final$residuals)

#Assumption 5: The number of observations must be greater than number of Xs
#602observations >> number of parameters

#Assumption 6: The variability in X values is positive (X values in a given dataset must not all be the same)

#Assumption 7: No perfect collinearity
install.packages("car")
library(car)
vif(model_final)

#Assumption 8: Normality of residuals
qqnorm(model_final$residuals)
qqline(model_final$residuals)
shapiro.test(model_final$residuals)

#Additional graphs====================================================================================

#Coef plot
install.packages("arm")
install.packages("RColorBrewer")
require(arm)
require(RColorBrewer)
colori1= brewer.pal(9, "Set1")
#colori=c(colori1,"red","blue","darkgreen","brown4","cyan4")
par(mfrow=c(1,1))
Labels <-c("inter","gestation", "2 prev. pregnancy", "3-5 prev. pregnancy", "6-9 prev. pregnancy", "10-11 prev. pregnancy", "mother's height", "mex dad", "black dad", "asian dad", "mixed dad", "father's weight", "light smoker", "normal smoker", "heavy smoker", "gest:light smok", "gest:norm smoker", "gest:heavy smoker") 
x11()
coefplot(model_final, varnames = Labels, mar = c(1, 6, 6, 6), xlim=c(-200, 10), col=colori1, cex.pts = 1)
text(0.21,1.19,labels = "0.21",cex=0.8, col = colori1[1])
text(2.63,2.19,labels = "2.63",col=colori1[2],cex=0.8)
text(5.11,3.19,labels = "5.11",col=colori1[3],cex=0.8)
text(8.58,4.19,labels = "10.58",cex=0.8, col = colori1[4])
text(1.82,5.19,labels = "1.82",col=colori1[5],cex=0.8)
text(1.16,6.19,labels = "1.16",cex=0.8, col = colori1[6])
text(-7.72,7.19,labels = "-7.72",col=colori1[7],cex=0.8)
text(-6.3,8.19,labels = "-6.3",cex=0.8, col = colori1[8])
text(-5.64,9.19,labels = "-5.64",col=colori1[9],cex=0.8)
text(2.66,10.19,labels = "2.66",cex=0.8, col = colori1[1])
text(0.07,11.19,labels = "0.07",col=colori1[2],cex=0.8)
text(-82.47,12.19,labels = "-82.47",cex=0.8, col = colori1[3])
text(-148.75,13.19,labels = "-148.75",col=colori1[4],cex=0.8)
text(-173.50,14.19,labels = "-173.50",cex=0.8, col = colori1[5])
text(0.29,15.19,labels = "0.29",col=colori1[6],cex=0.8)
text(0.50,16.19,labels = "-0.50",cex=0.8, col = colori1[7])
text(0.59,17.19,labels = "0.59",col=colori1[8],cex=0.8)
text(x=-50, y=17.19, labels=("R^2 = 0.274"))

#Histogram
hist(birth_data$wt, main="Histogram of birth weight in ounces", xlab = "Ounces",
     col = "lightgreen")
abline(v=mean(birth_data$wt), col="blue", lwd=2)
abline(v=median(birth_data$wt), col="orange", lwd=2)
legend(60, 140 , legend=c("mean", "median"),col=c("blue", "orange"),
       lty=1:1, cex=1)

