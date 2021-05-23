# Machine Learning

library(dslabs)
data(heights)
heights

str(heights)
nrow(heights)
heights$height[777]
max(heights$height)
which.min(heights$height)

library(tidyverse)
mean(heights$height)
heights %>% mean(height)
median(heights$height)
heights %>% summary(avg=mean(height))
tab <-table(heights)
mean(heights$sex=="Male")
prop.table(tab)
table(heights)
heights %>% filter(sex=="Female" & height>78) %>% nrow
heights %>% filter(height>78) %>% nrow
sum(heights$sex=="Male")/nrow(heights)
sum(heights$height > 78)
sum(heights$height>78 & heights$sex=="Female")


# The Caret Package

library(tidyverse)
library(caret)
library(dslabs)
data(heights)

#Define the outcome and predictors

y<-heights$sex
x<-heights$height
y

#Generate training and test sets

set.seed(2,sample.kind="Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index<-createDataPartition(y,times=1,p=0.5,list=FALSE)
test_set<-heights[test_index,]
class(test_set)
train_set<-heights[-test_index,]
train_set

#Guess the outcome

y_hat<-sample(c("Male","Female"),length(test_index),replace=TRUE)
y_hat
y_hat<-sample(c("Male","Female"), length(test_index), replace=TRUE) %>%
  factor(levels=levels(test_set$sex))
y_hat

#Compute accuracy

mean(y_hat==test_set$sex)

heights %>% group_by(sex) %>% summarize(avg=mean(height), SD=sd(height))
y_hat<-ifelse(x>62,"Male","Female") %>% factor(levels=levels(test_set$sex))
y_hat
mean(y==y_hat)

#examine the accuracy of 10 cutoffs
cutoff<-seq(61,70)
accuracy<-map_dbl(cutoff,function(x){
  y_hat<-ifelse(train_set$height>x,"Male","Female") %>%
    factor(levels=levels(test_set$sex))
  mean(y_hat==train_set$sex)
})
accuracy

data.frame(cutoff,accuracy) %>% 
  ggplot(aes(cutoff,accuracy)) +
  geom_point() +
  geom_line()

max(accuracy)
best_cutoff<-cutoff[which.max(accuracy)]
best_cutoff

y_hat<-ifelse(test_set$height > best_cutoff,"Male","Female") %>%
  factor(levels=levels(test_set$sex))
y_hat <-factor(y_hat)
y_hat
mean(y_hat==test_set$sex)

# Assesment

mnist<-read_mnist()
ncol(mnist$train$images)

#Confussion Matrix

# tabulate each combination of prediction and actual value
table(predicted=y_hat, actual=test_set$sex)
test_set %>% 
  mutate(y_hat=y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy=mean(y_hat==sex))

library(e1071)
confusionMatrix(data=y_hat,reference=test_set$sex)


# Maximize F-score

cutoff<-seq(61,70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x,"Male","Female") %>%
    factor(levels=levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff,F_1)
data.frame(cutoff, F_1) %>%
  ggplot(aes(cutoff, F_1)) +
  geom_point() +
  geom_line()

max(F_1)
which.max(F_1)
best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels=levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

confusionMatrix(data = y_hat, reference = test_set$sex)


#ROC and precision-recall curves

p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male","Female"),n, replace = TRUE, prob =c(p,1-p)) %>%
  factor(levels=levels(test_set$sex))
mean(y_hat==test_set$sex)

# ROC curve

probs <- seq(0, 1, length.out = 10)
probs
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1-p)) %>%
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat,test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing
guessing %>% qplot(FPR, TPR, data = ., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
cutoffs
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
  factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       FPR = 1 - specificity(y_hat, reference = test_set$sex),
       TPR = sensitivity (data=y_hat, reference = test_set$sex))
})
height_cutoff

#plot both curves together

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_point() +
  geom_line() +
  xlab("1-specificity")+
  ylab("sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x,
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

#plot precision against recall

guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE,
                  prob=c(p, 1-p)) %>%
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})
guessing

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})
height_cutoff

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color=method))+
  geom_line()+
  geom_point()

guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
guessing

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
height_cutoff

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

#Assesment

library(dslabs)
library(dplyr)
library(lubridate)
library(tidyverse)
library(e1071)
library(caret)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016,01,25) & date_time <= make_date(2016,02,1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & 
           between(minute(date_time),15,30), "inclass", "online")) %>%
  select(sex, type)
dat

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat_inclass <- dat %>% filter(type =="inclass")
mean(dat_inclass$sex == "Female")
dat_online <- dat %>% filter(type =="online")
mean(dat_online$sex == "Female")
table(dat)
str(dat)
dat %>% group_by(type) %>% summarize(prop_female=mean(sex == "Female"))

y_hat <-ifelse(dat$type == "inclass", "Female", "Male") %>%
  factor(levels=c("Female", "Male"))
mean(y_hat == y)

table(y, y_hat)
sensitivity(y_hat,y)
specificity(y_hat,y)
mean(y=="Female")
confusionMatrix(y_hat, y)


#Assesment part 2

library(caret)
data(iris)
head(iris)
str(iris)
iris<-iris[-which(iris$Species == "setosa"),]
y <- iris$Species
y
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE )
test_index
class(test_index)
test <- iris[test_index,] 
train <- iris[-test_index,]

#Sepal Length
cutoff_SL <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), by = 0.1)
cutoff_SL
accuracy_SL <- map_dbl(cutoff_SL, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(level=levels(test$Species))
  mean(y_hat==train$Species)
}) 
data.frame(cutoff_SL, accuracy_SL) %>%
  ggplot(aes(cutoff_SL, accuracy_SL))+
  geom_line() +
  geom_point()
max(accuracy_SL)
best_cutoff_SL <- cutoff_SL[which.max(accuracy_SL)]
best_cutoff_SL

#Sepal Width
cutoff_SW <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), by = 0.1)
cutoff_SW
accuracy_SW <- map_dbl(cutoff_SW, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(level=levels(test$Species))
  mean(y_hat==train$Species)
}) 
data.frame(cutoff_SW, accuracy_SW) %>%
  ggplot(aes(cutoff_SW, accuracy_SW))+
  geom_line() +
  geom_point()
max(accuracy_SW)
best_cutoff_SW <- cutoff_SW[which.max(accuracy_SW)]
best_cutoff_SW

#Petal Length
cutoff_PL <- seq(min(iris$Petal.Length), max(iris$Petal.Length), by = 0.1)
cutoff_PL
accuracy_PL <- map_dbl(cutoff_PL, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(level=levels(test$Species))
  mean(y_hat==train$Species)
}) 
data.frame(cutoff_PL, accuracy_PL) %>%
  ggplot(aes(cutoff_PL, accuracy_PL))+
  geom_line() +
  geom_point()
max(accuracy_PL)
best_cutoff_PL <- cutoff_PL[which.max(accuracy_PL)]
best_cutoff_PL

#Petal Width
cutoff_PW <- seq(min(iris$Petal.Width), max(iris$Petal.Width), by = 0.1)
cutoff_PW
accuracy_PW <- map_dbl(cutoff_PW, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(level=levels(test$Species))
  mean(y_hat==train$Species)
}) 
data.frame(cutoff_PW, accuracy_PW) %>%
  ggplot(aes(cutoff_PW, accuracy_PW))+
  geom_line() +
  geom_point()
max(accuracy_PW)
best_cutoff_PW <- cutoff_PW[which.max(accuracy_PW)]
best_cutoff_PW

#Same analysis all same time

foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
predictions
sapply(predictions,max)	
 

test
y_hat <- ifelse(test$Petal.Length > best_cutoff_PL, "virginica", "versicolor") %>%
  factor(level=levels(test$Species))
y_hat
mean(y_hat == test$Species)

#Same analysis 

predictions <- foo(train[,3])
predictions
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
rangedValues
cutoffs <-rangedValues[which(predictions==max(predictions))]
cutoffs
y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)


#Q10

foo1 <- function(x){ 
  RangedValues1 <- seq(range(x)[1], range(x)[2], by= 0.1)
  sapply(RangedValues1, function(i){
    y_hat <- ifelse(x>i, "virginica", "versicolor") %>%
      factor(level = levels(test$Species))
    mean(y_hat == test$Species)
  })
  }
predictions1 <- apply(test[,-5],2,foo1)
sapply(predictions1, max)

#Q11

plot(iris, pch = 21, bg = iris$Species)

best_cutoff_PL
best_cutoff_PW
y_hat_PLW <- ifelse(test$Petal.Length > best_cutoff_PL | 
                      test$Petal.Width > best_cutoff_PW, 
                    "virginica", "versicolor") 
mean(y_hat_PLW == test$Species)

# A different way Q11

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test_index
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)

#Assesment part 1

library(dslabs)
library(dplyr)
library(lubridate)
library(tidyverse)
library(e1071)
library(caret)

set.seed(1, sample.kind = "Rounding")
disease <- sample(c(0,1), size = 1e6, replace=TRUE, prob = c(0.98,0.02))
test <- rep (NA, 1e6)
test[disease == 0] <- sample(c(0,1), size = sum(disease==0), replace = TRUE, 
                             prob = c(0.90, 0.10))
test[disease == 1] <- sample(c(0,1), size = sum(disease == 1), replace=TRUE,
                             prob = c(0.15, 0.85))
mean(test)

mean(disease[test==0])

mean(disease[test==1]==1)
mean(disease[test==1]==1)/mean(disease==1)

#Assesment part 2

library(dslabs)
library(tidyverse)
install.packages("scales")
install.packages("broom", type="binary")
install.packages("pillar", type="binary")
install.packages("withr", type="binary")

library(scales)
data("heights")
str(heights)
heights %>% mutate(height = round(height)) %>% 
  group_by(height) %>%
  summarize(p = mean(sex=="Male")) %>%
  qplot(height, p, data=.)

ps <- seq(0, 1, 0.1)
heights %>%
  mutate(g = cut(height, quantile(height, ps), inlude.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data=.)

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
ps <- seq(0,1,0.1)

dat %>%
  mutate(g = cut(x, quantile(x, ps), inlude.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data=.)


#Linear regression for prediction
library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum ==1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son=childHeight)

head(galton_heights)

library(caret)
y <- galton_heights$son        
class(y)

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- galton_heights %>% slice(test_index)
train_set <- galton_heights %>% slice(-test_index)
test_set
train_set
avg <- mean(train_set$son)
avg

mean((avg-test_set$son)^2)

#fit lineal regression model

fit<- lm(son ~ father, data = train_set)
fit$coef

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat-test_set$son)^2)

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

?predict.lm
class(fit)
fit


#Assesment
library(tidyverse)
library(caret)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
y <- dat$y

set.seed(1, sample.kind = "Rounding")

  test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
  test_set <- dat %>% slice(test_index)
  train_set <- dat %>% slice(-test_index)
  fit <- lm(y ~ x, data = train_set )
  y_hat<- predict(fit, test_set)
  y_hat
  mean((y_hat -test_set$y)^2)

set.seed(1, sample.kind = "Rounding")

RMSE <- replicate(n, {
  test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
  test_set <- dat %>% slice(test_index)
  train_set <- dat %>% slice(-test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat<- predict(fit, test_set)
  sqrt(mean((y_hat -test_set$y)^2))
})
RMSE
mean(RMSE)
sd(RMSE)

set.seed(1, sample.kind = "Rounding")
n <- c(100, 500, 1000, 5000, 10000)
RMSE_1 <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  y<- dat$y
  replicate(100, {
    test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
    test_set <- dat %>% slice(test_index)
    train_set <- dat %>% slice(-test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat<- predict(fit, test_set)
    sqrt(mean((y_hat -test_set$y)^2))
  })
})
str(RMSE_1)
RMSE_n <- RMSE_1 %>% data.frame() %>% setNames(c("n=100", "n=500", "n=1000", "n=5000", "n=10000"))
head(RMSE_n)
RMSE_n_mean_sd <- data.frame(apply(RMSE_n,2,mean), apply(RMSE_n, 2, sd))
RMSE_n_mean_sd


set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
y <- dat$y

set.seed(1, sample.kind = "Rounding")
RMSE <- replicate(n, {
  test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
  test_set <- dat %>% slice(test_index)
  train_set <- dat %>% slice(-test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat<- predict(fit, test_set)
  sqrt(mean((y_hat -test_set$y)^2))
})
RMSE
mean(RMSE)
sd(RMSE)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
dat
cor(dat)
y <- dat$y

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test_set <- dat %>% slice(test_index)
train_set <- dat %>% slice(-test_index)
fit1 <- lm(y ~ x_1, data = train_set)
y_hat1<- predict(fit1, test_set)
sqrt(mean((y_hat1 -test_set$y)^2))
fit2 <- lm(y ~ x_2, data = train_set)
y_hat2<- predict(fit2, test_set)
sqrt(mean((y_hat2 -test_set$y)^2))
fit12 <- lm(y ~ x_1 + x_2, data = train_set)
y_hat12<- predict(fit12, test_set)
sqrt(mean((y_hat12 -test_set$y)^2))


# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)
y <- dat$y

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test_set <- dat %>% slice(test_index)
train_set <- dat %>% slice(-test_index)
fit1 <- lm(y ~ x_1, data = train_set)
y_hat1<- predict(fit1, test_set)
sqrt(mean((y_hat1 -test_set$y)^2))
fit2 <- lm(y ~ x_2, data = train_set)
y_hat2<- predict(fit2, test_set)
sqrt(mean((y_hat2 -test_set$y)^2))
fit12 <- lm(y ~ x_1 + x_2, data = train_set)
y_hat12<- predict(fit12, test_set)
sqrt(mean((y_hat12 -test_set$y)^2))


# Regression for categorical outcomes
library(dslabs)
data("heights")
y <- heights$height
set.seed(2, sample.kind = "Rounding")

test_index <- createDataPartition(y, times =1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
  filter(round(height) == 66) %>%
  summarize(y_hat = mean(sex == "Female"))

heights %>%
  mutate(x=round(height)) %>%
  group_by(x) %>%
  filter(n()>=10) %>%
  summarize(prop = mean(sex == "Female")) %>% 
  ggplot(aes(x, prop)) +
  geom_point()
as.numeric(train_set$sex =="Female")
lm_fit <- mutate(train_set, y=as.numeric(sex=="Female")) %>% 
  lm(y~height, data=.)            
lm_fit
p_hat <- predict(lm_fit, test_set)
p_hat
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>%  factor()
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]
confusionMatrix(y_hat, test_set$sex)

#Logistic regression
library(tidyverse)
library(dplyr)
library(dslabs)
library(caret)
data(heights)

heights %>% 
  mutate(x = round(height)) %>% 
  group_by(x) %>% 
  filter(n() >= 10) %>% 
  summarize(prop = mean(sex=="Female")) %>% 
  ggplot(aes(x, prop)) + 
  geom_point() +
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

range(p_hat)

#fit logistic regression model

glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex =="Female")) %>% 
  glm(y ~ height, data=., family="binomial")
glm_fit
p_hat_logit <- predict(glm_fit, newdata=test_set, type = "response")
p_hat_logit

tmp <- heights %>% 
  mutate(x = round(height)) %>% 
  group_by(x) %>% 
  filter(n() >= 10) %>% 
  summarize(prop = mean(sex == "Female"))
tmp
logistic_curve <- data.frame(x=seq(min(tmp$x), max(tmp$x))) %>% 
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty =2)

y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
y_hat_logit
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]


all_data <- data.frame(x=seq(min(tmp$x), max(tmp$x))) %>% 
                         mutate(p_hat_logis = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x),
                                p_hat_regression = lm_fit$coef[1] + lm_fit$coef[2]*x)
all_data_gathered <-all_data %>% gather(type, value, p_hat_logis:p_hat_regression)
head(all_data_gathered)
all_data_gathered %>% ggplot(aes(x, value, color=type)) +
                               geom_line()+
  geom_hline(yintercept=0.5)


#Case study 2 or 7

mnist <- read_mnist()
class(mnist$train)
str(mnist)
str(mnist_27)
class(mnist_27$train)
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), 
                             which.max(mnist_27$train$x_1))]

is
titles <- c("smallest", "largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row = 1:28, Column = 1:28) %>% 
    mutate(label = titles[i], value = mnist$train$images[is[i],])
})
head(tmp)
tmp <- Reduce(rbind, tmp)
head(tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low = "white", high = "black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

is_2 <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), 
                             which.max(mnist_27$train$x_2))]

titles <- c("smallest", "largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row = 1:28, Column = 1:28) %>% 
    mutate(label = titles[i], value = mnist$train$images[is_2[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low = "white", high = "black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

data("minst_27")
str(mnist_27$train)
  mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()
  
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7,2))
confusionMatrix(data=y_hat_glm, reference=mnist_27$test$y)$overall["Accuracy"]

str(mnist_27$true_p)
mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) + geom_raster()
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)


# set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

dat <- make_data()
str(dat)
dat$train %>% ggplot(aes(x, color=y)) + geom_density()

mu_1 <-seq(0,3, len=25)
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
acc <- sapply(mu_1, function(z){
  n = 1000
  p = 0.5
  mu_0 = 0
  sigma_0 = 1
  sigma_1 = 1
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, z, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train <- data.frame(x = x, y = as.factor(y)) %>% slice(-test_index)
  test <- data.frame(x = x, y = as.factor(y)) %>% slice(test_index)

  fit_glm <- glm(y ~ x, data=train, family="binomial")
  p_hat_glm <- predict(fit_glm, test, type="response")
  y_hat_glm<-ifelse(p_hat_glm > 0.5, 1,0) %>% factor(levels =c(0,1))
  confusionMatrix(y_hat_glm, test$y)$overall[["Accuracy"]]
})
res <- data.frame(delta=mu_1, res=acc)
res
res %>% ggplot(aes(delta, acc)) +
  geom_point()


set.seed(1) #if you are using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = ., type="response")
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)


#Introduction to smoothing

library(dslabs)
library(dplyr)
library(tidyverse)
library(ggplot2)

data("polls_2008")
head(polls_2008)
str(polls_2008)
qplot(day, margin, data=polls_2008)
fit<-lm(margin~day, data=polls_2008)
str(fit)
res <- ifelse(fit$residuals>0,"+","-")

polls_2008 %>%
  ggplot(aes(day, margin, color=res)) +
  geom_point()+
  geom_abline(intercept = fit$coef[1], slope =  fit$coef[2] )
  

#bin smoothers
span<- 7
fit <- with(polls_2008, ksmooth(day, margin, x.points = day, kernel ="box",
                               bandwidth = span))
str(fit)
polls_2008 %>% mutate(smooth=fit$y) %>% 
  ggplot(aes(day, margin)) +
  geom_point(size=3, alpha=.5, color="grey") +
  geom_line(aes(day, smooth), color="red")

# kernel
span <- 7
fit <- with(polls_2008, ksmooth(day, margin, x.points=day, kernel="normal",
                                 bandwidth = span))
fit
class(fit)
str(fit)
polls_2008 %>% mutate(smooth=fit$y) %>% 
  ggplot(aes(day, margin)) +
  geom_point(size=3, alpha=.5, color="grey") +
  geom_line(aes(day, smooth), color="red")

#Local weighted regression (loess)
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

#fitting loess straight lines
fit <- loess(margin ~ day, degree=1, span=span, data=polls_2008)
polls_2008 %>% mutate(smooth=fit$fitted) %>% 
  ggplot(aes(day, margin)) +
  geom_point(size=3, alpha=.5, color="grey") +
  geom_line(aes(day, smooth), color="red")

span_var<- c(0.1, 0.15, 0.25, 0.66)
fit_fit<-sapply(span_var, function(i) {
  fit <- loess(margin ~ day, degree=1, span=i, data=polls_2008)
})
polls_2008 %>%  mutate("0.1"=fit_fit[,1]$fitted,
                       "0.15"=fit_fit[,2]$fitted,
                       "0.25"=fit_fit[,3]$fitted,
                       "0.66"=fit_fit[,4]$fitted) %>% 
  gather(span, smooth, "0.1":"0.66", convert=TRUE)  %>% 
  ggplot(aes(day, margin)) +
  geom_point(size=1, alpha=0.5, color="grey")+
  geom_line(aes(day, smooth), color="red", alpha=0.6) +
  facet_wrap(~span)

#fitting loess parabolas
fit <- loess(margin ~ day, degree=2, span=span, data=polls_2008)
polls_2008 %>% mutate(smooth=fit$fitted) %>% 
  ggplot(aes(day, margin)) +
  geom_point(size=3, alpha=.5, color="grey") +
  geom_line(aes(day, smooth), color="red")

span_var<- c(0.1, 0.15, 0.25, 0.66)
fit_fit<-sapply(span_var, function(i) {
  fit <- loess(margin ~ day, degree=2, span=i, data=polls_2008)
})
polls_2008 %>%  mutate("0.1"=fit_fit[,1]$fitted,
                       "0.15"=fit_fit[,2]$fitted,
                       "0.25"=fit_fit[,3]$fitted,
                       "0.66"=fit_fit[,4]$fitted) %>% 
  gather(span, smooth, "0.1":"0.66", convert=TRUE)  %>% 
  ggplot(aes(day, margin)) +
  geom_point(size=1, alpha=0.5, color="grey")+
  geom_line(aes(day, smooth), color="red", alpha=0.6) +
  facet_wrap(~span)
  
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color = "red")

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color = "red", span=0.15, method="loess", 
              method.ags=list(degree=1))

#Comprenhension check: Smoothing

library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")
dat

library(lubridate)

dat <- dat %>% mutate(date_f = as.numeric(date))
span_f <- 61/diff(range(dat$date_f))
fit1 <- loess(deaths ~ date_f, degree = 1, span = span_f, data = dat)
dat %>%  mutate(smooth = predict(fit1, date_f)) %>% 
  ggplot(aes(date, deaths)) +
  geom_point() +
  geom_line(aes(date, smooth), lwd = 2, color="red")

dat %>% 
  mutate(smooth = predict(fit1, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 1)

library(broom)
library(tidyverse)
library(caret)
library(dplyr)
library(dslabs)

mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)

mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")


#Matrices

library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)
str(mnist)
str(mnist$train)
str(mnist$train$images)

x <- mnist$train$images[1:1000,]
head(mnist$train$images)
head(x)
class(x)
dim(x)
y <- mnist$train$labels[1:1000]
head(mnist$train$labels)
dim(y)

#Matrix notation

length(x[,1])
length(x[1,])
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1,x_2)
dim(x)
dim(x_1)
dim(as.matrix(x_1))
dim(x)
length(x[,1])

# Converting a vector to a matrix
my_vector <-1:15

#fill the matrix by colum
mat <- matrix(my_vector,5,3)
mat

# fill by row

mat_t <- matrix(my_vector, 3, 5, byrow=TRUE)
mat_t
identical(t(mat), mat_t)
matrix(my_vector, 5, 5)

str(x)
str(y)
x[3,]
y[3]
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)

#flip the image back
image(1:28, 1:28, grid[, 28:1])
grid
grid[, 28:1]
grid[28:1, 28:1]


# Row and column summaries and apply
library(dplyr)
library(tidyverse)
library(dslabs)

sums <- rowSums(x)
sums
avg <- rowMeans(x)

data_frame(labels = as.factor(y), row_averages = avg) %>% 
  qplot(labels, row_averages, data=., geom="boxplot")

avgs <- apply(x,1,mean)
avgs
sds <- apply(x,2,sd)
sds

# Filtering columns based on summaries

library(matrixStats)

sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))
?image
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

#extract columns and rows

x[ ,c(351,352)]
x[c(2,3),]
new_x <- x[ ,colSds(x) > 60]
dim(new_x)
class(x[,1])
dim(x[1,])

#preserve the matrix class
class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

#index with matrices and binarizing the data
#index with matrices
mat <-matrix(1:15, 5,3)
mat
class(mat)
as.vector(mat)
class(as.vector(mat))
sum(as.vector(x)>225 & as.vector(x)<=255)
sum(as.vector(x)<=225 & as.vector(x)>0)
sum(as.vector(x)==0)
qplot(as.vector(x), bins=30, color=I("black"))

new_x <- x
class(new_x)
new_x[new_x<50]<-0

mat <- matrix(1:15, 5, 3)
mat[mat>6 & mat < 15]<-0
mat

#binarize the data
bin_x <- x
bin_x[bin_x<255/2]<-0
bin_x[bin_x>255/2]<-1
bin_X<-(x>255/2)*1
dim(bin_X)

#Vectorization for Matrices and Matrix Algebra Operations
(x - rowMeans(x))/rowSds(x)

# Scale each column
t(t(x)-colMeans(x))

#take each entry of a vector and subtracts it from the corresponding 
#row or column
x_mean_0 <- sweep(x, 2, colMeans(x))

#divide by the standard deviation
x_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN="/")

#Comprehension check. Working with matrices
xx <- mnist$train$images
yy <- mnist$train$labels

sum(xx<205 & xx>50)
dim(xx)[1]*dim(xx)[2]
sum(xx<205 & xx>50)/(dim(xx)[1]*dim(xx)[2])
mean(xx<205 & xx>50)
between(xx, 50, 205)
sum(between(xx, 50, 205))/(60000*784)
mean(between(xx,50, 205))

mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y) # proportion of pixels
str(y)

?return()
#Distance

library(tidyverse)
library(dslabs)

if(!exists("mnist")) mnist <- read_mnist()
set.seed(0)
set.seed(0, sample.kind="Rounding")
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the label are in y
x <- mnist$train$images[ind,]
x
str(x)
y <- mnist$train$labels[ind]

y[1:3]
x_1 <- x[1,]
x_1
x_2 <- x[2,]
x_3 <- x[3,]

#distance between two numbers
sqrt(crossprod(x_1-x_2))
sqrt(crossprod(x_1-x_3))
sqrt(crossprod(x_2-x_3))

#Compute the distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]


#Visualize these distances
image(as.matrix(d))

#Order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute the distance between predictors
d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[492,]
d_492
image(1:28, 1:28, matrix(d_492, 28, 28))

#Comprehension check

library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

str(tissue_gene_expression$x)
d <- dist(tissue_gene_expression$x)
str(d)

samples <- c(1, 2, 39, 40, 73, 74)
samples
as.matrix(d)[samples, samples]

image(as.matrix(d))

#Knn

library(tidyverse)
library(dslabs)
data("mnist_27")
str(mnist_27$test)
mnist_27$test %>% ggplot(aes(x_1, x_2, color=y)) + geom_point()

#logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
str(p_hat_logistic)
y_hat_logistic <- factor(ifelse(p_hat_logistic>0.5, 7, 2))
str(y_hat_logistic)
confusionMatrix(data=y_hat_logistic, reference=mnist_27$test$y)$overall[1]

#fit knn model
knn_fit <- knn3(y~., data=mnist_27$train)
knn_fit

str(x)
head(mnist_27$train[,1:3])
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x,y)
knn_fit

knn_fit <- knn3(y~., data=mnist_27$train, k=5)
knn_fit

y_hat_knn <- predict(knn_fit, mnist_27$test, type="class")
confusionMatrix(y_hat_knn, reference=mnist_27$test$y)$overall["Accuracy"]
y_hat_knn


#Overtraining and oversmoothing
y_hat_knn <- predict(knn_fit, mnist_27$train, type="class")
confusionMatrix(y_hat_knn, reference=mnist_27$train$y)$overall["Accuracy"]
y_hat_knn <- predict(knn_fit, mnist_27$test, type="class")
confusionMatrix(y_hat_knn, reference=mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=1
knn_fit_1 <- knn3(y~., data=mnist_27$train, k=1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type="class")
confusionMatrix(data=y_hat_knn_1, mnist_27$train$y)$overall[["Accuracy"]]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type="class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall[["Accuracy"]]

#fit knn with k401
knn_fit_401 <- knn3(y~., data=mnist_27$train, k=401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type="class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

#pick the k in knn
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})
str(accuracy)
class(accuracy)
accuracy1 <- accuracy %>%  mutate(k=ks) %>% gather(data_type, acc, "train":"test")
str(accuracy1)
head(accuracy1)
accuracy1 %>% ggplot(aes(k, acc, color=data_type)) + geom_point() +
  geom_line()


#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)


#Comprehension check
library(dslabs)
library(caret)
library(tidyverse)
data("heights")

set.seed(1)
set.seed(1, sample.kind="Rounding")
str(heights)
x <- heights$height
y <- heights$sex
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)

test_heights <- heights[test_index,]
train_heights <- heights[-test_index,]

k <- seq(1, 101, 3)
F1 <- sapply(k, function(k) {
  fit_knn_heights <- knn3(sex~height, data=train_heights, k=k)
  y_knn_heights_hat <- predict(fit_knn_heights, test_heights, type = "class") %>% 
    factor(level=levels(train_set$sex))
  F_meas(data=y_knn_heights_hat, reference=test_heights$sex)
})
F1
k[which.max(F1)]
max(F1)

library(dslabs)
library(caret)
library(purrr)
data("tissue_gene_expression")
str(tissue_gene_expression)
str(tissue_gene_expression$x)
str(tissue_gene_expression$y)

set.seed(1)
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test_tissue_x <- x[test_index,]
test_tissue_y <- y[test_index]
train_tissue_x <- x[-test_index,]
train_tissue_y <- y[-test_index]

k <- seq(1, 11, 2)
accuracy <- sapply(k, function(k){
  fit_knn_tissue <- knn3(train_tissue_x, train_tissue_y, k=k)
  y_knn_hat <- predict(fit_knn_tissue, test_tissue_x, type="class")
  control<-confusionMatrix(data=y_knn_hat, reference=test_tissue_y)$overall["Accuracy"]
  control1 <- mean(y_knn_hat == test_tissue_y)
  data.frame(k=k, control=control, control1=control1)
  })

accuracy


#Comprehension check. Cross Validation
library(dplyr)
library(tidyverse)
library(caret)
library(dslabs)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
str(x)
y <- rbinom(n, 1, 0.5) %>% factor()
str(y)
head(y)
x_subset <- x[ ,sample(p, 100)]

?train()
names(getModelInfo())
fit <- train(x_subset, y, method = "glm")
fit$results
str(fit$results)

install.packages("Biocmanager")
BiocManager::install("genefilter")
library(genefilter)
?colttests()
tt <- colttests(x,y)
str(tt)
head(tt)
pvals <- tt$p.value

ind <- which(pvals<=0.01)
length(ind)
str(ind)

x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

fit <- train(x_subset, y, method = "knn", 
             tuneGrid = data.frame(k=seq(101, 301, 25)))
ggplot(fit)

#Re-doing logistic regression selection step in th cross-validation algorithm
indexes <- createDataPartition(y, times = 5, p = 0.2)
dat <- data.frame(y=y, data.frame(x))
res <- sapply(indexes, function(test_index){
  train_set <- slice(dat, -test_index)
  test_set <- slice(dat, test_index)
  pvals <- colttests(as.matrix(train_set[,-1]), train_set$y)$p.value
  ind <- c(TRUE, pvals <= 0.01)
  train_set <- train_set[, ind]
  fit <- glm(y ~ ., data = train_set, family = "binomial")
  y_hat <- ifelse(predict(fit, newdata = test_set[, ind], type = "response") > 0.5, 1, 0) %>%
    factor()
  mean(y_hat == test_set$y)
})
res

data("tissue_gene_expression")
set.seed(1)
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

fit <- train(x, y, method = "knn", 
             tuneGrid = data.frame(k=seq(1, 7, 2)))
ggplot(fit)
fit$results


#Bootstrap
library(dslabs)
library(tidyverse)
library(dplyr)
library(caret)

n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins=30, color=I("black"))

m <- median(income)
m

set.seed(1)
N <- 250
X <- sample(income, N)
M <- median(X)
M

library(gridExtra)
B <- 10^5
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})

p1 <- qplot(M, bins=30, color=I("black"))
p2 <- qplot(sample=scale(M)) + geom_abline()
  grid.arrange(p1, p2, ncol=2)
mean(M)
sd(M)

str(X)
B <- 10^5
M_star <- replicate(B, {
  X_star <- sample(X, N, replace=TRUE)
  median(X_star)
})

tibble(monte_carlo=sort(M), bootstrap=sort(M_star)) %>% 
  qplot(monte_carlo, bootstrap, data=.) + 
  geom_abline()
quantile(M, c(0.05,0.95))
quantile(M_star, c(0.05, 0.95))

median(X)+1.96*sd(X)/sqrt(N)*c(-1,1)
mean(M)+1.96*sd(M)*c(-1,1)
mean(M_star)+1.96*sd(M_star)*c(-1,1)


#Comprehension check. Bootstrap
library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995, sample.kind = "Rounding")
indexes <- createResample(mnist_27$train$y, 10)

str(mnist_27$train)
str(indexes$Resample01)

sum(indexes[[1]]=="3")
sum(indexes[[1]]=="4")
sum(indexes[[1]]=="7")

i <-seq(1:10)
tres <-sapply(i, function(i) {
  sum(indexes[[i]] == "3")
} )
tres
sum(tres)

x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

y <- rnorm(100, 0, 1)
qnorm(0.75)
quantile(y, 0.75)
set.seed(1, sample.kind="Rounding")
B <- 10000
q_75th <- replicate(B, {
  X <- rnorm(100, 0, 1)
  quantile(X, 0.75)
})
mean(q_75th)
sd(q_75th)

set.seed(1, sample.kind="Rounding")
B <- 10
q_75th <- replicate(B, {
  X <- sample(y, 100, replace=TRUE)
  quantile(X, 0.75)
})
mean(q_75th)
sd(q_75th)

set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding")
indexes <- createResample(y, 10)
q_75thb <- sapply(indexes, function(ind){
  y_star <-y[ind]
  quantile(y_star, 0.75)
})
q_75thb
mean(q_75thb)
sd(q_75thb)

set.seed(1, sample.kind="Rounding")
indexes <- createResample(y, 10000)
q_75thbb <- sapply(indexes, function(ind){
  y_star <-y[ind]
  quantile(y_star, 0.75)
})
mean(q_75thbb)
sd(q_75thbb)


#Naive Bayes
#Generating train and test sets
library("caret")
library(caret)
data("heights")

y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list =FALSE)
train_set <- heights %>%  slice(-test_index)
test_set <- heights %>% slice(test_index)

#Estimating averges and standard deviations
params <- train_set %>% 
  group_by(sex) %>% 
  summarize(avg = mean(height), sd = sd(height))
params

#Estimating the prevalence
pi <- train_set %>% summarize(pi = mean(sex == "Female")) %>% pull(pi)
pi

#Getting the actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi +f0*(1-pi))
f0
f1
p_hat_bayes


#Controlling prevalence
y_hat_bayes <- ifelse(p_hat_bayes>0.5, "Female", "Male")
sensitivity(data= factor(y_hat_bayes), reference = factor(test_set$sex))

#Computing specificity
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

#Changing the cutoff of the decision rule
p_hat_bayes_unbiased <- f1*0.5 / (f1*0.5 + f0*(1-0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data=factor(y_hat_bayes_unbiased), reference=factor(test_set$sex))
specificity(data=factor(y_hat_bayes_unbiased), reference=factor(test_set$sex))

#Draw plot
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept=0.5, lty=2) +
  geom_vline(xintercept=67, lty=2)


#QDA and LDA
data("mnist_27")

#Estimate parameter from the data
params <- mnist_27$train %>% 
  group_by(y) %>% 
  summarize(avg_1=mean(x_1), avg_2=mean(x_2), sd_1=sd(x_1), sd_2=sd(x_2),
            r=cor(x_1, x_2))

#Contour plots
mnist_27$train %>%  mutate(y=factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill=y, color=y)) +
  geom_point(show.legend=FALSE) +
  stat_ellipse(type="norm", lwd=1.5)

#Fit model
library(caret)
train_qda <- train(y~., method="qda", data=mnist_27$train)
plot_cond_prob(predict(train_qda, mnist_27$true_p, type="prob")[,2])

#Obtain predictors and accuracy
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data=y_hat, reference=mnist_27$test$y)$overall["Accuracy"]

#Draw separate plots for 2s and 7s
mnist_27$train %>% mutate(y=factor(y)) %>% 
ggplot(aes(x_1, x_2, fill=y, color=y)) +
  geom_point(show.legend=FALSE) +
  stat_ellipse(type="norm") + facet_wrap(~y)

#LDA
params <- mnist_27$train %>% 
  group_by(y) %>% 
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
params
train_lda <- train(y~., method="lda", data=mnist_27$train)
y_hat_lda <- predict(train_lda, mnist_27$test)
confusionMatrix(data=y_hat_lda, reference=mnist_27$test$y)$overall["Accuracy"]


#Case study. More than three classes
library(caret)
library(tidyverse)
library(dplyr)
library(dslabs)

if(!exists("mnist")) mnist <- read_mnist()
str(mnist)
str(mnist$train)

set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1, 2, 7)), 2000)
y <- mnist$train$labels[index_127]
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list=FALSE)

# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
str(row_column)
upper_left_ind <- which(row_column$col<=14 & row_column$row<=14)
lower_right_ind <- which(row_column$col>14 & row_column$row>14)

# binarize the values. Above 200 is ink, below is no ink
x <- x>200
str(x[,upper_left_ind])
str(x[,lower_right_ind])
# cbind proportion of pixels in upper right quadrant and proportion of pixels 
#in lower right
x <- cbind(rowSums(x[,upper_left_ind])/rowSums(x), 
           rowSums(x[,lower_right_ind])/rowSums(x))
train_set <- data.frame(y=factor(y[index_train]),
                        x_1=x[index_train,1],
                        x_2=x[index_train,2])
test_set <- data.frame(y=factor(y[-index_train]),
                       x_1=x[-index_train,1],
                       x_2=x[-index_train,2])

train_set %>%  ggplot(aes(x_1, x_2, color=y)) + geom_point()

train_qda <- train(y ~ ., method = "qda", data = train_set)
predict(train_qda, test_set, type = "prob") %>% head()
predict(train_qda, test_set) %>% head()

confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]
train_set %>% mutate(y = factor(y)) %>% ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + stat_ellipse(type="norm")

str(train_qda)
test_qda %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")


#Comprehension check. Generative Models
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")
set.seed(1993, sample.kind="Rounding")
str(tissue_gene_expression)
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind,]
x <- x[,sample(ncol(x), 10)]

tissue_10 <- data.frame(x=x, y=y)
fit_tissue_lda <-train(y~., data = tissue_10, method="lda")
#fit_tissue_lda <-train(x, y, method="lda")
fit_tissue_lda$results["Accuracy"]
fit_tissue_lda$results
fit_tissue_lda$finalModel

t(fit_tissue_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit_tissue_qda <- train(x, y, method="qda")
fit_tissue_qda$results["Accuracy"]
fit_tissue_qda$finalModel$means
t(fit_tissue_qda$finalModel$means) %>% data.frame() %>% 
  mutate(predictor_name=rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label=predictor_name)) +
  geom_point() +
  geom_label() +
  geom_abline()

fit_tissue_lda1 <- train(x, y, method="lda", preProcess="center")
fit_tissue_lda1$results["Accuracy"]
fit_tissue_lda1$finalModel$means
t(fit_tissue_lda1$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()

d <- apply(fit_tissue_lda1$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)

library(dslabs)      
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
fit_tissue_lda2 <- train(x, y, method="lda", preProcess="center")
fit_tissue_lda2$results["Accuracy"]


#Classification and regression trees (cart)
#load data
library(tidyverse)
library(dslabs)
data("olive")
class(olive)
str(olive)
olive %>% as_tibble()
table(olive$region)
olive <- select(olive, -area)
head(olive)

#Predict region using knn
library(caret)
fit <- train(region~., method="knn", tuneGrid = data.frame(k=seq(1, 15, 2)),
             data=olive)
ggplot(fit)

#Plot distribution of each predictor stratified by region
olive %>%  gather(fatty_acid, percentage, -region) %>% 
  ggplot(aes(region, percentage, fill=region)) +
  geom_boxplot()+
  facet_wrap(~fatty_acid, scales="free")+
  theme(axis.text.x=element_blank())

# plot values for eicosenoic and linoleic
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept=0.065, lty=2)+
  geom_segment(x=-0.2, y=10.54, xend=0.065, yend=10.54, color="black", lty=2)

#load data for regression tree
data("polls_2008")
qplot(day, margin, data=polls_2008)

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)

#Visualize the splits
plot(fit, margin=0.1)
text(fit, cex=0.75)
polls_2008 %>% 
  mutate(y_hat=predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#change parameters
fit <- rpart(margin~., data=polls_2008, control=rpart.control(cp=0, minsplit=2))
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#use crossvalidation to choose cp
library(caret)
train_rpart <- train(margin~., method="rpart", 
                     tuneGrid=data.frame(cp=seq(0, 0.05, len=25)), data=polls_2008)
ggplot(train_rpart)

#access the final model and plot it
plot(train_rpart$finalModel, margin=0.1)
text(train_rpart$finalModel, cex=0.75)
polls_2008 %>% 
  mutate(y_hat=predict(train_rpart)) %>% 
  ggplot()+
  geom_point(aes(day, margin))+
  geom_step(aes(day, y_hat), col="red")

#prune tree
pruned_fit <- prune(fit, cp=0.01)
polls_2008 %>% 
  mutate(y_hat = predict(pruned_fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")


#Classification (decision) trees
#fit a classification tree and plot it
train_rpart <- train(y~., 
                     method="rpart", 
                     tuneGrid=data.frame(cp=seq(0, 0.1, len=25)),
                     data=mnist_27$train)
plot(train_rpart)
plot_cond_prob(predict(train_rpart, mnist_27$true_p, type="prob")[,2])
#compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


#Random forests
library(randomForest)
fit <- randomForest(margin~., data=polls_2008)
plot(fit)
polls_2008 %>% 
  mutate(y_hat=predict(fit, new_data=polls_2008)) %>% 
  ggplot()+
  geom_point(aes(day, margin))+
  geom_line(aes(day, y_hat), col="red")

library(randomForest)
train_rf <- randomForest(y~., data=mnist_27$train)
plot_cond_prob(predict(train_rf, mnist_27$true_p, type="prob")[,2])
confusionMatrix(predict(train_rf, mnist_27$test),mnist_27$test$y)$overall["Accuracy"] 

#use crossvalidation to choose parameter
train_rf2 <- train(y~.,
                   method="Rborist",
                   tuneGrid=data.frame(predFixed=2, minNode=c(3, 50)),
                   data=mnist_27$train)
confusionMatrix(predict(train_rf2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


#Comprehension check. Trees and random forests
library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y~., data=dat)
plot(fit)
text(fit)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col="2")

library(randomForest)
fit <- randomForest(y~x, data=dat) 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

plot(fit)  

?randomForest()
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

  
#Caret package
#  http://topepo.github.io/caret/available-models.html   
#  http://topepo.github.io/caret/train-models-by-tag.html   
  
library(tidyverse)
library(dslabs)  
data("mnist_27")
str(mnist_27$train)
head(mnist_27$train)

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

#Tuning parameters with caret
getModelInfo("knn")
modelLookup("knn")

train_knn <- train(y~., method="knn", data=mnist_27$train)
ggplot(train_knn, highlight=TRUE)

train_knn <- train(y~., 
                   method="knn", data=mnist_27$train,
                   tuneGrid=data.frame(k=seq(9, 71, 2)))
ggplot(train_knn, highlight=TRUE)
train_knn$bestTune
train_knn$finalModel
confusionMatrix(predict(train_knn, mnist_27$test, type="raw"),
                mnist_27$test$y)$overall["Accuracy"]

control <- trainControl(method="cv", number=10, p=0.9)
train_knn_cv <- train(y~., method="knn",
                      data=mnist_27$train,
                      tuneGrid=data.frame(k=seq(9, 71, 2)),
                      trControl=control)
ggplot(train_knn_cv, highlight=TRUE)

train_knn$results
train_knn$results %>% 
  ggplot(aes(x=k, y=Accuracy))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(x=k,
                    ymin=Accuracy-AccuracySD,
                    ymax=Accuracy+AccuracySD))

plot_cond_prob <- function(p_hat=NULL){ 
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp<-mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p))+
    geom_raster(show.legend=FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4"))+
    stat_contour(breaks=c(0.5),color="black")
}
plot_cond_prob(predict(train_knn, mnist_27$true_p, type="prob")[,2])
str(predict(train_knn, mnist_27$true_p, type="prob"))
str(mnist_27$true_p)

install.packages("gam")
modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1


#Assesment
install.packages("caret")
library(rpart)
library(caret)
library(tidyverse)
library(dplyr)
library(dslabs)

data("tissue_gene_expression")
str(tissue_gene_expression)
set.seed(1991)
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
train_tge_rpart <- caret::train(x, y, method="rpart",
                    tuneGrid=data.frame(cp=seq(0, 0.1, 0.01)))
plot(train_tge_rpart)
train_tge_rpart$bestTune
train_tge_rpart$results
confusionMatrix(train_tge_rpart)

set.seed(1991)
fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))


train_tge_rpart2 <- caret::train(x, y, method="rpart",
                                tuneGrid=data.frame(cp=seq(0, 0.1, 0.01)),
                                control=rpart.control(minsplit=0))
ggplot(train_tge_rpart2)
train_tge_rpart2$results
confusionMatrix(train_tge_rpart2)

tissue_gene_df <- data.frame(x=x, y=y)
fit_tge_rpart <- rpart(y~., data=tissue_gene_df)
plot(fit_tge_rpart)
text(fit_tge_rpart, cex=0.9)
train_tge_rpart$finalModel
plot(train_tge_rpart$finalModel)
text(train_tge_rpart$finalModel)

set.seed(1991)
library(randomForest)
fit <- train(x, y, method="rf",
             tuneGrid=data.frame(mtry=seq(50, 200, 25)),
             nodesize=1)
ggplot(fit)
fit$bestTune
fit$results
fit$finalModel

varImp(fit)
imp <- varImp(fit)
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms


#Titanic excercises Part 1
library(titanic)
library(caret)
library(tidyverse)
library(rpart)

options(digits=3) #three significant digits

#clean data titanic
str(titanic_train)
titanic_clean <- titanic_train %>% 
  mutate(Survived = factor(Survived),
         Embarked= factor(Embarked),
         Age=ifelse(is.na(Age), median(Age, na.rm=TRUE), Age),
         FamilySize=SibSp + Parch + 1) %>% 
  select(Survived, Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

str(titanic_clean)

set.seed(42, sample.kind="Rounding")
y <- titanic_clean$Survived
test_index <- createDataPartition(y, times=1, p=0.2, list=FALSE)
test_set <- slice(titanic_clean, test_index)
train_set <- slice(titanic_clean, -test_index)

train_set %>% nrow(.)
test_set %>% nrow(.)
train_set %>% summarize(prop_surv_tr=mean(Survived==1)) %>% pull(prop_surv_tr)
mean(train_set$Survived==1)

N <- test_set %>% nrow(.)
set.seed(3, sample.kind="Rounding")
test_guess_survival <- sample(c(0,1), size=N, replace=TRUE, prob=NULL) %>% 
  factor(levels=levels(test_set$Survived))
mean(test_guess_survival==test_set$Survived)
data.frame(x1=test_guess_survival,x2=test_set$Survived)

train_set %>% group_by(Sex) %>% summarize(surv_prop=mean(Survived==1))

y_hat_sex <- ifelse(test_set$Sex=="female", 1,0)
mean(y_hat_sex==test_set$Survived)

train_set %>% group_by(Pclass) %>% summarize(avg=mean(Survived==1))

y_hat_class <- ifelse(test_set$Pclass==1,1,0)
mean(y_hat_class==test_set$Survived)

train_set %>% group_by(Sex, Pclass) %>% summarize(Survived=mean(Survived==1)) %>% 
  filter(Survived>0.5)

y_hat_sex_class <- ifelse((test_set$Sex=="female" & (test_set$Pclass==1|
                           test_set$Pclass==2)),1,0)
mean(y_hat_sex_class==test_set$Survived)  

y_hat_sex <- y_hat_sex %>% factor()
confusionMatrix(data=y_hat_sex, reference=test_set$Survived)

y_hat_class <- y_hat_class %>% factor()
confusionMatrix(data=y_hat_class, reference=test_set$Survived)

y_hat_sex_class <- y_hat_sex_class %>% factor()
confusionMatrix(data=y_hat_sex_class, reference=test_set$Survived)

F_meas(data=y_hat_sex, reference=test_set$Survived)
F_meas(data=y_hat_class, reference=test_set$Survived)
F_meas(data=y_hat_sex_class, reference=test_set$Survived)

?caret::train
str(train_set)
set.seed(1)
train_lda <- caret::train(Survived~Fare, method="lda", data=train_set)
y_hat_lda <- predict(train_lda, test_set)
confusionMatrix(y_hat_lda, test_set$Survived)$overall["Accuracy"]

set.seed(1)
train_qda <- caret::train(Survived~Fare, method="qda", data=train_set)
y_hat_qda <- predict(train_qda, test_set)
confusionMatrix(y_hat_qda, test_set$Survived)$overall["Accuracy"]

set.seed(1)
train_logis1 <- caret::train(Survived~Age, method="glm", data=train_set)
y_hat_logis1 <- predict(train_logis1, test_set, type="raw")
confusionMatrix(y_hat_logis1, test_set$Survived)$overall["Accuracy"]

set.seed(1)
train_logis2 <- caret::train(Survived~Sex+Pclass+Fare+Age, method="glm", data=train_set)
y_hat_logis2 <- predict(train_logis2, test_set, type="raw")
confusionMatrix(y_hat_logis2, test_set$Survived)$overall["Accuracy"]

set.seed(1)
train_logis3 <- caret::train(Survived~., method="glm", data=train_set)
y_hat_logis3 <- predict(train_logis3, test_set, type="raw")
confusionMatrix(y_hat_logis3, test_set$Survived)$overall["Accuracy"]

set.seed(6)
train_knn <- caret::train(Survived~., method="knn", data=train_set,
                          tuneGrid=data.frame(k=seq(3, 51, 2)))
ggplot(train_knn, higlight=TRUE)
train_knn$bestTune
train_knn$finalModel
train_knn$results

y_hat_knn <- predict(train_knn, test_set)
confusionMatrix(y_hat_knn, test_set$Survived)$overall["Accuracy"]

set.seed(8)
control <- trainControl(method="cv", number=10, p=0.9)
train_knn_cv <- train(Survived~., method="knn",
                      data=train_set,
                      tuneGrid=data.frame(k=seq(3, 51, 2)),
                      trControl = control)
ggplot(train_knn_cv)
train_knn_cv$bestTune
y_hat_knn_cv <- predict(train_knn_cv, test_set)
confusionMatrix(y_hat_knn_cv, test_set$Survived)$overall["Accuracy"]

set.seed(10)
train_rpart <- caret::train(Survived~., method="rpart",
                            data=train_set,
                            tuneGrid=data.frame(cp=seq(0, 0.05, 0.002)))
ggplot(train_rpart, highlight=TRUE)
train_rpart$bestTune
y_hat_rpart <- predict(train_rpart, test_set)
confusionMatrix(y_hat_rpart, test_set$Survived)$overall["Accuracy"]
confusionMatrix(y_hat_rpart, test_set$Survived)
 ?rpart()
plot(train_rpart$finalModel)
text(train_rpart$finalModel, cex=0.7)
train_rpart$finalModel
train_rpart$bestTune
train_rpart$results
varImp(train_rpart)

str(train_set)
head(train_set$Survived)
str(titanic_train)

set.seed(14)
train_rf <- caret::train(Survived~., method="rf",
                         data=train_set,
                         tuneGrid=data.frame(mtry=seq(1:7)),
                         ntree=100)
ggplot(train_rf)
train_rf$bestTune
y_hat_rf <- predict(train_rf, test_set)
confusionMatrix(y_hat_rf, test_set$Survived)$overall["Accuracy"]
varImp(train_rf)


#Model fitting and recommendation systems.
#Case study: MNIST 
library(dslabs)
mnist <- read_mnist()
data(mnist)
names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)
str(mnist)
str(mnist$train)

# sample 10k rows from training set, 1k rows from test set
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]                
y_test <- factor(mnist$test$labels[index])

library(matrixStats)
library(tidyverse)
library(dplyr)
sds <- colSds(x)
qplot(sds, bins=256, color=I("black"))

library(caret)
nzv <- nearZeroVar(x)
head(nzv)
str(nzv)
image(matrix(1:784 %in% nzv, 28, 28))

?setdiff()
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

#Model fitting for MNIST data
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

control <- trainControl(method = "cv", number=10, p=0.9)
train_knn <- train(x[,col_index], y,
                   method="knn",
                   tuneGrid=data.frame(k=c(1, 3, 5, 7)),
                   trControl = control)
ggplot(train_knn)

n <- 1000
b <- 2
nrow(x)
index <- sample(nrow(x), n)
control <- trainControl(method="cv", number=b, p=0.9)
train_knn1 <- train(x[index, col_index], y[index],
                    method="knn",
                    tuneGrid=data.frame(k=c(3,5,7)),
                    trControl=control)
ggplot(train_knn1)

fit_knn <- knn3(x[, col_index], y, k=3)

y_hat_knn <- predict(fit_knn, x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]
cm$byClass[,1:2]

library(Rborist)
control <- trainControl(method="cv", number=5, p=0.8)
grid <- expand.grid(minNode=c(1,5), predFixed=c(10, 15, 25, 35, 50))
grid
train_rf <- train(x[,col_index], y,
                  method="Rborist",
                  tuneGrid=grid,
                  trControl=control,
                  nTree=50,
                  nSamp=5000)
ggplot(train_rf)
train_rf$bestTune
fit_rf <- Rborist(x[,col_index], y,
                  nTree=1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed=train_rf$bestTune$predFixed)
y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[, col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

install.packages("rafalib")
rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[,28:1],
        main=paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}


#Variable Importance
library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y, ntree=50)
imp <- importance(rf)
imp

image(matrix(imp, 28, 28))

p_max <- predict(fit_knn, x_test[, col_index])
p_max <- apply(p_max, 1, max)
ind <- which(y_hat_knn!=y_test)
ind <- ind[order(p_max[ind], decreasing=TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main=paste0("Pr(", y_hat_knn[i],")=", round(p_max[i], 2),
                    " but is a ", y_test[i]),
        xaxt="n", yaxt="n")
}

p_max <- predict(fit_rf, x_test[,col_index])$census
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind <- which(y_hat_rf!=y_test)
ind <- ind[order(p_max[ind], decreasing=TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(", y_hat_rf[i],")=", round(p_max[i], 2),
                      " but is a ", y_test[i]),
        xaxt="n", yaxt="n")
}

?max
?apply()


#Ensemble
p_rf <- predict(fit_rf, x_test[, col_index])$census
p_rf <- p_rf/rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)


#Assesment. Ensembles
?lapply()
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", 
            "multinom", "qda", "rf", "adaboost")
library(caret)
library(dslabs)
library(tidyverse)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

pred_matrix <- sapply(fits, function(fits){
  predict(fits, mnist_27$test)
})
pred_matrix


length(mnist_27$test$y)
length(models)
dim(pred_matrix)

####
acc <- colMeans(pred_matrix == mnist_27$test$y)
acc
mean(acc)
####

pred_matrix_acc <- apply(pred_matrix, 2, function(pred_matrix){
  mean(pred_matrix==mnist_27$test$y)
})
names(pred_matrix_acc)<-models
pred_matrix_acc
mean(pred_matrix_acc)

####
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)
####

pred_maj_vote <- apply(pred_matrix, 1, function(pred_matrix){
  count_2 <- sum(pred_matrix==2)
  count_7 <- sum(pred_matrix==7)
  ifelse(count_7 > count_2,7,2)
})
pred_maj_vote_acc <-mean(pred_maj_vote==mnist_27$test$y)
confusionMatrix(factor(pred_maj_vote), factor(mnist_27$test$y))$overall["Accuracy"]

####
ind <- pred_matrix_acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]
####

sum(pred_matrix_acc>pred_maj_vote_acc)
index_acc <- which(pred_matrix_acc>pred_maj_vote_acc)
models[index_acc]

pred_train_acc <- sapply(fits, function(fits){
  min(fits$results$Accuracy)
})
pred_train_acc
mean(pred_train_acc)

index_acc_0.8 <- which(pred_train_acc>0.8)
models[index_acc_0.8]
votes_0.8 <-rowMeans(pred_matrix[, index_acc_0.8]=="7")
y_hat_0.8 <- ifelse(votes_0.8>0.5,"7", "2")
mean(y_hat_0.8==mnist_27$test$y)


#Recommendation Systems
library(dslabs)
library(tidyverse)
data("movielens")

head(movielens)
str(movielens)

movielens %>% summarize(n_users=n_distinct(userId),
                        n_movies =n_distinct(movieId))
keep <- movielens %>% 
  dplyr::count(movieId) %>% 
  top_n(5) %>% pull(movieId)
tab <- movielens %>% 
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating)
tab %>% knitr::kable()

users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>% 
  mutate(rating=1) %>% 
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>% 
  image(1:100, 1:100, ., xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col="grey")

movielens %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n))+
  geom_histogram(bins=30, color="black") +
  scale_x_log10() +
  ggtitle("Users")

library(caret)
set.seed(755)
test_index <- createDataPartition(y=movielens$rating, times=1,
                                  p=0.2, list=FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by="movieId") %>% 
  semi_join(train_set, by="userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#Building the recommendation system
options(digits=3) #three significant digits
mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)  
naive_rmse

predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

rmse_results <- data_frame(method="Just the average", RMSE=naive_rmse)
rmse_results

#fit <- lm(rating~as.factor(userId), data=movielens)
mu <- mean(train_set$rating)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i=mean(rating-mu))
head(movie_avgs)

movie_avgs %>% qplot(b_i, geom="histogram", bins=10, data=., color=I("black"))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by="movieId") %>% 
  .$b_i
head(predicted_ratings)
model_1_rmse <- RMSE(predicted_ratings,test_set$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie Effect Model",
                         RMSE=model_1_rmse))
rmse_results

rmse_results %>% knitr::kable()

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u=mean(rating)) %>% 
  filter(n()>=100) %>% 
  ggplot(aes(b_u)) +
  geom_histogram(bins=30, color="black")

#lm(rating~as.factor(movieId)+as.factor(userId))
user_avgs <- test_set %>% 
  left_join(movie_avgs, by="movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u=mean(rating-mu-b_i))
head(user_avgs)

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by="movieId") %>% 
  left_join(user_avgs, by="userId") %>% 
  mutate(pred=mu+b_i+b_u) %>% 
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Movie + User Effect Model",
                          RMSE=model_2_rmse))
rmse_results %>% knitr::kable()


#Comprenhesion check. Recommendation systems.
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

str(movielens)
#ratings_per_movie <-movielens %>% group_by(movieId) %>% dplyr::count(movieId)
ratings_per_movie <- movielens %>% 
  group_by(movieId, year) %>% 
  summarize(n=n())

ratings_per_movie %>% group_by(year) %>% 
  summarize(med=median(n), year=first(year)) %>% 
  arrange(desc(med))

ratings_per_movie %>% 
  mutate(year=as.character(first(year))) %>% 
  ggplot(aes(year, n))+
  geom_boxplot()+
  scale_y_sqrt()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
?first()

movielens %>% filter(year>=1993 & year<=2018) %>% 
  group_by(movieId, title, year) %>% summarize(average_rating=mean(rating)) %>% 
  arrange(desc(average_rating)) %>% 
  top_n(25, average_rating)

movie_avg_rating <- movielens %>% filter(year>=1993 & year<=2018) %>% 
  group_by(movieId) %>% summarize(average_rating=mean(rating),year=first(year)) %>% 
  arrange(desc(average_rating)) %>% 
  slice(1:25)
str(movie_avg_rating)

index_movieId <- first(movielens$movieId[which(
    str_detect(movielens$title,"Shawshank Redemption"))])
movielens %>% 
  filter(year>=1993 & year <= 2018 & movieId==index_movieId) %>% 
  group_by(movieId) %>% summarize(avg_rating=mean(rating)) %>% 
  pull(avg_rating)
  
index_movieId <- first(movielens$movieId[which(
      str_detect(movielens$title,"Forrest Gump"))])
movielens %>% filter(year>=1993 & year <= 2018 & movieId==index_movieId) %>%
  group_by(movieId) %>% summarize(n_rating_year=n()/(2018-1993-1))

#####
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))  
######

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  arrange(desc(rate)) %>% 
  ggplot(aes(rate, rating)) +
  geom_point()+
  geom_smooth()

movielens <- mutate(movielens, date = as_datetime(timestamp)) 
head(movielens)

movielens %>% mutate(date=round_date(date, unit="week")) %>% 
  group_by(date) %>% summarize(avg_rating=mean(rating)) %>%
  ggplot(aes(date, avg_rating))+
  geom_point()+
  geom_smooth()

movielens %>% group_by(genres) %>% 
  filter(n()>=1000) %>% summarize(n=n(), avg_rating=mean(rating), se=sd(rating)/sqrt(n)) %>% 
  mutate(genres=reorder(genres, avg_rating)) %>% 
  ggplot(aes(x=genres, ymin=avg_rating-2*se, ymax=avg_rating+2*se))+
  geom_errorbar()+
  geom_point(aes(genres,avg_rating))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Regularization
library(dslabs)
library(tidyverse)
library(caret)
data("movielens")
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))

test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred
model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()             

lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()


#Comprehension Check, Regularization
options(digits=7)
?rt()
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
str(schools)
schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))
str(scores)

schools %>% top_n(10, score) %>% arrange(desc(score))
median(schools$size)
schools %>% top_n(10, score) %>% pull(size) %>% median(.)
schools %>% top_n(10,-score) %>% pull(size) %>% median(.)

str(schools)
top_10_quality <- schools %>% top_n(10, quality)
not_top_10_quality <- setdiff(schools, top_10_quality)
top_10_quality <- top_10_quality %>% mutate(top_q="TOP10Q") %>% select(id, top_q)
not_top_10_quality <- not_top_10_quality %>% mutate(top_q="NOT") %>% select(id, top_q)
top_10_q <-bind_rows(top_10_quality, not_top_10_quality) 

schools %>% left_join(top_10_q, by="id") %>% 
  ggplot(aes(size, score, color=top_q))+geom_point()

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)

overall <- mean(sapply(scores, mean))
schools <- schools %>% mutate(score = sapply(scores, mean))
alpha <- 25
str(schools)
school_eff_f <- function(x){sum(x-overall)}
school_eff <- sapply(scores, school_eff_f)
schools <- schools %>% 
  mutate(score_reg=sapply(scores,school_eff_f)/(size+alpha)+overall)
schools %>% top_n(10, score_reg) %>% 
  arrange(desc(score_reg))

#####
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))
#####

alpha <- 10:250
rmse <- sapply(alpha,function (alpha){
  score_alpha <-sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
  sqrt(mean((schools_quality-score_alpha)^2))
})
qplot(alpha, rmse)
which.min(rmse)+9
alpha[which.min(rmse)]

#######
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]
######

schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

alpha=135
score_reg_alpha <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg_alpha = score_reg_alpha) %>%
  top_n(10, score_reg_alpha) %>% arrange(desc(score_reg_alpha))

alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg_1 <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg_1 - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]


#Matrix Factorization
library(dplyr)
library(tidyverse)
library(caret)
library(dslabs)

train_small <- movielens %>% 
  group_by(movieId) %>% 
  filter(n()>=50 | movieId==3252) %>% ungroup() %>% #3252 is Scent of a Woman
  group_by(userId) %>% 
  filter(n()>=50) %>% ungroup()
head(train_small)

y <- train_small %>% 
  select(userId, movieId, rating) %>% 
  spread(movieId, rating) %>% 
  as.matrix()
y[1:20, 1:20]

rownames(y) <- y[,1]
y <- y[,-1]
colnames(y)<- with(movie_titles, title[match(colnames(y), movieId)])
y[1:20, 1:20]

y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))
y[1:20, 1:20]

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[, m_1], y[,m_2], xlab=m_1, ylab=m_2)

m_3 <- "Goodfellas"
qplot(y[,m_1], y[,m_3], xlab=m_1, y_lab=m_2)

m_4 <- "You've Got Mail"
m_5 <- "Sleepless in Seattle"
qplot(y[,m_4], y[,m_5], xlab=m_4, ylab=m_5)

options(digits=2)
cor(y[,c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>% 
  knitr::kable()

set.seed(1)
options(digit=2)
Q <- matrix(c(1,1,1,-1,-1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2, 0, -2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)
P

X <- jitter(P%*%t(Q))
X %>% knitr::kable(align="c")

cor(X)
t(Q) %>% knitr::kable(align="c")
P

set.seed(1)
options(digits=2)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1, 1, 1, -1, -1, -1),
           c(1, 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2, 0, -2), c(3, 5, 4)), 
           c(-1, 1, 1, 0, 0, 1, 1, 1, 0, -1, -1, -1))/2
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align ="c")

cor(X)

t(Q) %>% knitr::kable(align="c")
P

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[, six_movies]
cor(tmp, use="pairwise.complete")


#SVD and PCA
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

dim(pca$rotation)


plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2)) 
plot(var_explained)

library(ggrepel)
pcs <- data.frame(pca$rotation, name=colnames(y))
head(pcs)
pcs %>% ggplot(aes(PC1, PC2)) + geom_point() +
                 geom_text_repel(aes(PC1, PC2, label=name),
                                 data=filter(pcs,PC1 <- -0.1| PC1>0.1|PC2 <- 0.075|PC2>0.1))

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)
pcs %>% select(name,PC1) %>% arrange(desc(PC1)) %>% slice(1:10)
pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)
pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)


#Assesment
library(dplyr)
library(tidyverse)
library(caret)
library(dslabs)
library(ggrepel)
library(ggplot2)
set.seed(1987)
n <- 100
k <- 8
Sigma <- 64*matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3)
m <- MASS::mvrnorm(n, rep(0,3), Sigma)
m <- m[order(rowMeans(m), decreasing=TRUE),]
y <- m %x% matrix(rep(1,k), nrow=1)+matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math", k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))
str(y)

my_image <- function(x, zlim=range(x),...){
  colors=rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt="n", yaxt="n",
        xlab="", ylab="", col=colors, zlim, ...)
  abline(h=rows+0.5, v=cols+0.5)
  axis(side=1, cols, colnames(x), las=2)
}
my_image(y)

my_image(cor(y), zlim=c(1,1))
range(cor(y))
axis(side=2, 1:ncol(y), rev(colnames(y)), las=2)

s <- svd(y)
s
str(s)
names(s)
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y-y_svd))

ss_y <- colSums(y^2)
sum(ss_y)
ss_yv <- colSums((s$u%*%diag(s$d))^2)
sum(ss_yv)

y[1:10, 1:10]
y_svd[1:10, 1:10]

col <- 1:ncol(y)
qplot(col, ss_y, xlab="col index", ylab="ss_y")
qplot(col, ss_yv, xlab="col index", ylab="ss_yv")

sqrt_ss_yv <- sqrt(ss_yv)
qplot(s$d, sqrt_ss_yv, ylab="sqrt(ss_yv)", xlab="D")

yv <- s$u%*%diag(s$d)
sum(colSums((yv^2)[,1:3]))/sum(colSums(yv^2))

identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN="*"))   

student_avg <- rowMeans(y)
U1d1 <- sweep(s$u, 2, s$d, FUN="*")[,1]
qplot(U1d1, student_avg, xlab="U1d1", ylab="Student Avg")

dim(s$v)
s$v
image(1:24, 1:24, t(s$v))

resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)

plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)

resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))


#Comprehension Check: Dimension Reduction
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
str(tissue_gene_expression)
str(tissue_gene_expression$x)
x_tissue <- tissue_gene_expression$x
y_tissue <- tissue_gene_expression$y


pca_tissue <- prcomp(x_tissue)
str(pca_tissue$x)
names(pca_tissue)
summary(pca_tissue)
class(pca_tissue$x)

pca_tissue_dat <- data.frame(pca_tissue$x[,1:2], tissue_type= y_tissue)
str(pca_tissue)
head(pca_tissue)
pca_tissue %>% ggplot(aes(PC1, PC2, color=tissue_type)) + geom_point()

############
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()
###############

data.frame(pc_1=pca_tissue$x[,1], 
           obs_avg <- rowMeans(tissue_gene_expression$x),
                            tissue=tissue_gene_expression$y) %>% 
  ggplot(aes(pc_1, obs_avg, color=tissue))+geom_point()
data.frame(pc_1=pca_tissue$x[,1], 
           obs_avg <- rowMeans(tissue_gene_expression$x),
           tissue=tissue_gene_expression$y)cor(pc) %>% 
  cor(pc_1, obs_avg)

x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()


data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], pc_3 = pc$x[,3], pc_4 = pc$x[,4],
           pc_5 = pc$x[,5], pc_6 = pc$x[,6], pc_7 = pc$x[,7], pc_8 = pc$x[,8],
           pc_9 = pc$x[,9], pc_10 = pc$x[,10], 
           tissue = tissue_gene_expression$y) %>% 
  gather(pc, value, "pc_1":"pc_10") %>% ggplot(aes(pc, value, fill=tissue))+
  geom_boxplot()

#########
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}
#######

summary(pc)
str(summary(pc))
names(summary(pc))
str(summary(pc$importance))
plot(summary(pc)$importance[3,])


#Comprehension Check: Clustering
data("tissue_gene_expression")
tissue_x <- tissue_gene_expression$x
tissue_x <- sweep(tissue_x, 1, rowMeans(tissue_x))
tissue_xx <- tissue_gene_expression$x - rowMeans(tissue_gene_expression$x)
identical(tissue_x, tissue_xx)
d <- dist(tissue_x)
dim(as.matrix(d))
h <- hclust(d)
str(h)
plot(h, cex=0.65, main="", xlab="")

library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), 
        scale = "row", ColSideColors = colors)


#Final Assessment. Breast Cancer Project
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(RColorBrewer)
library(ggplot2)

data(brca)
str(brca)
mean(brca$y=="M")
colMeans(brca$x)
head(brca$x)
which.max(colMeans(brca$x))
which.min(colSds(brca$x))

brca_x_scaled <- sweep(brca$x, 2, colMeans(brca$x))
brca_x_scaled <- sweep(brca_x_scaled, 2, colSds(brca$x), FUN="/")
head(brca_x_scaled)
brca_x_scaled[,1]
colSds(brca_x_scaled)[1]
sd(brca_x_scaled[,1])
median(brca_x_scaled[,1])

#######
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")
sd(x_scaled[,1])
#######

d <- dist(brca_x_scaled)
dim(as.matrix(d))
as.matrix(d)[1:5, 1:5]
index_benign <- which(brca$y=="B")
index_malignant <- which(brca$y=="M")
mean(as.matrix(d)[1,index_benign])
mean(as.matrix(d)[1,index_malignant])

#########
d_samples <- dist(x_scaled)
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])
dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)
#########

d_features <- dist(t(brca_x_scaled))
heatmap(as.matrix(d_features), labRow=NA, labCol=NA)

h_features <- hclust(d_features)
plot(h_features, cex = 0.65, main = "", xlab = "")
groups <- cutree(h_features, k = 5)
split(names(groups), groups)

pca_brca <- prcomp(brca_x_scaled)
str(pca_brca)
pca_brca$sdev[1]^2/sum(pca_brca$sdev^2)
summary(pca_brca)
data.frame(pc_1=pca_brca$x[,1], pc_2=pca_brca$x[,2], BM=brca$y) %>% 
  ggplot(aes(pc_1, pc_2, color=BM))+
  geom_point()

data.frame(pc_1 = pca_brca$x[,1], pc_2 = pca_brca$x[,2], pc_3 = pca_brca$x[,3],
           pc_4 = pca_brca$x[,4], pc_5 = pca_brca$x[,5], pc_6 = pca_brca$x[,6], 
           pc_7 = pca_brca$x[,7], pc_8 = pca_brca$x[,8], pc_9 = pca_brca$x[,9],
           pc_10 = pca_brca$x[,10], BM = brca$y) %>% 
  gather(pc, value, "pc_1":"pc_10") %>% ggplot(aes(pc, value, fill=BM))+
  geom_boxplot()


#Breast Cancer Project part 3
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

mean(train_y=="B")
mean(test_y=="B")

set.seed(3)
k <- kmeans(train_x, centers=2)
k$centers
k$cluster
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}
cluster_prediction <- predict_kmeans(test_x, k) 
y_hat_cluster <- ifelse(cluster_prediction==1, "B", "M") %>% 
  factor(levels=levels(test_y))
cluster_prediction
test_y
mean(y_hat_cluster==test_y)

confusionMatrix(y_hat_cluster, test_y)

str(train_x)
train_brca <- data.frame(y=train_y, train_x)
test_brca <- data.frame(y=test_y, test_x)
str(train_brca)
train_logistic_brca <- caret::train(y~., method="glm", data=train_brca)
y_hat_brca_glm <- predict(train_logistic_brca, test_brca, type="raw")
confusionMatrix(y_hat_brca_glm, test_y)$overall["Accuracy"]
#########
train_glm <- train(train_x, train_y, method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)
#########

train_brca_lda <- caret::train(train_x, train_y, method="lda")
y_hat_brca_lda <- predict(train_brca_lda, test_x)
confusionMatrix(y_hat_brca_lda, test_y)$overall["Accuracy"]
mean(y_hat_brca_lda==test_y)
train_brca_qda <- caret::train(train_x, train_y, method="qda")
y_hat_brca_qda <- predict(train_brca_qda, test_x)
confusionMatrix(y_hat_brca_qda, test_y)$overall["Accuracy"]
mean(y_hat_brca_qda==test_y)

library(gam)

train_brca_loess <- train(train_x, train_y, method="gamLoess")
y_hat_brca_loess <- predict(train_brca_loess, test_x)
train_brca_loess$bestTune
train_brca_loess$results
confusionMatrix(y_hat_brca_loess, test_y)$overall["Accuracy"]
mean(y_hat_brca_loess==test_y)


#Breast Cancer Project Part 4
??caret
set.seed(7, sample.kind="Rounding")
train_brca_knn <- train(train_x, train_y, method="knn",
                        tuneGrid = data.frame(k=seq(3, 21, 2)))
plot(train_brca_knn)
train_brca_knn$bestTune
train_brca_knn$results
train_brca_knn$finalModel
y_hat_brca_knn <- predict(train_brca_knn, test_x)
confusionMatrix(y_hat_brca_knn, test_y)$overall["Accuracy"]
mean(y_hat_brca_knn==test_y)

########
# set.seed(7)
set.seed(7, sample.kind = "Rounding") # simulate R 3.5
tuning <- data.frame(k = seq(3, 21, 2))
train_knn <- train(train_x, train_y,
                   method = "knn", 
                   tuneGrid = tuning)
train_knn$bestTune

knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)
##############


library(randomForest)
set.seed(9)
train_brca_rf <- train(train_x, train_y, 
                       method="rf",
                       tuneGrid=data.frame(mtry=c(3, 5, 7, 9)),
                       importance=TRUE)
train_brca_rf$bestTune
train_brca_rf$results
train_brca_rf$finalModel
y_hat_brca_rf <- predict(train_brca_rf, test_x)
confusionMatrix(y_hat_brca_rf, test_y)$overall["Accuracy"]
mean(y_hat_brca_rf==test_y)
varImp(train_brca_rf)

?sapply
models <- c("k_means", "log_reg", "LDA", "QDA", "loess", "knn", "rf")
prediction_matrix_models <- data.frame(y_hat_cluster, y_hat_brca_glm,
                                       y_hat_brca_lda, y_hat_brca_qda, 
                                       y_hat_brca_loess, y_hat_brca_knn,
                                       y_hat_brca_rf)
colnames(prediction_matrix_models) <- models
prediction_matrix_models

votes <- rowMeans(prediction_matrix_models=="B")
y_hat_brca_ensemble <-  as.factor(ifelse(votes>0.5, "B", "M"))

confusionMatrix(y_hat_brca_ensemble, test_y)$overall["Accuracy"]

prediction_matrix_models <- cbind(prediction_matrix_models, y_hat_brca_ensemble)

brca_model_acc <- apply(prediction_matrix_models, 2, function(i) {
  acc <- confusionMatrix(as.factor(i), test_y)$overall["Accuracy"]
})
t(brca_model_acc)


?files
