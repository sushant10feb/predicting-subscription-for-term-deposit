#load in the libraries

library(readxl)
library(caret)
library(dplyr)
library(tidyverse)
library(corrplot)
library(eadrm)
library(ggplot2)
library(Hmisc)
library(lmtest)
library(funModeling)
library(ggpubr)
library("ggnewscale")
library(stats)
library(pscl)











#Read in the Data

bank <- read_excel("bank.xlsx")


#descriptive stastics

summary(bank)


#converting into factor

str(bank)
bank1 <- bank %>% mutate_if(is.character, as.factor)
str(bank1)

#checking factors levels for subscription
levels(bank1$subscribed)


#Dealing with data quality issues
head(bank1)

#solved Data quality issues for Job (extra full stop for admin)
bank1$job<-gsub(".", "", bank1$job, fixed = TRUE)

#Age
boxplot(bank1$age)
bank1 %>% mutate(age=replace(age, age<=17, NA),
                 age=replace(age, age>100,NA))-> bank2_clean

boxplot(bank2_clean$age)

#removing NA from age
bank2_clean%>%filter(!is.na(age)) ->bank3_clean
sum(is.na(bank3_clean$age))

#Month 

head(bank3_clean$month)
bank3_clean$month <- recode_factor(bank3_clean$month, "mar" = "march")
head(bank3_clean$month)

#pdays (No of days passed)
#check for blanks

sum(is.na(bank3_clean$pdays))

#Removing blanks
bank3_clean %>% filter(!is.na(pdays)) ->bank4_clean
sum(is.na(bank4_clean$pdays))


#checking for overall missing values
colSums(is.na(bank4_clean))


#used mutate function again to change it to factor

bank5 <- bank4_clean %>% mutate_if(is.character, as.factor)

str(bank4_clean)

#check levels for subscription
levels(bank5$subscribed)


## Establishing relationship and Hypothesis ##

chisq.test(bank5$month,bank5$subscribed,correct = FALSE) #for two categorical data
t.test(age ~ subscribed, data = bank5) # for one numeric and another categorical
t.test(euribor3m ~ subscribed, data = bank5)
t.test(campaign~ subscribed, data = bank5)
t.test(cons.price.idx ~ subscribed, data = bank5)
chisq.test(bank5$job , bank5$subscribed, correct = FALSE)


#visualizations for relationship between different variables
library(ggplot2)
library("ggnewscale") 

#Boxplot visualization of numeric
bank5 %>% 
  select_if(is.numeric) %>% 
  pivot_longer(cols=c(1:9),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(y=value))+geom_boxplot()+facet_wrap(~variable,scales="free")


#Histogram visualization of character var
bank5 %>% 
  select_if(is.numeric) %>% 
  pivot_longer(cols=c(1:9),names_to = "variable",values_to = "value") %>% 
  ggplot(aes(y=value))+geom_histogram()+facet_wrap(~variable,scales="free")+coord_flip()+theme_minimal()


#no of people subscribed to Term deposit
ggplot(bank5,aes(x=subscribed))+geom_bar() +
scale_color_gradient(low = "green", high = "blue") +
  new_scale_color() + 
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "No of people suscribed to term deposit ", x = "subscribed", y = " No of people") +
  theme_minimal() 





##density plot of subscribed, age, job, marital status
ggplot(bank5,aes(x=age,fill=subscribed))+theme_bw()+facet_wrap(marital ~ job)+geom_density(alpha=0.9)


#density plot of subscribed, age, education, marital status

ggplot(bank5,aes(x=age,fill=subscribed))+theme_bw()+facet_wrap(marital ~ education)+geom_density(alpha=0.9)


#bar chart between subscription and job

ggplot(bank5, 
       aes(x = subscribed, 
           fill = job)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")


#cross plotting between different variables with dependent variable (subscribed)  

cross_plot(data=bank5, target= "subscribed")



####set seed
set.seed(40386815)


#create index and split the data
index <- createDataPartition(bank5$subscribed, p=0.8, list = FALSE, times = 1)
train <- bank5[index,]
test <- bank5[-index,]

# Regression MODEL 1, model on train data

formula <- subscribed ~ age + job + contact

model1 <- glm(formula, data = train, family = "binomial")

summary(model1)

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}

logisticPseudoR2s(model1)


#model 2
formula <- subscribed ~ education + euribor3m

model2 <- glm(formula, data = train, family = "binomial")
summary(model2)

#model 3

formula <- subscribed ~ campaign + default + nr.employed
model3 <- glm(formula, data = train, family = "binomial")
summary(model3)


#model 4 

formula <- subscribed ~ cons.conf.idx + cons.price.idx + emp.var.rate + loan
model4 <- glm(formula, data = train, family = "binomial")
summary(model4)


#model 5

formula <- subscribed ~ campaign + poutcome + housing
model5 <- glm(formula, data = train, family = "binomial")
summary(model5)


#Final Multivariate Regression Model (Model 6)

formula <- subscribed ~ poutcome + housing + cons.conf.idx + emp.var.rate + cons.price.idx + default + euribor3m + campaign + contact + nr.employed + age + marital + education + month + pdays  
model6 <- glm(formula, data = train, family = "binomial")
summary(model6)



#check the Pseudo R squared value on the training data for all the models
logisticPseudoR2s(model1)
logisticPseudoR2s(model2)
logisticPseudoR2s(model3)
logisticPseudoR2s(model4)
logisticPseudoR2s(model5)
logisticPseudoR2s(model6)



## to get odds ratio ##
exp(model1$coefficients) 
exp(model2$coefficients) 
exp(model3$coefficients) 
exp(model4$coefficients) 
exp(model5$coefficients) 
exp(model6$coefficients) 


##check assumptions ##

#Model 1
train$rstandard <- rstandard(model1)
sum(train$rstandard > 1.96)  # is more than 5% of the observations of the train data 

#Model 2
train$rstandard <- rstandard(model2)
sum(train$rstandard > 1.96)

#Model 3
train$rstandard <- rstandard(model3)
sum(train$rstandard > 1.96)

#Model 4
train$rstandard <- rstandard(model4)
sum(train$rstandard > 1.96)

#Model 5
train$rstandard <- rstandard(model5)
sum(train$rstandard > 1.96)

#Model 6
train$rstandard <- rstandard(model6)
sum(train$rstandard > 1.96)


##Cooks Distance ##

train$cook <- cooks.distance(model6) #created cook coloumn to check for outliers or faults in model if more than 1.
sum(train$cook > 1) #if zero observations then its good.

## check multicolineraty ##

library(caret) #Its classification of regression 
library(rms)
vif(model6)
?vif

#For many variables the vif > 3 then there is multicoolineraty or independent variable are highly coorelated
# like all economic indicators like emp variance rate, euribor3m are highly coorelated, so we can drop.
#as its evaluated on test data so wont affect predective accuracy


#linearity of logit (means to check if there is a linear relationship b/w each continous input variable and log of target variable) )
#Its only for numeric value

## create interaction ##

train$eurInt <- log(train$euribor3m)*train$euribor3m # new coloumn tenInt creates log of variable * the variable itself
summary(train$eurInt)

train$ageInt <- log(train$age)*train$age
summary(train$ageInt)


#add in to formula again
formula <- subscribed ~  poutcome + housing + cons.conf.idx + emp.var.rate + cons.price.idx + default + euribor3m + campaign + contact + nr.employed + age + marital + education + month + pdays + eurInt + ageInt
model7 <- glm(formula, data = train, family = "binomial")
summary(model7) #we can see eurInt and ageInt are positive means we have violated the assumptions of linearity of logit. 



#Making Predictions on Test Data
predict <- predict(model6, test, type = "response") #response to return predicted probabilities , test is data set
predict
predict <- ifelse(predict >0.5, 'yes', 'no') # to return it in yes or no
postResample(predict, test$subscribed) # it gives us accuracy and kappa
missing_classerr <- predict != test$subscribed

## creating data frame ##

train$predict <- fitted(model6)
head(data.frame(train$predict, train$subscribed))




#confusion matrix 1
postResample(pred = predict, obs = test$subscribed)
table(test$subscribed, predict)


## Durbin-watson test ##

library(lmtest)
dwtest(model6)
# A result of 1.89 which lies between 1.5 to 2.5


#Few Final Plotting
plot(model6)





















