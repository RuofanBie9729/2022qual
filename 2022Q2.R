S1 <- read.csv("S1.csv")[,c("PAIN_INTENSITY_AVERAGE", 
"PROMIS_PHYSICAL_FUNCTION", "PROMIS_DEPRESSION", 
"PROMIS_ANXIETY", "PROMIS_SLEEP_DISTURB_V1_0", "PROMIS_PAIN_INTERFERENCE", 
"GH_MENTAL_SCORE", "GH_PHYSICAL_SCORE", "AGE_AT_CONTACT", "BMI", 
"PAIN_INTENSITY_AVERAGE1", "BODYREGIONSUM", "BODYREGIONSUM1", "PAT_SEX", 
"race_cat", "CCI_bin", "medicaid_bin", "PDtotal", "RESP_HIGH")]

dim(S1)

summary(S1)
library(dplyr)
S1 <- S1 %>% mutate_all(~replace(., .=="", NA))

summary(S1)
head(S1, 6)

###### handling missing data
library(mice)
baseline_covariate <- S1[, !(names(S1) %in% c("PAIN_INTENSITY_AVERAGE1", 
"BODYREGIONSUM1", "RESP_HIGH"))]
imputed_Data <- mice(baseline_covariate, maxit=5)
completeData <- complete(imputed_Data)
completeData[is.na(completeData$race_cat),"race_cat"] <- sample(completeData[!is.na(completeData$race_cat),"race_cat"], 141, replace=T)
completeData[is.na(completeData$medicaid_bin),"medicaid_bin"] <- sample(completeData[!is.na(completeData$medicaid_bin),"medicaid_bin"], 300, replace=T)
head(completeData, 6)
S1Data <- cbind(completeData, S1[, c("PAIN_INTENSITY_AVERAGE1", "BODYREGIONSUM1", 
"RESP_HIGH")])
colnames(S1Data) <- c("pain_intensity0", "physical_function", "depression", 
"anxiety", "sleep_disturb", "pain_inference", "global_mental", 
"global_physical", "age", "bmi", "body_region0",  "gender", "race", "cci", 
"medicaid", "pain_detect", "pain_intensity1", "body_region1", "resp_high")
Data_inten <- na.omit(S1Data[, 1:17])
Data_inten$risk <- rep(1, dim(Data_inten)[1])
Data_inten[which(Data_inten$pain_intensity1<Data_inten$pain_intensity0), "risk"] <- 0

Data_region <- na.omit(S1Data[, c(1:16, 18)])
Data_region$risk <- rep(1, dim(Data_region)[1])
Data_region[which(Data_region$body_region1<Data_region$body_region0), "risk"] <- 0

Data_resp <- na.omit(S1Data[, c(1:16, 19)])

####### exploratory data analysis
continuous_name <- c("pain_intensity0", "physical_function", "depression", 
"anxiety", "sleep_disturb", "pain_inference", "global_mental", 
"global_physical", "age", "bmi", "body_region0", "pain_detect")

categorical_name <- c("gender", "race", "cci", "medicaid")

continue_var <- S1Data[, continuous_name]
cate_var <- S1Data[,categorical_name]

win.graph(w=16, h=12)
par(mfrow=c(4, 3))
for(i in 1:12){
  hist(continue_var[, i], xlab=colnames(continue_var)[i], 
    main=paste("Histogram of", colnames(continue_var)[i]))
}

win.graph(w=16, h=12)
par(mfrow=c(4, 3))
for(i in 1:12){
  qqnorm(continue_var[, i], xlab=colnames(continue_var)[i], 
    main=paste("QQplot of", colnames(continue_var)[i]), pch = 1, frame = FALSE)
  qqline(continue_var[, i], col = "steelblue", lwd = 2, xlab=colnames(continue_var)[i], 
    main=paste("QQplot of", colnames(continue_var)[i]))
}

library(PerformanceAnalytics)
win.graph(w=12, h=12)
chart.Correlation(continue_var[, c("physical_function", "depression", 
"anxiety", "pain_inference", "global_mental", 
"global_physical")], histogram = TRUE, method = "pearson")


for(i in 1:4){
  print(table(cate_var[, i])/21658)
}

table(Data_inten$risk)
table(Data_region$risk)
table(Data_resp$resp_high)

apply(S1Data[,continuous_name], 2, mean)

apply(Data_inten[which(Data_inten$risk==1),continuous_name], 2, mean)
apply(Data_inten[which(Data_inten$risk==0),continuous_name], 2, mean)

apply(Data_region[which(Data_region$risk==1),continuous_name], 2, mean)
apply(Data_region[which(Data_region$risk==0),continuous_name], 2, mean)

apply(Data_resp[which(Data_resp$resp_high=="no"),continuous_name], 2, mean)
apply(Data_resp[which(Data_resp$resp_high=="yes"),continuous_name], 2, mean)

Data_resp[which(Data_resp$resp_high=="no"),"resp_high"] <- 1
Data_resp[which(Data_resp$resp_high=="yes"),"resp_high"] <- 0

Data_resp[,"resp_high"] <- as.numeric(Data_resp[,"resp_high"])

for(i in  c("pain_intensity0", "body_region0", "pain_detect", 
 "sleep_disturb", "global_mental", "global_physical", "age", "bmi" )
){
  fit <- glm(Data_inten$risk~Data_inten[,i], family="binomial")
  print(summary(fit))
}

for(i in  c("pain_intensity0", "body_region0", "pain_detect", 
 "sleep_disturb", "global_mental", "global_physical", "age", "bmi" )
){
  fit <- glm(Data_region$risk~Data_region[,i], family="binomial")
  print(summary(fit))
}

for(i in  c("pain_intensity0", "body_region0", "pain_detect", 
 "sleep_disturb", "global_mental", "global_physical", "age", "bmi" )
){
  fit <- glm(Data_resp$resp_high~Data_resp[,i], family="binomial")
  print(summary(fit))
}

lapply(apply(Data_inten[which(Data_inten$risk==1),categorical_name], 2, table), prop.table)
lapply(apply(Data_inten[which(Data_inten$risk==0),categorical_name], 2, table), prop.table)

lapply(apply(Data_region[which(Data_region$risk==1),categorical_name], 2, table), prop.table)
lapply(apply(Data_region[which(Data_region$risk==0),categorical_name], 2, table), prop.table)


lapply(apply(Data_resp[which(Data_resp$resp_high==1),categorical_name], 2, table), prop.table)
lapply(apply(Data_resp[which(Data_resp$resp_high==0),categorical_name], 2, table), prop.table)

Data_inten[which(Data_inten$race=="OTHER"),"race"] <- "BLACK"
Data_region[which(Data_region$race=="OTHER"),"race"] <- "BLACK"
Data_resp[which(Data_resp$race=="OTHER"),"race"] <- "BLACK"

for(i in  c("gender", "race", "medicaid", "cci")){
  fit <- glm(Data_inten$risk~Data_inten[,i], family="binomial")
  print(summary(fit))
}

for(i in   c("gender", "race", "medicaid", "cci")){
  fit <- glm(Data_region$risk~Data_region[,i], family="binomial")
  print(summary(fit))
}

for(i in   c("gender", "race", "medicaid", "cci")){
  fit <- glm(Data_resp$resp_high~Data_resp[,i], family="binomial")
  print(summary(fit))
}

###### change to integer
S1Data[which(S1Data$race=="OTHER"), "race"] <- "BLACK"
S1Data[which(S1Data$resp_high=="no"),"resp_high"] <- 1
S1Data[which(S1Data$resp_high=="yes"),"resp_high"] <- 0

S1Data[,"resp_high"] <- as.numeric(S1Data[,"resp_high"])
S2Data <- S1Data[,c("pain_intensity0", "sleep_disturb", "global_mental", 
"global_physical", "age", "bmi", "body_region0",  "gender", "race", "cci", 
"medicaid", "pain_detect", "pain_intensity1", "body_region1", "resp_high")]

for(i in c("sleep_disturb", "global_mental", "global_physical", "age", "bmi", "pain_detect")){
  S2Data[, i] <- round(S2Data[,i]/10)
}

Data_inten <- na.omit(S2Data[, 1:13])
Data_inten$risk <- rep(1, dim(Data_inten)[1])
Data_inten[which(Data_inten$pain_intensity1<Data_inten$pain_intensity0), "risk"] <- 0

Data_region <- na.omit(S2Data[, c(1:12, 14)])
Data_region$risk <- rep(1, dim(Data_region)[1])
Data_region[which(Data_region$body_region1<Data_region$body_region0), "risk"] <- 0

Data_resp <- na.omit(S2Data[, c(1:12, 15)])

for(i in 1:nrow(Data_inten)){
  if(Data_inten[i, "body_region0"]<2){
     Data_inten[i, "body_region0"] <- 1
  }else if(Data_inten[i, "body_region0"]<5){
     Data_inten[i, "body_region0"] <- 2
  }else if(Data_inten[i, "body_region0"]<8){
     Data_inten[i, "body_region0"] <- 3
  }else if(Data_inten[i, "body_region0"]<20){
     Data_inten[i, "body_region0"] <- 4
  }else{
     Data_inten[i, "body_region0"] <- 5
  }
}

for(i in 1:nrow(Data_region)){
  if(Data_region[i, "body_region0"]<2){
     Data_region[i, "body_region0"] <- 1
  }else if(Data_region[i, "body_region0"]<5){
     Data_region[i, "body_region0"] <- 2
  }else if(Data_region[i, "body_region0"]<8){
     Data_region[i, "body_region0"] <- 3
  }else if(Data_region[i, "body_region0"]<20){
     Data_region[i, "body_region0"] <- 4
  }else{
     Data_region[i, "body_region0"] <- 5
  }
}

for(i in 1:nrow(Data_resp)){
  if(Data_resp[i, "body_region0"]<2){
     Data_resp[i, "body_region0"] <- 1
  }else if(Data_resp[i, "body_region0"]<5){
     Data_resp[i, "body_region0"] <- 2
  }else if(Data_resp[i, "body_region0"]<8){
     Data_resp[i, "body_region0"] <- 3
  }else if(Data_resp[i, "body_region0"]<20){
     Data_resp[i, "body_region0"] <- 4
  }else{
     Data_resp[i, "body_region0"] <- 5
  }
}
######### statistical analysis
dim(Data_inten)
dim(Data_region)
dim(Data_resp)

##### data_inten
set.seed(1)
test_id <- sample(dim(Data_inten)[1], 738)
test_set <- Data_inten[test_id, ]
train_set <- Data_inten[-test_id, ]

### two-stage stepwise
full0 <- glm(risk ~ pain_intensity0 + sleep_disturb + global_mental + global_physical + age + bmi + 
  as.factor(body_region0) + gender + race + cci + medicaid + pain_detect, data=train_set, family="binomial")
sub0 <- step(full0)

full2 <- glm(risk ~ age + pain_detect + as.factor(body_region0) + medicaid + sleep_disturb + global_mental + 
  race + pain_intensity0 + I(age^2) + I(pain_detect^2) + I(sleep_disturb^2) + I(global_mental^2) + 
  I(pain_intensity0^2) + age:pain_detect + age:as.factor(body_region0) + age:medicaid + age:sleep_disturb + 
  age:global_mental + age:race + age:pain_intensity0 + pain_detect:as.factor(body_region0) + pain_detect:medicaid + 
  pain_detect:sleep_disturb + pain_detect:global_mental + pain_detect:race + pain_detect:pain_intensity0 + 
  as.factor(body_region0):medicaid + as.factor(body_region0):sleep_disturb + as.factor(body_region0):global_mental + 
  as.factor(body_region0):race + as.factor(body_region0):pain_intensity0 + medicaid:sleep_disturb + 
  medicaid:global_mental + medicaid:race + medicaid:pain_intensity0 + sleep_disturb:global_mental + 
  sleep_disturb:race + sleep_disturb:pain_intensity0 + global_mental:race + global_mental:pain_intensity0 + 
  race:pain_intensity0, data=train_set, family="binomial")
sub2 <- step(full2)
summary(sub2)
final <- glm(risk ~  pain_detect + as.factor(body_region0) + medicaid + global_mental + pain_intensity0 + 
  pain_detect:global_mental, data=train_set, family="binomial")
summary(final)
plot(final, which = 4, id.n = 3)
car::vif(final)
exp(confint(final))

### cross validation
logit <- function(x){log(x/(1-x))}
expit <- function(x){exp(x)/(1+exp(x))}
shuffle_id <- sample(dim(train_set)[1], dim(train_set)[1])
train_set <- train_set[shuffle_id, ]
Acc <- NULL
for(j in seq(0.1, 0.9, 0.1)){
acc <- NULL
for(i in 1:10){
  valid_id <- c(((i-1)*640+1):(i*640))
  valid <- train_set[valid_id,]
  train <- train_set[-valid_id,]
  logistic  <- glm(risk ~  pain_detect + as.factor(body_region0) + medicaid + global_mental + pain_intensity0 + 
  pain_detect:global_mental, data=train, family="binomial")
  coef <- round(coefficients(logistic), 1)
  predictX <- model.matrix(logistic, data=valid)
  prob <- expit(predictX %*% coef)
  predictY <- ifelse(prob>=j, 1, 0)
  acc <- c(acc, sum(diag(table(predictY, valid$risk)))/640)
}
Acc <- rbind(Acc, acc)
}
apply(Acc, 1, mean)
apply(Acc, 1, sd)

coef <- round(coefficients(final), 1)
predictX <- model.matrix(final, data=test_set)
prob <- expit(predictX %*% coef)
library(verification)
roc.plot(test_set$risk, prob, xlab="False Positive Rate", ylab="True Positive Rate")
predictedY <- ifelse(prob>=0.5, 1, 0)
table(predictedY, test_set$risk)

##### data_region
set.seed(1)
test_id <- sample(dim(Data_region)[1], 710)
test_set <- Data_region[test_id, ]
train_set <- Data_region[-test_id, ]

### two-stage stepwise
full0 <- glm(risk ~ pain_intensity0 + sleep_disturb + global_mental + global_physical + age + bmi + 
  as.factor(body_region0) + gender + race + cci + medicaid + pain_detect, data=train_set, family="binomial")
sub0 <- step(full0)

full2 <- glm(risk ~ bmi + global_mental + age + as.factor(body_region0) + I(bmi^2) + I(global_mental^2) + I(age^2) + 
  bmi:global_mental + bmi:age + bmi:as.factor(body_region0) + global_mental:age + 
  global_mental:as.factor(body_region0) + age:as.factor(body_region0), data=train_set, family="binomial")
sub2 <- step(full2)
summary(sub2)
final <- glm(risk ~ bmi + age + global_mental + as.factor(body_region0) + I(age^2) + bmi:age + bmi:global_mental, 
  data=train_set, family="binomial")
summary(final)
plot(final, which = 4, id.n = 3)
car::vif(final)
exp(confint(final))

### cross validation
shuffle_id <- sample(dim(train_set)[1], dim(train_set)[1])
train_set <- train_set[shuffle_id, ]
Acc <- NULL
for(j in seq(0.1, 0.9, 0.1)){
acc <- NULL
for(i in 1:10){
  valid_id <- c(((i-1)*630+1):(i*630))
  valid <- train_set[valid_id,]
  train <- train_set[-valid_id,]
  logistic  <- glm(risk ~  bmi + age + global_mental + as.factor(body_region0) + I(age^2) + bmi:age + bmi:global_mental, data=train, family="binomial")
  coef <- round(coefficients(logistic), 1)
  predictX <- model.matrix(logistic, data=valid)
  prob <- expit(predictX %*% coef)
  predictY <- ifelse(prob>=j, 1, 0)
  acc <- c(acc, sum(diag(table(predictY, valid$risk)))/630)
}
Acc <- rbind(Acc, acc)
}
apply(Acc, 1, mean)
apply(Acc, 1, sd)

coef <- c(17, 0.03, 0.12, 0.06, -16, 17, -17, -18, -0.03, 0.04, -0.05)
predictX <- model.matrix(final, data=test_set)
prob <- expit(predictX %*% coef)
library(verification)
roc.plot(test_set$risk, prob, xlab="False Positive Rate", ylab="True Positive Rate")
predictY <- ifelse(prob>=0.7, 1, 0)
table(predictY, test_set$risk)


##### data_resp
#Data_resp <- na.omit(S2Data[, c(1:12, 15)])
#Data_resp$body_region0 <- round(log(Data_resp$body_region0, 2))

set.seed(1)
test_id <- sample(dim(Data_resp)[1], 738)
test_set <- Data_resp[test_id, ]
train_set <- Data_resp[-test_id, ]

full0 <- glm(resp_high ~ pain_intensity0 + sleep_disturb + global_mental + global_physical + age + bmi + 
  body_region0 + gender + race + cci + medicaid + pain_detect, data=train_set, family="binomial")
sub0 <- step(full0)

full2 <- glm(resp_high ~ pain_detect + sleep_disturb + global_physical + bmi + medicaid + body_region0 + 
pain_intensity0 + global_mental + I(pain_detect^2) + I(sleep_disturb) + I(global_physical^2) + I(bmi^2) + 
I(body_region0^2) + I(pain_intensity0^2) + I(global_mental^2) + pain_detect:sleep_disturb + 
pain_detect:global_physical + pain_detect:bmi + pain_detect:medicaid + pain_detect:body_region0 + 
pain_detect:pain_intensity0 + pain_detect:global_mental + sleep_disturb:global_physical + 
sleep_disturb:bmi + sleep_disturb:medicaid + sleep_disturb:body_region0 + sleep_disturb:pain_intensity0 + 
sleep_disturb:global_mental + global_physical:bmi + global_physical:medicaid + global_physical:body_region0 + 
global_physical:pain_intensity0 + global_physical:global_mental + bmi:medicaid + bmi:body_region0 + 
bmi:pain_intensity0 + bmi:global_mental + medicaid:body_region0 + medicaid:pain_intensity0 + 
medicaid:global_mental + body_region0:pain_intensity0 + body_region0:pain_intensity0 + 
pain_intensity0:global_mental, data=train_set, family="binomial")
sub2 <- step(full2)

final <- glm(resp_high ~ pain_detect + sleep_disturb + global_physical + bmi + medicaid + body_region0 + 
pain_intensity0 + global_mental + I(pain_intensity0^2) + sleep_disturb:global_physical + sleep_disturb:bmi, 
data=train_set, family="binomial")


### two-stage stepwise
full0 <- glm(resp_high ~ pain_intensity0 + sleep_disturb + global_mental + global_physical + age + bmi + 
  as.factor(body_region0) + gender + race + cci + medicaid + pain_detect, data=train_set, family="binomial")
sub0 <- step(full0)

full2 <- glm(resp_high ~ pain_intensity0 + pain_detect + sleep_disturb + global_physical + bmi + as.factor(body_region0) + 
  medicaid + global_mental + I(pain_intensity0) + I(pain_detect^2) + I(sleep_disturb^2) + I(global_physical^2) + I(global_mental^2) + 
  I(bmi^2) + pain_intensity0:sleep_disturb + pain_intensity0:global_physical + pain_intensity0:bmi + 
  pain_intensity0:as.factor(body_region0) + pain_intensity0:medicaid + pain_intensity0:global_mental + 
  pain_detect:sleep_disturb + pain_detect:global_physical + pain_detect:bmi + pain_intensity0:pain_detect + 
  pain_detect:as.factor(body_region0) + pain_detect:medicaid + pain_detect:global_mental + 
  sleep_disturb:global_physical + sleep_disturb:bmi + sleep_disturb:as.factor(body_region0) + 
  sleep_disturb:medicaid + sleep_disturb:global_mental + global_physical:bmi + 
  global_physical:as.factor(body_region0) + global_physical:medicaid + global_physical:global_mental + 
  bmi:as.factor(body_region0) + bmi:medicaid + bmi:global_mental + as.factor(body_region0):medicaid + 
  as.factor(body_region0):global_mental + medicaid:global_mental, data=train_set, family="binomial")
sub2 <- step(full2)
summary(sub2)
final <- glm(resp_high ~ pain_intensity0 + pain_detect + sleep_disturb + global_physical + global_mental + bmi + 
  as.factor(body_region0) + medicaid + pain_intensity0:global_physical +  sleep_disturb:global_physical + 
  sleep_disturb:bmi,  data=train_set, family="binomial")
summary(final)
#final <- glm(resp_high ~ pain_intensity0 + global_physical + global_mental + medicaid + 
#  pain_intensity0:global_physical,  data=train_set, family="binomial")

#a <- predict(final, test_set, type="response")
#summary(a[test_set$resp_high==1])
#predictY <- ifelse(a>=0.55, 1, 0)
#table(predictY, train_set$resp_high)

exp(coefficients(final))
plot(final, which = 4, id.n = 3)
car::vif(final)
exp(confint(final))

### cross validation
shuffle_id <- sample(dim(train_set)[1], dim(train_set)[1])
train_set <- train_set[shuffle_id, ]
Acc <- NULL
for(j in seq(0.1, 0.9, 0.1)){
acc <- NULL
for(i in 1:10){
  valid_id <- c(((i-1)*640+1):(i*640))
  valid <- train_set[valid_id,]
  train <- train_set[-valid_id,]
  logistic  <- glm(resp_high ~ pain_intensity0 + pain_detect + sleep_disturb + global_physical + global_mental + bmi + 
  as.factor(body_region0) + medicaid + pain_intensity0:global_physical +  sleep_disturb:global_physical + 
  sleep_disturb:bmi, data=train, family="binomial")
  coef <- round(coefficients(logistic), 2)
  predictX <- model.matrix(logistic, data=valid)
  prob <- expit(predictX %*% coef)
  predictY <- ifelse(prob>=j, 1, 0)
  acc <- c(acc, sum(diag(table(predictY, valid$resp_high)))/640)
}
Acc <- rbind(Acc, acc)
}
apply(Acc, 1, mean)
apply(Acc, 1, sd)


coef <- round(coefficients(final), 2)
predictX <- model.matrix(final, data=test_set)
prob <- expit(predictX %*% coef)
library(verification)
roc.plot(test_set$resp_high, prob, xlab="False Positive Rate", ylab="True Positive Rate")
predictY <- ifelse(prob>=0.5, 1, 0)
table(predictY, test_set$resp_high)

