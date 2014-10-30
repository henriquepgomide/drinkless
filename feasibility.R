# WHO FEASIBILITY STUDY ============================================================================

# Objectives --
# 1 - Compare alcohol consumption after 6 weeks among - intention to treat and treatment completers.
# 2 - Create a model to predict success.

# Libraries
library(car)      # Recode
library(lattice)  # for graphs
library(caret)    # Modelling
library(ez)       # for traditional anova ezANOVA
library(reshape2) # Melt function
library(ggplot2)  # Graphs
library(nnet)     # Regression

# Data import
drinkLess_cc  <- read.csv("banco_artigo.csv", na.strings = c("NA",99),  dec=",")
drinkLess_cc  <- drinkLess_cc[,-c(2,7,8,9,10,11,12,13,14,15)]
drinkLess_full  <- read.csv("banco_artigo_full.csv", na.strings = c("NA",99),  dec=",")

# Final dataframe
drinkLess  <- merge(drinkLess_full, drinkLess_cc, by = "client_id", all = TRUE)

## Clean unused objects
rm(drinkLess_cc); rm(drinkLess_full)

# Data preprocessing

## Recode vars to numeric
for (i in c(3,5,9,10,11,12,13,15)) {
  drinkLess[, i]  <- as.numeric(drinkLess[,i])
}

## Removing outliers

### alcohol_pre
drinkLess$alcohol_pre[scale(drinkLess$alcohol_pre) > 3 | scale(drinkLess$alcohol_pre) < -3 ]  <- NA

### alcohol_pos
drinkLess$alcohol_pos[scale(drinkLess$alcohol_pos) > 3 | scale(drinkLess$alcohol_pos) < -3 ]  <- NA

### login_times
drinkLess$login_times[scale(drinkLess$login_times) > 3 | scale(drinkLess$login_times) < -3 ]  <- NA

### ave_login_time
drinkLess$ave_login_time[scale(drinkLess$ave_login_time) > 3 | scale(drinkLess$ave_login_time) < -3 ]  <- NA

### sum_login
drinkLess$sum_login[scale(drinkLess$sum_login) > 3 | scale(drinkLess$sum_login) < -3 ]  <- NA

## Fix reduced_factor
drinkLess$reduced_factor  <- drinkLess$alcohol_pos - drinkLess$alcohol_pre
drinkLess$reduced_factor[drinkLess$reduced_factor < 0]  <- "-"
drinkLess$reduced_factor[drinkLess$reduced_factor > 0]  <- "+"

## Recode vars to appropriate factors
### audit
drinkLess$audit  <-  Recode(drinkLess$audit, "0 = 'Low risk'; 1 = 'Risk'; 2 = 'Dependence'")

### gender
drinkLess$gender  <-  Recode(drinkLess$gender, "0 = 'Men'; 1 = 'Women'")

### educational level
drinkLess$school  <-  Recode(drinkLess$school, "0 = 'High School'; 1 = 'College'")

### rcq
drinkLess$rcq_factor  <-  Recode(drinkLess$rcq_factor, "0 = 'Pre'; 1 = 'Con'; 2 = 'Action'")

### completed
drinkLess$completed  <-  Recode(drinkLess$completed, "0 = 'Yes'; 1 = 'No'")

# Save data.frame
write.csv(drinkLess, "drinkless_R.csv")

### EXPLORATORY ANALYSIS ====

drinkLess  <- read.csv("drinkless_R.csv")

# Atrition rate after 6 weeks 
table(drinkLess$completed) # 85.6%

  
# TREATMENT COMPLETERS  ----
  
# Completers
treatComp  <- subset(drinkLess, drinkLess$completed == "Yes")  
  
# Program Evaluation
mean(treatComp$prog_evaluation)  # 8.45
sd(treatComp$prog_evaluation, na.rm = TRUE)  # 1.60
  
# pre
histogram(~ alcohol_pre | audit * gender, data = treatComp)
qqmath(~ alcohol_pre | audit * gender, data = treatComp)

# pos
histogram(~ alcohol_pos | audit * gender, data = treatComp)
qqmath(~ alcohol_pos | audit * gender, data = treatComp)

# average login time
histogram(~ ave_login_time | audit * gender, data = treatComp)
qqmath(~ ave_login_time | audit * gender, data = treatComp)

# sum logins
histogram(~ sum_login | audit * gender, data = treatComp)
qqmath(~ sum_login | audit * gender, data = treatComp)

# login times
histogram(~ login_times | audit * gender, data = treatComp)
qqmath(~ login_times | audit * gender, data = treatComp)

# Graph analyses suggest the partition of the data using audit as a classifier. Low risk users should be excluded due to floor effects. Standard-doses, login_times, and ave_login_time must be normalized first to proceed to comparasions.

# Check non-zero values
nearZeroVar(drinkLess, saveMetrics=TRUE)

# Table
table(drinkLess$reduced_factor, drinkLess$audit)

hist(drinkLess$sum_login)
hist(log10(drinkLess$alcohol_pos)+1)


#### OBJECTIVE 1 #######################################################################################
# 1 - Compare alcohol consumption after 6 weeks among - intention to treat and treatment completers. ---
# Include age, gender and group in analysis

# Treatment Completers ----
treatCompA   <- subset(treatComp, treatComp$client_id != 2302 )
treatCompA   <- treatCompA[, -c(2,7,8,9,10,11,13,14,15,16,17,18)]

treatMelted  <- melt(treatCompA, id = c("client_id",  "gender", "age", "audit"), measured = c("alcohol_pre",  "alcohol_pos")) 

names(treatMelted)  <- c("id", "sex", "age", "group", "time", "value")

## Normalization of variables

# alcohol prelog
treatMelted$value  <-  log(treatMelted$value + 1)
qqnorm(treatMelted$value)


# Traditional ANOVA approach
## Set contrasts
treatMelted$group  <- as.factor(treatMelted$group)
alcoholvsLow     <- c(1,-2, 1)
contrasts(treatMelted$group, 1)  <- cbind(alcoholvsLow, depvsLow, depvsRisk, riskvsLow)

# Traditional Anova
alcoholModel  <- ezANOVA(data = treatMelted, dv = .(value), wid = .(id), within = .(time), between = .(group, sex), detailed = TRUE, type = 3, return_aov = TRUE)

# Print results 
alcoholModel

# Graphs

# Data frame
plotdf  <- treatMelted
plotdf$group  <- factor(plotdf$group, levels=c("Dependence", "Risk", "Low risk"))
plotdf$time  <- Recode(plotdf$time, "'alcohol_pre' = 'Pre'; 'alcohol_pos' = 'Pos'")
plotdf$time  <- factor(plotdf$time, levels=c("Pre", "Pos"))
plotdf$value  <- exp(plotdf$value) - 1 


# vs. Group
bar2  <- ggplot(plotdf, aes(time, value, fill = group))
bar2 + stat_summary(fun.y = mean, geom = "bar", position = "dodge", colour="black") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2) + facet_wrap( ~ group) + labs(x = "Time",  y = "Mean number of standard Drinks", fill = "group") +  theme_bw(base_size = 18) + scale_fill_manual(values=c("#777777","#e0e0e0","#FFFFFF")) + theme(legend.position = "none")

# vs. Sex
bar3  <- ggplot(plotdf, aes(time, value, fill = sex))
bar3 + stat_summary(fun.y = mean, geom = "bar", position = "dodge", colour="black") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2) + facet_wrap( ~ sex) + labs(x = "Time",  y = "Mean number of Standard Drinks", fill = "group") +  theme_bw(base_size = 18) + scale_fill_manual(values=c("#777777","#e0e0e0","#FFFFFF")) + theme(legend.position = "none")

# Intention to treat #----
# Assumption - missing values as failure

## Create a dataframe
itt   <- subset(drinkLess, drinkLess$client_id != 2302)
ittA   <- itt[, -c(1,3,8,10,11,12,14,15,16)]

## Impute missing as no change
ittA$alcohol_pos  <- ifelse(is.na(ittA$alcohol_pos), ittA$alcohol_pre, ittA$alcohol_pos) 

# Prepare data to repeated measures analyses
ittMelted  <- melt(ittA, id = c("client_id",  "gender", "age", "audit", "rcq_factor"), measured = c("alcohol_pre",  "alcohol_pos")) 

# Pick good names for dataframe
names(ittMelted)  <- c("id", "sex", "age", "group", "rcq", "time", "value")


## Normalization of variables

# alcohol prelog
ittMelted$value  <-  log(ittMelted$value + 1)
qqnorm(ittMelted$value)

# Traditional ANOVA approach
## Set contrasts
ittMelted$group  <- as.factor(ittMelted$group)
alcoholvsLow     <- c(1,-2, 1)
contrasts(ittMelted$group, 1)  <- cbind(alcoholvsLow)

# Remove NA's
ittMelted <- subset(ittMelted, complete.cases(ittMelted))

# Traditional Anova
itt_alcoholModel  <- ezANOVA(data = ittMelted, dv = .(value), wid = .(id), within = .(time), between = .(group, sex), detailed = TRUE, type = 3, return_aov = TRUE)

# Print results 
itt_alcoholModel

# Graphs

# Data frame
plotdf  <- ittMelted
plotdf$group  <- factor(plotdf$group, levels=c("Dependence", "Risk", "Low risk"))
plotdf$time  <- Recode(plotdf$time, "'alcohol_pre' = 'Pre'; 'alcohol_pos' = 'Pos'")
plotdf$time  <- factor(plotdf$time, levels=c("Pre", "Pos"))
plotdf$value  <- exp(plotdf$value) - 1 


# vs. Group
bar2  <- ggplot(plotdf, aes(time, value, fill = group))
bar2 + stat_summary(fun.y = mean, geom = "bar", position = "dodge", colour="black") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2) + facet_wrap( ~ group) + labs(x = "Time",  y = "Mean number of standard Drinks", fill = "group") +  theme_bw(base_size = 18) + scale_fill_manual(values=c("#777777","#e0e0e0","#FFFFFF")) + theme(legend.position = "none")

# vs. Sex
bar3  <- ggplot(plotdf, aes(time, value, fill = sex))
bar3 + stat_summary(fun.y = mean, geom = "bar", position = "dodge", colour="black") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2) + facet_wrap( ~ sex) + labs(x = "Time",  y = "Mean number of Standard Drinks", fill = "group") +  theme_bw(base_size = 18) + scale_fill_manual(values=c("#777777","#e0e0e0","#FFFFFF")) + theme(legend.position = "none")

  
  

#### OBJECTIVE 2 #######################################################################################
# 2 - Create a model to explain success. ###############################################################

drinkLessReg  <- drinkLess[,-c(1,8,11,14,15,16)]

drinkLessReg$completed <- factor(drinkLessReg$completed, levels = c("Yes", "No"))
drinkLessReg$completed <- relevel(drinkLessReg$completed, "No")

drinkLessReg$audit <- factor(drinkLessReg$audit, levels = c("Dependence", "Low risk", "Risk"))
drinkLessReg$audit <- relevel(drinkLessReg$audit, "Low risk")

drinkLessReg$rcq_factor <- factor(drinkLessReg$audit, levels = c("Action", "Con", "Pre"))
drinkLessReg$rcq_factor <- relevel(drinkLessReg$rcq_factor, "Pre")

drinkLessReg$gender <- factor(drinkLessReg$gender, levels = c("Men", "Women"))
drinkLessReg$gender <- relevel(drinkLessReg$gender, "Men")


set.seed(666)

# Partioning data
inTrain <- createDataPartition(y = drinkLessReg$completed, p = .75, list = FALSE)
            
training <- drinkLessReg[inTrain,] # training
testing <- drinkLessReg[-inTrain,] # testing


featurePlot(x=training[,c("age","alcohol_pre", "login_times", "ave_login_time", "sum_login","gender")], y = training$completed, plot="pairs")

model0  <- glm(completed ~ 1, data = training, family=binomial())
model1  <- update(model0, .~. + alcohol_pre)
model2  <- update(model1, .~.  + sum_login +  age +  gender + audit)

# summary
summary(model0); summary(model1); summary(model2)

# AIC
model0$aic; model1$aic; model2$aic

# Odss
round(cbind(exp(model2$coefficients)),3)
round(exp(confint(model2)),3)

# R^ for models
logisticPseudoR2s  <- function(LogModel){
  dev  <- LogModel$deviance
  nullDev  <- LogModel$null.deviance
  modelN  <- length(LogModel$fitted.values)
  R.l  <- 1 - dev / nullDev
  R.cs <- 1-exp(-(nullDev - dev) / modelN)
  R.n  <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  
  cat("Pseudo R^2 for logistic regression \n")
  cat("Hosmer and Lemeshor R^2 ", round(R.l,3), "\n")
  cat("Cox and Snell R^2       ", round(R.cs,3), "\n")
  cat("Nagelkerke R^2          ", round(R.n,3), "\n")
    
}
logisticPseudoR2s(model2)

teste  <- predict(model2, testing)


table(teste, testing$completed)

# Caret Package ---------------
caretModel  <- train(completed ~ alcohol_pre + sum_login + age + gender + audit, data = training, preProcess = "knnImpute", method="multinom")

predictedValues  <- predict(caretModel, newdata = testing)



# Check Model Accuracy
confusionMatrix(predictedValues, testing$completed)


caretModel$pred

anova(model0, model1, model2)

nearZeroVar(drinkLessReg, saveMetrics =  TRUE)

str(training)

