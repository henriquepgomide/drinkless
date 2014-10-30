# Question - How can we customize the drinkless intervention?
# Variables of interest - sex, age, education, rcq_stage, audit_score, work status

# 1st task

## Merge three dataframes - rcq_motivation_form, audit, registration_form
drinkLess_partial  <- merge(registration_form, audit, by="client_ID") # Merge registration_form and audit
drinkLess  <- merge(drinkLess_partial, rcq_motivation, by="client_ID"); rm(drinkLess_partial) # Merge rcq_motivation

## Clean and prepare data for analysis
drinkLess  <- subset(drinkLess, drinkLess$Research=="Sim") # Use data for research purposes

## RCQ outcome
library(car)
drinkLess$rcq_outcome  <- Recode(drinkLess$rcq_outcome, "1='Pre Contemplation'; 2 ='Contemplation'; 3 = 'Action'; else = NA")
drinkLess$rcq_outcome  <- as.factor(drinkLess$rcq_outcome)

## Audit outcome
drinkLess$Result  <- Recode(drinkLess$Result, "0:6='Low risk'; 7:15='Risky'; 16:19='High Risk'; 19:40 = 'Dependence'; else = NA")

## Sex
drinkLess$sex  <- Recode(drinkLess$sex, "3 = 'Female'; 'Homem' = 'Male'; else = NA")

## Write new dataframe with interest vars
drinkLess  <- subset(drinkLess, select = c(1,5,6,7,12,31,49))
write.csv(drinkLess, "drinkless_kmeans.csv")

# Recode into dummy Vars

## RCQ outcome
drinkLess$rcq_action  <- ifelse(drinkLess$rcq_outcome == "Action" & !is.na(drinkLess$rcq_outcome), 1, 0)
drinkLess$rcq_contemp  <- ifelse(drinkLess$rcq_outcome == "Contemplation" & !is.na(drinkLess$rcq_outcome), 1, 0)
drinkLess$rcq_precon  <- ifelse(drinkLess$rcq_outcome == "Pre Contemplation" & !is.na(drinkLess$rcq_outcome), 1, 0)
drinkLess$rcq_unknown  <- ifelse(is.na(drinkLess$rcq_outcome), 1, 0)

## Audit
drinkLess$au_Lr  <- ifelse(drinkLess$Result == "Low risk" & !is.na(drinkLess$Result), 1, 0)
drinkLess$au_R  <- ifelse(drinkLess$Result == "Risky" & !is.na(drinkLess$Result), 1, 0)
drinkLess$au_Hr  <- ifelse(drinkLess$Result == "High Risk" & !is.na(drinkLess$Result), 1, 0)
drinkLess$au_D  <- ifelse(drinkLess$Result == "Dependence" & !is.na(drinkLess$Result), 1, 0)
drinkLess$au_unknown  <- ifelse(is.na(drinkLess$Result), 1, 0)

## Sex
drinkLess$sex_female  <- ifelse(drinkLess$sex == "Female" & !is.na(drinkLess$sex), 1, 0)
drinkLess$sex_male  <- ifelse(drinkLess$sex == "Male" & !is.na(drinkLess$sex), 1, 0)
drinkLess$sex_unknown  <- ifelse(is.na(drinkLess$sex), 1, 0)

## Education
drinkLess$education  <- Recode(drinkLess$education, "c('1 Grau','2 Grau', '2 Grau completo', '2 Grau incompleto', 'Fundamental') = 'High School'; c('P?s Gradua??o (especializa??o, mestrado, doutorado)', 'Superior completo', 'Superior incompleto') = 'College'; else = NA")
drinkLess$education_highschool  <- ifelse(drinkLess$education == "High School" & !is.na(drinkLess$education), 1, 0)
drinkLess$education_college  <- ifelse(drinkLess$education == "College" & !is.na(drinkLess$education), 1, 0)
drinkLess$education_unknown  <- ifelse(is.na(drinkLess$education), 1, 0)

## Work situation
drinkLess$work_yes  <- ifelse(drinkLess$Work.situation == "Sim" & !is.na(drinkLess$Work.situation), 1, 0)
drinkLess$work_no  <- ifelse(drinkLess$Work.situation == "N?o" & !is.na(drinkLess$Work.situation), 1, 0)
drinkLess$work_unknown  <- ifelse(is.na(drinkLess$Work.situation), 1, 0)

# K-Means
drinkLessK  <- subset(drinkLess, select = -c(1,2,4,5,6,7))

## Normalization
drinkLessK_z <- as.data.frame(lapply(drinkLessK, scale))

## Removing NAN vars - due to lack of NA's
drinkLessK_z  <- subset(drinkLessK_z, select = -c(5,10,13))

# K-means
userClusters  <- kmeans(drinkLessK_z, 3)

# Size of clusters
userClusters$size

# Assign clusters to original data
drinkLess$clusters  <- userClusters$cluster

# by age
by(drinkLess$age, drinkLess$clusters, summary)

# by sex
by(drinkLess$sex, drinkLess$clusters, summary)

# by rcq
by(drinkLess$rcq_outcome, drinkLess$clusters, summary)

# by audit
by(drinkLess$Result, drinkLess$clusters, summary)

# by education
by(drinkLess$education, drinkLess$clusters, summary)

# by work
by(drinkLess$Work.situation, drinkLess$clusters, summary)

