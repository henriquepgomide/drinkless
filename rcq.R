#######################
# RCQ - Readiness to
# Change Questionnaire
#######################

####################
# AIM
####################

# This code has all analyses from the Brazilian RCQ validation.
# If you need access to the draft, reach me at henriquepgomide@gmail.com.

# ===================
# PACKAGES 
# ===================

# You need to install the following packages: psych, lavaan, sp, car, lattice. Don't forget do cite them using the citation("pkgname") function.

# Install packages

install.packages(c("psych","lavaan", "car", "lattice"))

# Load packages

library(psych)   # Exploratory Factor Analysis and Reliability
library(lavaan)  # Confirmatory Factor Analysis
library(car)     # Recode function
library(lattice) # Trellis graphics

# ===================
# PREPARE DATA 
# ===================

## Set working directory. This is described in the drinklessPortal.R file. You shoul point the working directory to the folder where all csvs were downloaded.

setwd("csvs/sites/default/files/gwiaa_export/") # You may need to change this line before proceed.

## Read Audit questionnaire
audit  <- read.csv("export_audit.csv")

## Read rcq questionnaire
rcq_motivation  <- read.csv("export_rcq_motivation.csv")

## Read registration form
registration_form <- read.csv("export_registration_form.csv")

# Merge csv's with - demographics + audit + rcq_motivation
## Putting same names in different data frames
names(audit)[names(audit)=="client_ID"] <- "client_id"
names(registration_form)[names(registration_form)=="client_ID"] <- "client_id"

## Merge data frames using "client_id" as the key

### Create temporary data.frame
rcqMerged  <- merge(audit, registration_form, by.x="client_id", by.y="client_id", all= TRUE) 
rcqFinal  <- merge(rcqMerged, rcq_motivation, by.x="client_id", by.y="client_ID", all = TRUE)
### Remove the temporary dataframe
rm(rcqMerged) 

## Select just valid cases for RCQ. - Participants had to agreed to participate and also filled in all rcq questions.
rcq  <- subset(rcqFinal, rcqFinal$rcq_1 != "NA" & rcqFinal$rcq_2 != "NA" & rcqFinal$rcq_1 != "NA" & rcqFinal$Research == "Sim")
rcq$sex <-  as.numeric(rcq$sex)
rcq$Timestamp.x <- as.Date(rcq$Timestamp.x, "%d.%m.%Y")

## Final dataframe
rcq  <- rcq[, c("client_id", "Timestamp.x", "audit_1", "audit_2", "audit_3", "audit_4", "audit_5", "audit_6", "audit_7", "audit_8", "audit_9", "audit_10", "Result", "sex", "age", "education", "Province", "País", "Work.situation", "rcq_1", "rcq_2", "rcq_3","rcq_4","rcq_5","rcq_6","rcq_7","rcq_8","rcq_9","rcq_10","rcq_11","rcq_12","rcq_a","rcq_b","rcq_c","rcq_outcome")]

## Remove non useful data to clean computer memory. This is required for those running computers from the 80's...
rm("audit", "rcqFinal", "rcq_motivation", "registration_form")

## Save final data.frame as csv file
write.csv(rcq, "rcq.csv")

# ===================
# DATA ANALYSIS 
# ===================

## Open dataframe 
rcq  <- read.csv("rcq.csv")

## Summary variables
describe(rcq)

##---- Transform and compute vars ---------

### Audit Zones
rcq$auditRec[rcq$Result <= 7]  <-  "Low Risk"
rcq$auditRec[rcq$Result > 7 &  rcq$Result <= 15]  <-  "Risky"
rcq$auditRec[rcq$Result > 15 &  rcq$Result <= 19]  <-  "High-Risk"
rcq$auditRec[rcq$Result > 20]  <-  "Dependency"
rcq$auditRec  <- as.factor(rcq$auditRec) # transforming var as a factor

### Sex
rcq$sex <- factor(rcq$sex, labels=c("Female","Male"))

### Education
rcq$education <- factor(rcq$education, labels=c("High School Comp","High School Incomp", "Elementary", "Graduate", "College", "College Incomp."))

### Work situation
rcq$Work.situation  <- factor(rcq$Work.situation, labels=c("No", "Yes"))

### Province to Region
# Southeast
rcq$region[rcq$Province == "Esp?rito Santo" | rcq$Province == "Minas Gerais" | rcq$Province == "Rio de Janeiro" | rcq$Province == "S?o Paulo"]  <- "Southeast"
# South
rcq$region[rcq$Province == "Paran?" | rcq$Province == "Santa Catarina" | rcq$Province == "Rio Grande do Sul"]  <- "South"
# Mid-west
rcq$region[rcq$Province == "Mato Grosso" | rcq$Province == "Mato Grosso do Sul" | rcq$Province == "Goi?s" | rcq$Province == "Distrito Federal"]  <- "Midwest"
# Northeast
rcq$region[rcq$Province == "Bahia" | rcq$Province == "Sergipe" | rcq$Province == "Pernambuco" | rcq$Province == "Piau?" | rcq$Province == "Rio Grande do Norte" |  rcq$Province == "Para?ba" | rcq$Province == "Cear?" |  rcq$Province == "Alagoas" | rcq$Province == "Maranh?o"]  <- "Northeast"
# North
rcq$region[rcq$Province == "Acre" | rcq$Province == "Amazonas" | rcq$Province == "Rond?nia" | rcq$Province == "Roraima" | rcq$Province == "Amap?" | rcq$Province == "Tocantins" ]  <- "North"
# NA
rcq$region[rcq$Province == "0"]  <- NA

### RCQ
#computing using WHO criteria
rcq$scorePc  <- rcq$rcq_1 + rcq$rcq_5 + rcq$rcq_10 + rcq$rcq_12
rcq$scoreC <- rcq$rcq_3 + rcq$rcq_4 + rcq$rcq_8 + rcq$rcq_9
rcq$scoreA  <- rcq$rcq_2 + rcq$rcq_6 + rcq$rcq_7 + rcq$rcq_11

### Write dataframe
write.csv(rcq, "rcq_df.csv" )

## --- OPEN DATA ----

rcq  <- read.csv("rcq_df.csv")


##---- Descriptives ---------

# Age -
describe(rcq$age)
by(rcq$age, rcq$sex, summary)
bwplot(~age|sex*education, data=rcq)

# Sex - 
round(prop.table(table(rcq$sex)),3)
by(rcq$sex, rcq$education, summary)

# Education - 
tableEducation  <- sort(table(rcq$education), decreasing=TRUE)
cbind(round(prop.table(tableEducation),3))

# Work situation
cbind(round(prop.table(table(rcq$Work.situation)),3))
by(rcq$Result, rcq$Work.situation, summary)

# Region
cbind(round(prop.table(sort(table(rcq$region), decreasig=TRUE)),3))

# Audit Score
cbind(round(prop.table(sort(table(rcq$auditRec), decreasig=TRUE)),3))*100
boxplot(Result ~sex, data=rcq)
bwplot(~Result|sex*education, data=rcq)

# RCQ classification vs. Audit
by(rcq$Result, rcq$rcq_outcome, summary)
bwplot(~Result|rcq_outcome, data=rcq)


##---- Psychometrics  ---------

set.seed(12345)
rcq_rand <- rcq[order(runif(713)), ]

# Split data frames for analysis
rcqEfa  <- rcq_rand[1:356, ]   # Exploratory factor analysis data
rcqCfa  <- rcq_rand[357:713, ] # Confirmatory factor analysis data

# Exploratory Factor Analysis ----
rcqEfa_complete  <- rcqEfa[,22:33] # Select just the RCQ items
      
# Descriptive statistics for questionnaire
describe(rcqEfa_complete)

# KMO = .89
KMO(rcqEfa_complete)

# Barlett test of homogeneity; K²=169.92; p < 0.001
bartlett.test(rcqEfa_complete)

# Parallel Analysis with polychoric correlations and minimal residuals method
fa.parallel.poly(rcqEfa_complete, fm="minres", fa="fa")

# Very simple structure
VSS(rcqEfa_complete)

# Factor analysis

## 1-factor model
rcq1factor  <- rcqEfa_complete

# Recode preContemplation into Contemplation
rcq1factor$rcq_1 <- Recode(rcq1factor$rcq_1, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
rcq1factor$rcq_5 <- Recode(rcq1factor$rcq_5, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
rcq1factor$rcq_10 <- Recode(rcq1factor$rcq_10, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
rcq1factor$rcq_12 <- Recode(rcq1factor$rcq_12, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")

## fa with 1 factor
fa1 <- fa.poly(rcq1factor, nfactors = 1,  fm="minres")
print(fa1, cut = .3)

## 2-factor model
rcq2factor  <- rcqEfa_complete
fa2 <- fa.poly(rcq2factor, nfactors = 2, rotate="oblimin", fm="minres")
print(fa2, cut = .3)

## 3-factor model
rcq3factor  <- rcqEfa_complete

## fa with 3 factor
fa3 <- fa.poly(rcq3factor, nfactors = 3)
print(fa3, cut = .3)

### Reliability ----
alpha(rcq2factor)

# 1st factor
alpha(rcq2factor[,c(1,3,4,5,8,9,10,12)])

# 2nd factor
alpha(rcq2factor[,-c(1,3,4,5,8,9,10,12)])

### CFA ----


rcqCfa_complete  <- rcqCfa[,22:33]

# 1-factor
cfa.rcq1f  <- rcqCfa_complete 


# Recode preContemplation into Contemplation
cfa.rcq1f$rcq_1 <- Recode(cfa.rcq1f$rcq_1, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
cfa.rcq1f$rcq_5 <- Recode(cfa.rcq1f$rcq_5, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
cfa.rcq1f$rcq_10 <- Recode(cfa.rcq1f$rcq_10, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")
cfa.rcq1f$rcq_12 <- Recode(cfa.rcq1f$rcq_12, "1='5' ; 2='4' ; 3 = '3'; 3 = '3'; 4 = '2'; 5 = '1'")


# 1-Factor Model
RCQ.1f.MODEL  <- ' # Latent variables
                   readiness       =~ rcq_1 + rcq_2 + rcq_3 + rcq_4
                                   + rcq_5 + rcq_6 + rcq_7 + rcq_8
                                   + rcq_9 + rcq_10 + rcq_11 + rcq_12'

cfa1f  <- cfa(RCQ.1f.MODEL,  data = cfa.rcq1f)

# 2-Factor Model 
RCQ.2f.MODEL  <- ' # Latent variables
                   Con   =~ rcq_1 + rcq_3 + rcq_4 + rcq_5 + 
                            rcq_8 + rcq_9 + rcq_10 + rcq_12                                   
                   Action =~ rcq_2 + rcq_6 + rcq_7 + rcq_11                 
                '

cfa2f  <- cfa(RCQ.2f.MODEL,  data = rcqCfa_complete)

# 3-Factor Model with Correlation
RCQ.3fr.MODEL  <- '# Latent variables                  
                  Con       =~ rcq_1 + rcq_5 + rcq_10 + rcq_12
                  Precont   =~ rcq_3 + rcq_4 + rcq_8 + rcq_9
                  Action    =~ rcq_2 + rcq_6 + rcq_7 + rcq_11                 
                '

cfa3fr  <- cfa(RCQ.3fr.MODEL,  data = rcqCfa_complete)

# 3-Factor Model with no Correlation
RCQ.3f.MODEL  <- '# Latent variables                  
                  Con       =~ rcq_1 + rcq_5 + rcq_10 + rcq_12
                  Precont   =~ rcq_3 + rcq_4 + rcq_8 + rcq_9
                  Action    =~ rcq_2 + rcq_6 + rcq_7 + rcq_11                                                
                '

cfa3f  <- cfa(RCQ.3f.MODEL,  data = rcqCfa_complete, orthogonal=TRUE)


## Summary
summary(cfa1f, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)
summary(cfa2f, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)
summary(cfa3f, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)


# 1-Factor Improved Reduced with the best loadings
RCQ.1fi.MODEL  <- ' # Latent variables
                   readiness       =~ rcq_1 + rcq_4 + rcq_8 +  rcq_10'

cfa1fi  <- cfa(RCQ.1fi.MODEL,  data = rcq1factor)

summary(cfa1fi, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)



# Fit measures
rbind(
round(fitMeasures(cfa1fi, fit.measures = c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "bic", "aic")),3) , 
round(fitMeasures(cfa2f, fit.measures = c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "bic", "aic")),3) , 
round(fitMeasures(cfa1f, fit.measures = c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "bic", "aic")),3),
round(fitMeasures(cfa3f, fit.measures = c("chisq", "df", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "bic", "aic")),3)
)

## Anova
anova(cfa1f, cfa2f, cfa1fi, cfa3f)

# Graphing solution
library(qgraph)
qgraph(cfa1fi, layout = "tree", titles = FALSE)

# Alpha
alpha(cfa.rcq1f[,c(1,4,8,10)])

# 2-Factor Model 
RCQ.2fb.MODEL  <- ' # Latent variables
Con   =~ rcq_1 + rcq_3 + rcq_3 +  
rcq_8 + rcq_9 + rcq_10 + rcq_12                                   
Action =~ rcq_6 + rcq_7 + rcq_11                 
'

cfa2fb  <- cfa(RCQ.2fb.MODEL,  data = rcq1factor)

summary(cfa2fb, standardized=TRUE, fit.measures=TRUE, rsq=TRUE, modindices=TRUE)


# Final Scale sum
sumrcq  <- (rcq[,c("rcq_1","rcq_4","rcq_8","rcq_10")])

# Recode 1 to 10

sumrcq$rcq_1 <- Recode(sumrcq$rcq_1, "1='2'  ; 2='1'  ; 3 = '0'; 4 = '-1'; 5 = '-2'")
sumrcq$rcq_4 <- Recode(sumrcq$rcq_4, "1='-2' ; 2='-1' ; 3 = '0'; 4 = ' 1'; 5 = ' 2'")
sumrcq$rcq_8 <- Recode(sumrcq$rcq_8, "1='-2' ; 2='-1' ; 3 = '0'; 4 = ' 1'; 5 = ' 2'")
sumrcq$rcq_10 <- Recode(sumrcq$rcq_10, "1='2'; 2='1'  ; 3 = '0'; 4 = '-1'; 5 = '-2'")

# Sum vari
sumrcq$sum  <- sumrcq$rcq_1 + sumrcq$rcq_4 + sumrcq$rcq_8 + sumrcq$rcq_10

# summary
describe(sumrcq$sum)


# =======================
# ORIGINAL COMPARISIONS
# =======================

# Creating proflies based on Heather et al. (1991)
rcq$heather
rcq$heather[rcq$rcqA  == rcq$rcqC & rcq$rcqA != rcq$rcqPc & rcq$rcqPc != rcq$rcqC]    <- "Tie A-C"
rcq$heather[rcq$rcqA  == rcq$rcqPc & rcq$rcqA != rcq$rcqC & rcq$rcqPc != rcq$rcqC]    <- "Tie A-Pc"
rcq$heather[rcq$rcqPc == rcq$rcqC & rcq$rcqPc != rcq$rcqA & rcq$rcqPc != rcq$rcqA]    <- "Tie C-Pc"
rcq$heather[rcq$rcqPc == rcq$rcqC & rcq$rcqPc == rcq$rcqA]    <- "Triple Tie"
rcq$heather[rcq$rcqA  > rcq$rcqC & rcq$rcqA > rcq$rcqPc]    <- "Action"
rcq$heather[rcq$rcqC  > rcq$rcqA & rcq$rcqC > rcq$rcqPc]    <- "Contemplation"
rcq$heather[rcq$rcqPc > rcq$rcqC & rcq$rcqPc > rcq$rcqA]    <- "Pre Contemplation"

## Heather with correction
rcq$heatherC[rcq$rcqA  >= rcq$rcqC & rcq$rcqA >= rcq$rcqPc]    <- "Action"
rcq$heatherC[rcq$rcqC  > rcq$rcqA & rcq$rcqC > rcq$rcqPc]    <- "Contemplation"
rcq$heatherC[rcq$rcqPc > rcq$rcqC & rcq$rcqPc > rcq$rcqA]    <- "Pre Contemplation"

cbind(table(rcq$heatherC))

# Creating profiles based on Rollnick et al. (1992) adapted for the 3 point subscales
rcq$profile[rcq$rcqPc > 9 & rcq$rcqC > 9 & rcq$rcqA > 9 ]  <- "A (+ + +)"
rcq$profile[rcq$rcqPc > 9 & rcq$rcqC > 9 & rcq$rcqA <= 9 ]  <- "B (+ + –)"
rcq$profile[rcq$rcqPc > 9 & rcq$rcqC <= 9 & rcq$rcqA > 9 ]  <- "C (+ – +)"
rcq$profile[rcq$rcqPc > 9 & rcq$rcqC <= 9 & rcq$rcqA <= 9 ]  <- "D (+ – –)"
rcq$profile[rcq$rcqPc <= 9 & rcq$rcqC > 9 & rcq$rcqA > 9 ]  <- "E (– + +)"
rcq$profile[rcq$rcqPc <= 9 & rcq$rcqC > 9 & rcq$rcqA <= 9 ]  <- "F (– + –)"
rcq$profile[rcq$rcqPc <= 9 & rcq$rcqC <= 9 & rcq$rcqA > 9 ]  <- "G (– – +)"
rcq$profile[rcq$rcqPc <= 9 & rcq$rcqC <= 9 & rcq$rcqA <= 9 ]  <- "H (– – –)"


#- Item Response Theory playground -----
# Be advised! This is just for study purposes.
# Feel free to play with the code.

## Subscales Graded Response Model

## Checking unidimensionality
### 1 Factor
library(ltm)
irt.rcq  <- rcq[,22:33]
irt.rcq  <- irt.rcq[,c(1,4,8,10)]

rcq2p  <- grm(irt.rcq); rcq1p  <- grm(irt.rcq, constrained = T)

anova(rcq1p, rcq2p)

summary(rcq2p)


## Pre contemplation
preo2p  <- grm(preo); preo1p  <- grm(preo, constrained = T) # Comparing models - one parameter vs. 2 parameter
### Comparing, using BIC and AIC as criteria. (Less is better)
anova(preo1p, preo2p) # 2 parameters model (preo2p) seems to fit data better

### Plotting Item Characteristic Curves
plot(rcq2p, type= "IIC", col = brewer.pal(4,"Dark2"), legend= TRUE) 
plot(rcq2p, type= "ICC", col = brewer.pal(4,"Dark2"), legend= TRUE)
plot(rcq2p, type= "OCCu", col = brewer.pal(4,"Dark2"), legend= TRUE)

### Coeficients
coef(rcq2p)

## Contemplation
c2Par  <- grm(shortC); c1Par  <- grm(shortC, constrained = T) # Comparing models - one parameter vs. 2 parameter
### Comparing, using BIC and AIC as criteria. (Less is better)
anova(pc1Par, pc2Par) # 2 parameters model (pc2Par) seems to fit data better

### Plotting Item Characteristic Curves
plot(c2Par, type= "IIC", col = brewer.pal(4,"Dark2"), legend= TRUE) 
plot(c2Par, type= "ICC", col = brewer.pal(4,"Dark2"), legend= TRUE)
plot(c2Par, type= "OCCu", col = brewer.pal(4,"Dark2"), legend= TRUE)

### Coeficients
coef(c2Par)

## Action
a2Par  <- grm(shortA); a1Par  <- grm(shortA, constrained = T) # Comparing models - one parameter vs. 2 parameter
### Comparing, using BIC and AIC as criteria. (Less is better)
anova(a1Par, a2Par) # 2 parameters model (pc2Par) seems to fit data better

### Plotting Item Characteristic Curves
plot(a2Par, type= "IIC", col = brewer.pal(4,"Dark2"), legend= TRUE) 
plot(a2Par, type= "ICC", col = brewer.pal(4,"Dark2"), legend= TRUE)
plot(a2Par, type= "OCCu", col = brewer.pal(4,"Dark2"), legend= TRUE)

### Coeficients
coef(a2Par)

# Scoring subscales
scorePre  <- factor.scores(pc2Par, resp.patterns=shortPc)
scoreContemplation  <- factor.scores(c2Par, resp.patterns=shortC)
scoreAction  <- factor.scores(a2Par, resp.patterns=shortA)


boxplot(scorePre$score.dat$z1 ~ rcq$auditRec)
boxplot(scoreContemplation$score.dat$z1 ~ rcq$auditRec)
boxplot(scoreAction$score.dat$z1 ~ rcq$auditRec)

by(scoreContemplation$score.dat$z1, rcq$auditRec, mean)


# Thetas Scores
plot(scoreAction$score.dat$z1, rcq$rcqA, type = "p", xlab = "Thetas", ylab = "Soma bruta da escala")
cor(scoreAction$score.dat$z1, rcq$rcqA)

plot(scoreContemplation$score.dat$z1, rcq$rcqC, type = "p", xlab = "Thetas", ylab = "Soma bruta da escala")
cor(scoreContemplation$score.dat$z1, rcq$rcqC)

plot(scorePre$score.dat$z1, rcq$rcqPc, type = "p", xlab = "Thetas", ylab = "Soma bruta da escala")
cor(scorePre$score.dat$z1, rcq$rcqPc)

cbind(table(rcq$profile))

