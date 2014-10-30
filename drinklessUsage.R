# User registration statistics -------------------------------------------------------------------

head(user_logins)
names(user_logins)[2]<-paste("client_id")
userLogins  <- merge(user_logins, user_logins_stats, by="client_id")
names(registration_form)

# Descriptives
# Age
summary(registration_form$age[registration_form$age > 0])
boxplot(registration_form$age ~ registration_form$sex)
# Estado
cbind(sort(table(registration_form$Province), decreasing=TRUE))
# Registration
registration_form$registr_date <- as.Date(registration_form$registr_date, "%d.%m.%Y")
graphAccess  <- ggplot(registration_form, aes(registr_date))
graphAccess + geom_freqpoly(colour = "blue", binwidth = 10) + labs(x = "Month", y = "Frequency")
# Sex
table(registration_form$sex)
# education
cbind(sort(table(registration_form$education), decreasing = TRUE))

# WHO intervention Usage ----
# Registration form
registration_form$registr_date <- as.Date(registration_form$registr_date, "%d.%m.%Y")
registration_form$registr_date


table(registration_form$registr_date >= "2013-02-01" & registration_form$registr_date <= "2013-09-01")
sort(table(registration_form$client_id), decreasing = TRUE)

# User Drinks
user_drinks$drinks_time_stamp <- as.Date(user_drinks$drinks_time_stamp, "%d.%m.%Y")
user_drinks  <- subset(user_drinks, user_drinks$drinks_time_stamp >= "2013-02-01" & user_drinks$drinks_time_stamp <= "2013-09-01" & user_drinks$client_id != 1)
freqUserdrinks  <- as.data.frame(table(user_drinks$client_id))
length(freqUserdrinks$Var1)

# export_audit
audit$timestamp <- as.Date(audit$timestamp, "%d.%m.%Y")
audit  <- subset(audit, audit$timestamp >= "2013-02-01" & audit$timestamp <= "2013-09-01" & audit$Client_Id != 1)
freqAudit  <- as.data.frame(table(audit$Client_Id))
length(freqAudit$Var1)

# advantages_disadvantages
advantages_disadvantages$timestamp <- as.Date(advantages_disadvantages$timestamp, "%d.%m.%Y")
advantages_disadvantages  <- subset(advantages_disadvantages, advantages_disadvantages$timestamp >= "2013-02-01" & advantages_disadvantages$timestamp <= "2013-09-01" & advantages_disadvantages$client_id != 1)
freqAvantages  <- as.data.frame(table(advantages_disadvantages$client_id))
length(freqAvantages$Var1)

# rcq-motivation
rcq_motivation$time_stamp_rcq <- as.Date(rcq_motivation$time_stamp_rcq, "%d.%m.%Y")
rcq_motivation  <- subset(rcq_motivation, rcq_motivation$time_stamp_rcq >= "2013-02-01" & rcq_motivation$time_stamp_rcq <= "2013-09-01" & rcq_motivation$client_id != 1)
freqRcq  <- as.data.frame(table(rcq_motivation$client_id))
length(freqRcq$Var1)

# export-evaluation
evaluation$time_stamp_evals1 <- as.Date(evaluation$time_stamp_evals1, "%d.%m.%Y")
evaluation  <- subset(evaluation, evaluation$time_stamp_evals1 >= "2013-02-01" & evaluation$time_stamp_evals1 <= "2013-09-01" & evaluation$client_id != 1)
freqEvaluation  <- as.data.frame(table(evaluation$client_id))
length(freqEvaluation$Var1)

# goal
goal$goal_time_stamp  <- as.Date(goal$goal_time_stamp, "%d.%m.%Y")
goal  <- subset(goal, goal$goal_time_stamp >= "2013-02-01" & goal$goal_time_stamp <= "2013-09-01" & goal$client_id != 1)
freqGoal  <- as.data.frame(table(goal$client_id))
length(freqGoal$Var1)

sort(freqGoal$Freq, decreasing = TRUE) # OMG! - two user with 517 and 513 access? Maybe wrong.

# maintain rewards
maintain_rewards$rewards_time_stamp <- as.Date(maintain_rewards$rewards_time_stamp, "%d.%m.%Y")
maintain_rewards  <- subset(maintain_rewards, maintain_rewards$rewards_time_stamp >= "2013-02-01" & maintain_rewards$rewards_time_stamp <= "2013-09-01" & maintain_rewards$client_id != 1)
freqMaintain  <- as.data.frame(table(maintain_rewards$client_id))
length(freqMaintain$Var1)

# handle risks situations
maintain_handle_risk_situations$time_stamp_handlerisk  <- as.Date(maintain_handle_risk_situations$time_stamp_handlerisk, "%d.%m.%Y")
maintain_handle_risk_situations  <- subset(maintain_handle_risk_situations, maintain_handle_risk_situations$time_stamp_handlerisk >= "2013-02-01" & maintain_handle_risk_situations$time_stamp_handlerisk <= "2013-09-01" & maintain_handle_risk_situations$client_id != 1)
freqHandle  <- as.data.frame(table(maintain_handle_risk_situations$client_id))
length(freqHandle$Var1)

# maintain inform others
maintain_inform_others$time_stamp_inform_others  <- as.Date(maintain_inform_others$time_stamp_inform_others, "%d.%m.%Y")
maintain_inform_others  <- subset(maintain_inform_others, maintain_inform_others$time_stamp_inform_others >= "2013-02-01" & maintain_inform_others$time_stamp_inform_others <= "2013-09-01" & maintain_inform_others$client_id != 1)
freqOthers  <- as.data.frame(table(maintain_inform_others$client_id))
length(freqOthers$Var1)

# maintain pros e cons
maintain_pros_cons_drinking_less$time_stamp_maintain_proscons  <- as.Date(maintain_pros_cons_drinking_less$time_stamp_maintain_proscons, "%d.%m.%Y")
maintain_pros_cons_drinking_less  <- subset(maintain_pros_cons_drinking_less, maintain_pros_cons_drinking_less$time_stamp_maintain_proscons >= "2013-02-01" & maintain_pros_cons_drinking_less$time_stamp_maintain_proscons <= "2013-09-01" & maintain_pros_cons_drinking_less$client_id != 1)
freqPros  <- as.data.frame(table(maintain_pros_cons_drinking_less$client_id))
length(freqPros$Var1)

# craving
maintain_craving_make_commitment$time_stamp_commitment  <- as.Date(maintain_craving_make_commitment$time_stamp_commitment, "%d.%m.%Y")
maintain_craving_make_commitment  <- subset(maintain_craving_make_commitment, maintain_craving_make_commitment$time_stamp_commitment >= "2013-02-01" & maintain_craving_make_commitment$time_stamp_commitment <= "2013-09-01" & maintain_craving_make_commitment$client_id != 1)
freqCraving  <- as.data.frame(table(maintain_craving_make_commitment$client_id))
length(freqCraving$Var1)

# relapse
relapse$relapse_time_stamp  <- as.Date(relapse$relapse_time_stamp, "%d.%m.%Y")
relapse  <- subset(relapse, relapse$relapse_time_stamp >= "2013-02-01" & relapse$relapse_time_stamp <= "2013-09-01" & relapse$client_id != 1)
freqRelapse  <- as.data.frame(table(relapse$client_id))
length(freqRelapse$Var1)

# diary
diary$Time_stamp_drinksit  <- as.Date(diary$Time_stamp_drinksit, "%d.%m.%Y")
diaryU  <- subset(diary, diary$Time_stamp_drinksit >= "2013-02-01" & diary$Time_stamp_drinksit <= "2013-09-01" & diary$client_id != 1)
freqDiaryU  <- as.data.frame(table(diaryU$client_id))
length(freqDiaryU$Var1)

# total nr of times filled in / unique users
length(diary$client_id)/length(freqDiaryU$Var1)

#---- AUDIT ----

##--- Recoding sum of audit ---## 
# Scores computed using as AUDIT-BR manual as reference

audit$AuditCod[audit$Result <= 7]  <-  "Education"
audit$AuditCod[audit$Result > 7 &  audit$Result <= 15]  <-  "Basic advice"
audit$AuditCod[audit$Result > 15 &  audit$Result <= 19]  <-  "Brief Advice and brief counseling"
audit$AuditCod[audit$Result > 20]  <-  "Referral for Clinical"

tableAudit  <- table(audit$AuditCod)
barplot(tableAudit, names.arg=c("BA", "BA and BI", "Education", "Referral"))

#---- Forums ----

# Converting dates
forum_statistics$time_stamp_forum <- as.Date(forum_statistics$time_stamp_forum, "%d.%m.%Y")
table(forum_statistics$time_stamp_forum)

plot(table(forum_statistics$time_stamp_forum), type="l", ylim=c(0,8), xlab="Dates", ylab="Frequency")

forumGraphs  <- ggplot(forum_statistics, aes(time_stamp_forum, forum_visit, colour = Client_id)) + geom_line()
forumGraphs

#---- User login stats ----

cbind(table(user_logins_stats$login_1wk))

#---- Evaluation ----
names(evaluation)

# "Eu bebo de acordo com o o-que-é-beber-de-baixo-risco?"
evaluation$eval_step1_1  <- as.factor(evaluation$eval_step1_1)
levels(evaluation$eval_step1_1)[1]  <- "no"
levels(evaluation$eval_step1_1)[2]  <- "yes"
ggplot(evaluation, aes(evaluation$eval_step1_1)) + geom_bar() + ylim(0,300) + ylab("Frequency") + xlab("")

# Existem razões para que eu reduza meu consumo ou pare totalmente de beber?"
evaluation$eval_step1_2  <- as.factor(evaluation$eval_step1_2)
levels(evaluation$eval_step1_2)[1]  <- "no"
levels(evaluation$eval_step1_2)[2]  <- "yes"
ggplot(evaluation, aes(evaluation$eval_step1_2)) + geom_bar() + ylim(0,300) + ylab("Frequency") + xlab("")

# "O que pesa mais para mim: os prós ou contras em beber?"
evaluation$eval_step1_3  <- as.factor(evaluation$eval_step1_3)
levels(evaluation$eval_step1_3)[1]  <- "pros"
levels(evaluation$eval_step1_3)[2]  <- "cons"
ggplot(evaluation, aes(evaluation$eval_step1_3)) + geom_bar() + ylim(0,300) + ylab("Frequency") + xlab("")

# "Eu estou motivado em reduzir meu consumo de álcool?"
evaluation$eval_step1_4  <- as.factor(evaluation$eval_step1_4)
levels(evaluation$eval_step1_4)[1]  <- "no"
levels(evaluation$eval_step1_4)[2]  <- "yes"
ggplot(evaluation, aes(evaluation$eval_step1_4)) + geom_bar() + ylim(0,300) + ylab("Frequency") + xlab("")

# "Eu estou motivado em reduzir meu consumo de álcool?"
evaluation$eval_step1_4  <- as.factor(evaluation$eval_step1_4)
levels(evaluation$eval_step1_4)[1]  <- "no"
levels(evaluation$eval_step1_4)[2]  <- "yes"
ggplot(evaluation, aes(evaluation$eval_step1_4)) + geom_bar() + ylim(0,300) + ylab("Frequency") + xlab("")

# "Eu realmente quero mudar meu consumo de ?lcool (reduzir ou parar totalmente? " - Plase, check this recode method. Might be wrong.
evaluation$eval_step1_5  <- as.factor(evaluation$eval_step1_5)
levels(evaluation$eval_step1_5)[1]  <- "no, leaving"
levels(evaluation$eval_step1_5)[2]  <- "yes, carrying on"
ggplot(evaluation, aes(evaluation$eval_step1_5)) + geom_bar() + ylim(0,300) + ylab("Frequency") + xlab("")

#---- Evaluation after 6 weeks ----

# Descriptives
sapply(evaluation_after6weeks[, 4:10], describe)

# Comparing evaluation by weeks
boxplot(evaluation_after6weeks[,4:10], ylim=c(0,20), names=c("Ev1", "Ev2", "Ev3", "Ev4", "Ev5", "Ev6", "Ev7"))

#---- User logins ----
# check if client.id is replaced by row.names

#---- User Statistics ----
names(user_statistic)
head(user_statistic)

# Converting variable Average.login.time to something useful 
user_statistic$Average.login.time[1:10]
time  <- as.character(user_statistic$Average.login.time)
timeD  <- as.numeric(gsub("days(.*)", "", time), ); timeD[is.na(timeD)]  <- 0
timeH  <- as.numeric(gsub("(.*)days|hours(.*)", "", time)); timeH[is.na(timeH)]  <- 0
timeM  <- as.numeric(gsub("(.*)hours,| minutes(.*)", "", time)); timeM[is.na(timeM)]  <- 0
timeS  <- as.numeric(gsub("seconds", "", substr(time, nchar(time)-10+1, nchar(time))))
rm(timeD, timeH, timeM, timeS) # removing time objetcs
user_statistic$average.logtime  <- timeD*24*60 + timeH*60 +  timeM + timeS/60 # converting to minutes

# Descriptives
summary(user_statistic$average.logtime)

describe(user_statistic[, c("average.logtime", "Visits", "Emails", "Audit", "Your.Drinking", "Advantages.Disadvantages", "Goals")])
