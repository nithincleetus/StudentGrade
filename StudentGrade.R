setwd('D:/University/Data Science')
data <- read.csv('student_grades.csv', sep = ',', header = TRUE, stringsAsFactors=T)

#Studying the data
  
head(data)
str(data)
summary(data)
sapply(data,class)


#Checking for null values

sum(is.na(data))


library(tidyverse)
library(plyr)
library(dplyr)
library(plotly)
library(ggpubr)

# Converting to factor

new_data <- data 
new_data$Pass <- factor(new_data$Pass,levels=c(0,1), labels=c("Fail", "Pass"))


# Total Pass v Fail:

temp_pass <- new_data %>% group_by(Pass) %>% summarise(n=n())
temp_pass
ggplot(temp_pass, aes(x=Pass, y=n, fill=Pass)) + geom_bar(stat="identity") + labs(title = "Pass Fail Numbers",x = "Pass/Fail", y = "Count of Students")


# sex:

temp_sex <- new_data %>% group_by(sex,Pass) %>% summarise(n=n())
sex_plot <- ddply(temp_sex, "sex", transform, percent = n / sum(n) * 100)
ggplot_sex <- ggplot(sex_plot, aes(x=sex, y=percent, fill=Pass)) + geom_bar(stat="identity") + labs(title = "Gender Pass-Fail Percentage",x = "Gender", y = "Percent")

# age :

temp_age <- new_data %>% group_by(age,Pass) %>% summarise(n = n()) 
age_plot <- ddply(temp_age,"age", transform, percent = n / sum(n) * 100)
age_plot
ggplot_age <- ggplot(age_plot, aes(x = age, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Age Pass-Fail Numbers",x = "Age", y = "Count of Students")

# address :

temp_address <- new_data %>% group_by(address,Pass) %>% summarise(n = n()) 
address_plot <- ddply(temp_address,"address", transform, percent = n / sum(n) * 100)
address_plot
ggplot_address <- ggplot(address_plot, aes(x = address, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Address Pass-Fail Percentage",x = "Address", y = "Percent")

# school :

temp_school <- new_data %>% group_by(school,Pass) %>% summarise(n = n()) 
school_plot <- ddply(temp_school,"school", transform, percent = n / sum(n) * 100)
school_plot
ggplot_school <- ggplot(school_plot, aes(x = school, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "School Pass-Fail Percentage",x = "School", y = "Percent")

# famsize :

temp_famsize <- new_data %>% group_by(famsize,Pass) %>% summarise(n = n()) 
famsize_plot <- ddply(temp_famsize,"famsize", transform, percent = n / sum(n) * 100)
famsize_plot
ggplot_famsize <- ggplot(famsize_plot, aes(x = famsize, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Family Size Pass-Fail Percentage",x = "Family Size", y = "Percent")


# Pstatus :

temp_Pstatus <- new_data %>% group_by(Pstatus,Pass) %>% summarise(n = n()) 
Pstatus_plot <- ddply(temp_Pstatus,"Pstatus", transform, percent = n / sum(n) * 100)
Pstatus_plot
ggplot_Pstatus <- ggplot(Pstatus_plot, aes(x = Pstatus, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Parent cohabitation Pass-Fail Percentage",x = "Parent cohabitation", y = "Percent")

# Medu :

temp_Medu <- new_data %>% group_by(Medu,Pass) %>% summarise(n = n()) 
Medu_plot <- ddply(temp_Medu,"Medu", transform, percent = n / sum(n) * 100)
Medu_plot
ggplot_Medu <- ggplot(Medu_plot, aes(x = Medu, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Mother's Education Pass-Fail Numbers",x = "Mother's Education", y = "Count of Students")

# Fedu:

temp_Fedu <- new_data %>% group_by(Fedu,Pass) %>% summarise(n = n()) 
Fedu_plot <- ddply(temp_Fedu,"Fedu", transform, percent = n / sum(n) * 100)
Fedu_plot
ggplot_Fedu <- ggplot(Fedu_plot, aes(x = Fedu, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Father's Education Pass-Fail Numbers",x = "Father's Education", y = "Count of Students")

# Mjob:

temp_Mjob <- new_data %>% group_by(Mjob,Pass) %>% summarise(n = n()) 
Mjob_plot <- ddply(temp_Mjob,"Mjob", transform, percent = n / sum(n) * 100)
Mjob_plot
ggplot_Mjob <- ggplot(Mjob_plot, aes(x = Mjob, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Mother's Job Pass-Fail Numbers",x = "School", y = "Count of Students")


# Fjob :

temp_Fjob <- new_data %>% group_by(Fjob,Pass) %>% summarise(n = n()) 
Fjob_plot <- ddply(temp_Fjob,"Fjob", transform, percent = n / sum(n) * 100)
Fjob_plot
ggplot_Fjob <- ggplot(Fjob_plot, aes(x = Fjob, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Father's Job Pass-Fail Numbers",x = "School", y = "Count of Students")

# reason :

temp_reason <- new_data %>% group_by(reason,Pass) %>% summarise(n = n()) 
reason_plot <- ddply(temp_reason,"reason", transform, percent = n / sum(n) * 100)
reason_plot
ggplot_reason <- ggplot(reason_plot, aes(x = reason, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Reason for Schooling Pass-Fail Numbers",x = "Reason for Schooling", y = "Count of Students")

# guardian :

temp_guardian <- new_data %>% group_by(guardian,Pass) %>% summarise(n = n()) 
guardian_plot <- ddply(temp_guardian,"guardian", transform, percent = n / sum(n) * 100)
guardian_plot
ggplot_guardian <- ggplot(guardian_plot, aes(x = guardian, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Guardian Pass-Fail Percentage",x = "Guardian", y = "Percent")

# traveltime :

temp_traveltime <- new_data %>% group_by(traveltime,Pass) %>% summarise(n = n()) 
traveltime_plot <- ddply(temp_traveltime,"traveltime", transform, percent = n / sum(n) * 100)
traveltime_plot
ggplot_traveltime <- ggplot(traveltime_plot, aes(x = traveltime, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Travel Time Pass-Fail Numbers",x = "Travel Time", y = "Count of Students")
ggplot_traveltime_percent <- ggplot(traveltime_plot, aes(x = traveltime, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Travel Time Pass-Fail Percent",x = "Travel Time", y = "Percentage")

# studytime :

temp_studytime <- new_data %>% group_by(studytime,Pass) %>% summarise(n = n()) 
studytime_plot <- ddply(temp_studytime,"studytime", transform, percent = n / sum(n) * 100)
studytime_plot
ggplot_studytime <- ggplot(studytime_plot, aes(x = studytime, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Study Time Pass-Fail Numbers",x = "Study Time", y = "Count of Students")
ggplot_studytime_percent <- ggplot(studytime_plot, aes(x = studytime, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Study Time Pass-Fail Percent",x = "Study Time", y = "Percentage")

#failures :

temp_failures <- new_data %>% group_by(failures,Pass) %>% summarise(n = n()) 
failures_plot <- ddply(temp_failures,"failures", transform, percent = n / sum(n) * 100)
failures_plot
ggplot_failures <- ggplot(failures_plot, aes(x = failures, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Failures Pass-Fail Numbers",x = "Failures", y = "Count of Students")
ggplot_failures_percent <- ggplot(failures_plot, aes(x = failures, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Failures Pass-Fail Numbers",x = "Failures", y = "Count of Students")


# schoolsup : 

temp_schoolsup <- new_data %>% group_by(schoolsup,Pass) %>% summarise(n = n()) 
schoolsup_plot <- ddply(temp_schoolsup,"schoolsup", transform, percent = n / sum(n) * 100)
schoolsup_plot
ggplot_schoolsup <- ggplot(schoolsup_plot, aes(x = schoolsup, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "School Support Pass-Fail Percentage",x = "School Support", y = "Percent")

# famsup :

temp_famsup <- new_data %>% group_by(famsup,Pass) %>% summarise(n = n()) 
famsup_plot <- ddply(temp_famsup,"famsup", transform, percent = n / sum(n) * 100)
famsup_plot
ggplot_famsup <- ggplot(famsup_plot, aes(x = famsup, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Family Support Pass-Fail Percentage",x = "Family Support", y = "Percent")

# paid :

temp_paid <- new_data %>% group_by(paid,Pass) %>% summarise(n = n()) 
paid_plot <- ddply(temp_paid,"paid", transform, percent = n / sum(n) * 100)
paid_plot
ggplot_paid <- ggplot(paid_plot, aes(x = paid, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Paid Support Pass-Fail Percentage",x = "Paid Support", y = "Percent")


# activities :

temp_activities <- new_data %>% group_by(activities,Pass) %>% summarise(n = n()) 
activities_plot <- ddply(temp_activities,"activities", transform, percent = n / sum(n) * 100)
activities_plot
ggplot_activities <- ggplot(activities_plot, aes(x = activities, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Extra-curricular Activities Pass-Fail Percentage",x = "Extra-curricular Activities", y = "Percent")

# nursery :

temp_nursery <- new_data %>% group_by(nursery,Pass) %>% summarise(n = n()) 
nursery_plot <- ddply(temp_nursery,"nursery", transform, percent = n / sum(n) * 100)
nursery_plot
ggplot_nursery <- ggplot(nursery_plot, aes(x = nursery, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Nursery Attendance Status Pass-Fail Percentage",x = "Nursery Attendance Status ", y = "Percent")

# higher :

temp_higher <- new_data %>% group_by(higher,Pass) %>% summarise(n = n()) 
higher_plot <- ddply(temp_higher,"higher", transform, percent = n / sum(n) * 100)
higher_plot
ggplot_higher <- ggplot(higher_plot, aes(x = higher, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Intrested in higher education Pass-Fail Percentage",x = "Intrested in higher education", y = "Percent")


# internet : 

temp_internet <- new_data %>% group_by(internet,Pass) %>% summarise(n = n()) 
internet_plot <- ddply(temp_internet,"internet", transform, percent = n / sum(n) * 100)
internet_plot
ggplot_internet <- ggplot(internet_plot, aes(x = internet, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Access to Internet Pass-Fail Percentage",x = "Access to Internet", y = "Percent")

# romantic :

temp_romantic <- new_data %>% group_by(romantic,Pass) %>% summarise(n = n()) 
romantic_plot <- ddply(temp_romantic,"romantic", transform, percent = n / sum(n) * 100)
romantic_plot
ggplot_romantic <- ggplot(romantic_plot, aes(x = romantic, y = percent, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Involved in Romatic Relationship Pass-Fail Percentage",x = "Involved in Romatic Relationship", y = "Percent")


# famrel :

temp_famrel <- new_data %>% group_by(famrel,Pass) %>% summarise(n = n()) 
famrel_plot <- ddply(temp_famrel,"famrel", transform, percent = n / sum(n) * 100)
famrel_plot
ggplot_famrel <- ggplot(famrel_plot, aes(x = famrel, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Family Relationship Pass-Fail Numbers",x = "Family Relationship", y = "Count of Students")

# freetime :

temp_freetime <- new_data %>% group_by(freetime,Pass) %>% summarise(n = n()) 
freetime_plot <- ddply(temp_freetime,"freetime", transform, percent = n / sum(n) * 100)
freetime_plot
ggplot_freetime <- ggplot(freetime_plot, aes(x = freetime, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Free Time Pass-Fail Numbers",x = "Free Time", y = "Count of Students")

# goout :

temp_goout <- new_data %>% group_by(goout,Pass) %>% summarise(n = n()) 
goout_plot <- ddply(temp_goout,"goout", transform, percent = n / sum(n) * 100)
goout_plot
ggplot_goout <- ggplot(goout_plot, aes(x = goout, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Going Out Pass-Fail Numbers",x = "Going Out", y = "Count of Students")

# Dalc :

temp_dalc <- new_data %>% group_by(Dalc,Pass) %>% summarise(n = n()) 
Dalc_plot <- ddply(temp_dalc,"Dalc", transform, percent = n / sum(n) * 100)
Dalc_plot
ggplot_Dalc <- ggplot(Dalc_plot, aes(x = Dalc, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Weekday Alcohol Consumption Pass-Fail Numbers",x = "Weekday Alcohol Consumption", y = "Count of Students")

# Walc :

temp_Walc <- new_data %>% group_by(Walc,Pass) %>% summarise(n = n()) 
Walc_plot <- ddply(temp_Walc,"Walc", transform, percent = n / sum(n) * 100)
Walc_plot
ggplot_Walc <- ggplot(Walc_plot, aes(x = Walc, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Weekend Alcohol Consumption Pass-Fail Numbers",x = "Weekend Alcohol Consumption", y = "Count of Students")

# health :

temp_health <- new_data %>% group_by(health,Pass) %>% summarise(n = n()) 
health_plot <- ddply(temp_health,"health", transform, percent = n / sum(n) * 100)
health_plot
ggplot_health <- ggplot(health_plot, aes(x = health, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Health Status Pass-Fail Numbers",x = "Health Status", y = "Count of Students")

#absences

temp_absences <- new_data %>% group_by(absences,Pass) %>% summarise(n = n()) 
absences_plot <- ddply(temp_absences,"absences", transform, percent = n / sum(n) * 100)
absences_plot
ggplot_absences <- ggplot(absences_plot, aes(x = absences, y = n, fill = Pass)) + geom_bar(stat = "identity") + labs(title = "Attendance Pass-Fail Numbers",x = "Attendance", y = "Count of Students")

#Plotting the graphs

ggarrange(ggplot_school,ggplot_sex,ggplot_age,ggplot_address, ncol = 2, nrow = 2)
ggarrange(ggplot_famsize, ggplot_Pstatus,ncol = 2)
ggarrange(ggplot_Medu, ggplot_Fedu, ggplot_Mjob, ggplot_Fjob, ncol = 2, nrow = 2)
ggarrange(ggplot_reason, ggplot_guardian, ncol = 2)
ggarrange(ggplot_traveltime, ggplot_studytime, ncol = 2)
ggarrange(ggplot_traveltime_percent, ggplot_studytime_percent, ncol = 2)
ggplot_failures
ggplot_failures_percent
ggarrange(ggplot_schoolsup, ggplot_famsup, ggplot_paid, ggplot_activities, ggplot_nursery, ggplot_higher, ggplot_internet, ggplot_romantic, ncol = 3, nrow = 3)
ggarrange(ggplot_famrel, ggplot_freetime, ggplot_goout, ggplot_Dalc, ggplot_Walc, ggplot_health, ncol = 3, nrow = 2)
ggplot_absences





# Corealtion :

library("greybox")
corelation <- assoc(new_data)

corval <- corelation$value
corpvalue <- corelation$p.value
cortype <- corelation$type

pass_corval <- corval[31,]
pass_corpvalue <- corpvalue[31,]
pass_cortype <- cortype[31,]


#Chi Square for all attributes to double check the value.

chisq_sex<- chisq.test(new_data$Pass, new_data$sex)
chisq_age<- chisq.test(new_data$Pass, new_data$age)
chisq_address<- chisq.test(new_data$Pass, new_data$address)
chisq_famsize<- chisq.test(new_data$Pass, new_data$famsize)
chisq_Pstatus<- chisq.test(new_data$Pass, new_data$Pstatus)
chisq_Medu<- chisq.test(new_data$Pass, new_data$Medu)
chisq_Fedu<- chisq.test(new_data$Pass, new_data$Fedu)
chisq_Mjob<- chisq.test(new_data$Pass, new_data$Mjob)
chisq_Fjob<- chisq.test(new_data$Pass, new_data$Fjob)
chisq_reason<- chisq.test(new_data$Pass, new_data$reason)
chisq_guardian<- chisq.test(new_data$Pass, new_data$guardian)
chisq_traveltime<- chisq.test(new_data$Pass, new_data$traveltime)
chisq_studytime<- chisq.test(new_data$Pass, new_data$studytime)
chisq_failures<- chisq.test(new_data$Pass, new_data$failures)
chisq_schoolsup<- chisq.test(new_data$Pass, new_data$schoolsup)
chisq_famsup<- chisq.test(new_data$Pass, new_data$famsup)
chisq_paid<- chisq.test(new_data$Pass, new_data$paid)
chisq_activities<- chisq.test(new_data$Pass, new_data$activities)
chisq_nursery<- chisq.test(new_data$Pass, new_data$nursery)
chisq_higher<- chisq.test(new_data$Pass, new_data$higher)
chisq_internet<- chisq.test(new_data$Pass, new_data$internet)
chisq_romantic<- chisq.test(new_data$Pass, new_data$romantic)
chisq_famrel<- chisq.test(new_data$Pass, new_data$famrel)
chisq_freetime<- chisq.test(new_data$Pass, new_data$freetime)
chisq_goout<- chisq.test(new_data$Pass, new_data$goout)
chisq_Dalc<- chisq.test(new_data$Pass, new_data$Dalc)
chisq_Walc<- chisq.test(new_data$Pass, new_data$Walc)
chisq_health<- chisq.test(new_data$Pass, new_data$health)
chisq_absences<- chisq.test(new_data$Pass, new_data$absences)


format(round(pass_corpvalue, 5), nsmall = 5) #rounding off float to 5 decimal points 
pass_corval
pass_cortype


new_corpval <- format(round(pass_corpvalue, 2), nsmall = 2) > 0.05 #checking for null hypothesis
new_corpval 

chisq_sex
chisq_address
chisq_famsize

chisq_sex<- chisq.test(new_data$Pass, new_data$sex)
chisq_sex
library(rcompanion)
cramerV(new_data$Pass, new_data$sex)

chisq_higher<- chisq.test(new_data$Pass, new_data$higher)
chisq_higher
library(rcompanion)
cramerV(new_data$Pass, new_data$higher)

order(-pass_corval)
pass_corval[order(-pass_corval)]


pass_corpvalue[order(-pass_corpvalue)] #ordering the data with p value for feature selection

# Feature Selection :

new_data_backup <- new_data

new_data <- new_data[ , -which(names(new_data) %in% c("activities","romantic","nursery","famsup"))] 

names(new_data)

# Classifcation : 

#DT

set.seed(32)

new_data[,"train"] <- ifelse(runif(nrow(new_data))<0.8, 1, 0)
trainset <- new_data[new_data$train == "1",]
testset <- new_data[new_data$train == "0",]

names(new_data)
names(trainset)

trainset <- trainset[-28]
testset <- testset[-28]
names(trainset)



library(rpart)
library(rpart.plot)


pass_tree <- rpart(Pass~., data = trainset, method ='class')


rpart.plot(pass_tree, extra = 106)

test_data <- testset[-27]
tree_pred <- predict(pass_tree, newdata = test_data, type ='class')


table(predicted = tree_pred, actual = testset$Pass)
mean(tree_pred==testset$Pass)

#RF

library("party")


pass_rf <- cforest(Pass~., data = trainset, control = cforest_unbiased(mtry = 8, ntree = 100))
rf_prob <- predict(pass_rf, newdata = test_data, type = "response")

table(predicted = rf_prob, actual = testset$Pass)

mean(rf_prob==testset$Pass)


