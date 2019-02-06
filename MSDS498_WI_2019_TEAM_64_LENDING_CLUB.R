# Julio Testing Git on RStudio
# options(scipen = 999)

# getwd()
# setwd("C:/Users/nishidh/Documents/Personal/MSPA/MSDS 498 Capstone")
# Manually Set Workng Directory

# Read Data
data = read.csv("loan.csv")

# Show First Records
head(data)

# Summary of data
summary(data)

#EDA of Dataset
install.packages("DataExplorer")
library(DataExplorer)
introduce(data)

plot_intro(data)

# Missing Data
pMiss <- function(x){sum(is.na(x))/length(x)*100}
sort(format(apply(data,2,pMiss),scientific=F),decreasing=T)

# Graph Missing Values
plot_missing(data)

# Not Used?
# install.packages("mice")
# library(mice)
# md.pattern(data)

# Distributions of Loan Amounts
par(mfrow = c(2,3))
# Histograms
# Loan Amnt
hist(data$loan_amnt,
          type = "percent",
          main = "Loan Applied for by the Borrower",
          xlab = "Amount",
          col = "#113C5E",
          breaks = 10)

# Funded Amnt
hist(data$funded_amnt,
     type = "percent",
     main = "Amount Commited to Loan",
     xlab = "Amount",
     col = "#113C5E",
     breaks = 10)

# Funded Amnt Inv
hist(data$funded_amnt_inv,
     type = "percent",
     main = "Amount Commited by Investors",
     xlab = "Amount",
     col = "#113C5E",
     breaks = 10)

# QQ- Plots
# Loan Amnt
qqnorm(data$loan_amnt, 
       col = "#81CCE3",
       main = "Loan Applied for by the Borrower")
qqline(data$loan_amnt, 
       col = "#EA4225", 
       lty = 2,
       lwd = 2)

# Funded Amnt
qqnorm(data$funded_amnt, 
       col = "#81CCE3",
       main = "Amount Commited to Loan")
qqline(data$funded_amnt, 
       col = "#EA4225", 
       lty = 2,
       lwd = 2)

# Funded Amnt Inv
qqnorm(data$funded_amnt_inv, 
       col = "#81CCE3",
       main = "Amount Commited by Investors")
qqline(data$funded_amnt_inv, 
       col = "#EA4225", 
       lty = 2,
       lwd = 2)

par(mfrow = c(1,1))

# dti_joint and annual_inc_joint have over 99.9% missing data. This is due to the small representation of joint loan applications.
summary(data$application_type) #Only 511 of all 887379 are joint application (99.94241% is single application)
1-(511/(886868+511))

# These follwoing fields could be useful to predict the default for the joint applicants.
summary(data[which(data$application_type=="JOINT"),]$dti_joint) # Only 2 (out of 511) missing
summary(data[which(data$application_type=="JOINT"),]$annual_inc_joint) # None missing for those joint applicants
summary(data[which(data$application_type=="JOINT"),]$verification_status_joint) # None missing

# There are 14 variables that have 98% of the data missing and 3 variables that have 8% of the data missing. Why is that?
# Looking at the missing values chronologically...
# The codes below reveal that they started collecting data on those variables with 98% missing data in December 2015 (the last month of the available data).
# and collecting data on those variables with 8% missing data in AUGUST 2012.


library(zoo)
summary(data$issue_d)
unique(data$issue_d)
data$issue_d2 <- as.yearmon(as.character(paste(substr(as.character(data$issue_d),5,8),substr(as.character(data$issue_d),1,3),sep="-")), "%Y-%b")
##paste(substr(as.character(data$issue_d),5,8),match(substr(as.character(data$issue_d),1,3),month.abb),sep=".")
data$issue_d2-data

#57
summary(data[which(data$issue_d2<="JUL 2012"),]$tot_coll_amt)
summary(data[which(data$issue_d2<="AUG 2012"),]$tot_coll_amt)
summary(data[which(data$issue_d2>="SEP 2012"),]$tot_coll_amt)

#58
summary(data[which(data$issue_d2<="JUL 2012"),]$tot_cur_bal)
summary(data[which(data$issue_d2<="AUG 2012"),]$tot_cur_bal)
summary(data[which(data$issue_d2>="SEP 2012"),]$tot_cur_bal)

#59
summary(data[which(data$issue_d2<="NOV 2015"),]$open_acc_6m)
summary(data[which(data$issue_d2=="DEC 2015"),]$open_acc_6m)

#60
summary(data[which(data$issue_d2<="NOV 2015"),]$open_il_6m)
summary(data[which(data$issue_d2=="DEC 2015"),]$open_il_6m)

#61
summary(data[which(data$issue_d2<="NOV 2015"),]$open_il_12m)
summary(data[which(data$issue_d2=="DEC 2015"),]$open_il_12m)

#62
summary(data[which(data$issue_d2<="NOV 2015"),]$open_il_24m)
summary(data[which(data$issue_d2=="DEC 2015"),]$open_il_24m)

#63
summary(data[which(data$issue_d2<="NOV 2015"),]$mths_since_rcnt_il)
summary(data[which(data$issue_d2=="DEC 2015"),]$mths_since_rcnt_il)

#64
summary(data[which(data$issue_d2<="NOV 2015"),]$total_bal_il)
summary(data[which(data$issue_d2=="DEC 2015"),]$total_bal_il)

#65
summary(data[which(data$issue_d2<="NOV 2015"),]$il_util)
summary(data[which(data$issue_d2=="DEC 2015"),]$il_util)

#67
summary(data[which(data$issue_d2<="NOV 2015"),]$open_rv_12m)
summary(data[which(data$issue_d2=="DEC 2015"),]$open_rv_12m)

#68
summary(data[which(data$issue_d2<="NOV 2015"),]$open_rv_24m)
summary(data[which(data$issue_d2=="DEC 2015"),]$open_rv_24m)

#69
summary(data[which(data$issue_d2<="NOV 2015"),]$max_bal_bc)
summary(data[which(data$issue_d2=="DEC 2015"),]$max_bal_bc)

#70
summary(data[which(data$issue_d2<="NOV 2015"),]$all_util)
summary(data[which(data$issue_d2=="DEC 2015"),]$all_util)

#71
summary(data[which(data$issue_d2<="JUL 2012"),]$total_rev_hi_lim)
summary(data[which(data$issue_d2<="AUG 2012"),]$total_rev_hi_lim)
summary(data[which(data$issue_d2>="SEP 2012"),]$total_rev_hi_lim)

#72
summary(data[which(data$issue_d2<="NOV 2015"),]$inq_fi)
summary(data[which(data$issue_d2=="DEC 2015"),]$inq_fi)

#73
summary(data[which(data$issue_d2<="NOV 2015"),]$total_cu_tl)
summary(data[which(data$issue_d2=="DEC 2015"),]$total_cu_tl)

#74
summary(data[which(data$issue_d2<="NOV 2015"),]$inq_last_12m)
summary(data[which(data$issue_d2=="DEC 2015"),]$inq_last_12m)

#28
summary(data[which(data$delinq_2yrs==0),]$mths_since_last_delinq)
summary(data[which(data$mths_since_last_delinq>=0),]$delinq_2yrs)

#29
(mths_since_last_record)

#51
mths_since_last_major_derog

#######################
######### EDA #########
#######################

sort(unique(data$issue_d2)) #Earliest data from June 2007, the latest data from Dec 2015

#####################
# Response Variable #
#####################
summary(data$loan_status)
data$loan_status2 = as.factor(ifelse(data$loan_status=='Charged Off','Default',
                           ifelse(data$loan_status=='Current','Current',
                                  ifelse(data$loan_status=='Default','Default',
                                         ifelse(data$loan_status=='Does not meet the credit policy. Status:Charged Off','Default',
                                                ifelse(data$loan_status=='Does not meet the credit policy. Status:Fully Paid','Fully Paid',
                                                       ifelse(data$loan_status=='Fully Paid','Fully Paid','Current')))))))

# Assuming that those Fully Paid loans might have become "Grace Period" or "Late" in a course of loan period.
# Those may have higher chance of getting defaulted than those are in "current" status. However, they may still be "Fully Paid".

summary(data$loan_status2) # 630440 (Current), 47228 (Default), 209711 (Fully Paid)

# Bar Plots of Loan Status and Loan Status2
install.packages("ggplot2")
library(ggplot2)

plot1 <- ggplot(data, aes(x = loan_status, fill = loan_status)) +
                geom_bar() +
                theme_bw() +
                theme(legend.position = "none") + 
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                ylab("Total Count") +
                ggtitle("Distribution by Loan Status") + 
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                theme(axis.title.x=element_blank())

plot2 <- ggplot(data, aes(x = loan_status2, fill = loan_status2)) +
                geom_bar() +
                theme_bw() +
                theme(legend.position = "none") + 
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                ylab("Total Count") +
                ggtitle("Distribution by Loan Status2") + 
                theme(plot.title = element_text(hjust = 0.5)) + 
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                theme(axis.title.x=element_blank())

require(gridExtra)
grid.arrange(plot1, plot2, nrow = 1)


######################
# Predictor Variable #
######################

names(data)
attach(data)

length(unique(data$id)) #887379
length(unique(data$member_id)) #887379 
# This tells that not one person borrowed more than once OR they do not keep the same member_id so no way to track multiple borrowing

hist(data$loan_amnt)
length(data[which(data$loan_amt==0),])
nrow(data[which(data$loan_amt==0),]) # no rows shows $0 requested

hist(data$funded_amnt)
nrow(data[which(data$funded_amnt==0),]) #0 rows shows $0 for funded_amnt_inv

hist(data$funded_amnt_inv)
nrow(data[which(data$funded_amnt_inv==0),]) #233 rows shows $0 for funded_amnt_inv
data[which(data$funded_amnt_inv==0),]

summary(data[which(data$funded_amnt!=0),]$funded_amnt/data[which(data$loan_amnt!=0),]$loan_amnt)
hist(data[which(data$funded_amnt!=0),]$funded_amnt/data[which(data$loan_amnt!=0),]$loan_amnt,breaks=1000)
hist(data[which(data$funded_amnt!=0),]$funded_amnt/data[which(data$loan_amnt!=0),]$loan_amnt,xlim=c(0.99,1),breaks=1000)
boxplot(data[which(data$funded_amnt!=0),]$funded_amnt/data[which(data$loan_amnt!=0),]$loan_amnt)
# Majority of the loan is approved for full 100% or almost 100%.

## Funded Amount vs. Loan Status
boxplot(data$funded_amnt~data$loan_status2)

data$loan_approved_perc= data[which(data$funded_amnt!=0),]$funded_amnt/data[which(data$loan_amnt!=0),]$loan_amnt
(1-0.9976988)*100
length(data[which(data$loan_approved_perc==1.00),]$loan_approved_perc) #885317 (99.76763% of loans are approved at 100% or more of the original loan request amount)
length(data[which(data$loan_approved_perc>=0.99),]$loan_approved_perc) #885337 (99.76988% of loans are approved at 99% or more of the original loan request amount)
length(data[which(data$loan_approved_perc<0.99),]$loan_approved_perc) # only 2042 cases were approved less than 99% of the original requested amount.
length(data[which(data$loan_approved_perc<0.90),]$loan_approved_perc) # 1930 cases were approved less than 90% of the original requested amount.
length(data[which(data$loan_approved_perc<1.00),]$loan_approved_perc) # 1930 cases were approved less than 90% of the original requested amount.

hist(data[which(data$loan_approved_perc<1.00),]$loan_approved_perc,
     breaks=20, 
     type = "percent",
     col = "#113C5E",
     xlab = '% of the Original Requested Loan Amount', 
     main='Distributions of Loans Approved < 100%
           (The whole population here represents 0.23% of the total)',
     ylim = c(0,800),
     xlim = c(0,1))

summary(data$term) # 36M = 621125 (~70%), 60M = 266254 (~30%)

summary(data$int_rate)
hist(data$int_rate, main="Interest Rate",xlab="Interest Rate")
boxplot(data$int_rate~data$grade, main="Interest Rate by Credit Grade",xlab="Credit Grade", ylab="Interest Rate")
boxplot(data$int_rate~data$sub_grade, main="Interest Rate by Detailed Credit Grade",xlab="Credit Grade", ylab="Interest Rate")

summary(data$installment)
hist(data$installment, main="Installment Amount",xlab="Installment Amount")
hist(data[which(data$loan_status2=='Default'),]$installment, main="Installment Amount - Default",xlab="Installment Amount",col=rgb(1,1,0,0.7),ylim=c(0,50000))
par(new=T)
hist(data[which(data$loan_status2=='Fully Paid'),]$installment, main="",xlab="", col=rgb(0,1,1,0.4),ylim=c(0,50000))

unique(data$emp_title) # It has a mix of job titles and employers in this field, making the data unusable.

summary(emp_length)
data$emp_length2 = as.numeric(ifelse(data$emp_length=='< 1 year',1,
                                     ifelse(data$emp_length=='1 year',1,
                                            ifelse(data$emp_length=='2 years',2,
                                                   ifelse(data$emp_length=='3 years',3,
                                                          ifelse(data$emp_length=='4 years',4,
                                                                 ifelse(data$emp_length=='5 years',5,
                                                                        ifelse(data$emp_length=='6 years',6,
                                                                               ifelse(data$emp_length=='7 years',7,
                                                                                      ifelse(data$emp_length=='8 years',8,
                                                                                             ifelse(data$emp_length=='9 years',9,
                                                                                                    ifelse(data$emp_length=='10+ years',10,NA))))))))))))

str(data$emp_length2)
str(data$loan_status2)
boxplot(data$emp_length2~data$loan_status2) #Does numerical number of years help differentiate whether the loan ending up as default or fully paid?

summary(data$home_ownership)
data$home_ownership2 = as.factor(ifelse(data$home_ownership=='MORTGAGE','MORTGAGE',
                                        ifelse(data$home_ownership=='OWN','OWN',
                                               ifelse(data$home_ownership=='RENT','RENT','RENT'))))
plot(data[which(data$loan_status2!='Current'),]$home_ownership2,data[which(data$loan_status2!='Current'),]$loan_status2)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(data[which(data$loan_status2!='Current'),], row.vars = "home_ownership2", col.vars = "loan_status2", type = c("f", "row.pct"), addmargins = FALSE)

summary(data$annual_inc)
plot(data$annual_inc~data$funded_amnt)

head(data$verification_status)
unique(data$verification_status) #Verified, Source Verified, and Not Verified.
summary(data$verification_status)/length(data$verification_status) # about 1/3 each
# Having all 3 categories impact on the response variable (loan_status2)? 
# Or can we consolidate "Verified" and "Source Verified"?
plot(data[which(data$loan_status2!='Current'),]$loan_status2~data[which(data$loan_status2!='Current'),]$verification_status)
# This plot reveals Verified and Source Verified have higher default ratio than Not Verified.

# As performed in the very beginning, issue_d2 (yearmon) has been created based on issue_d (factor).
summary(issue_d)
# Crosstab - Current vs. Default vs. Fully Paid
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(data, row.vars = "issue_d2", col.vars = "loan_status2", type = c("f", "row.pct"), addmargins = FALSE)

# Excluding "Current", default ratio fluctuates between 12%-32%.
crosstab(data[which(data$loan_status2!='Current'),], row.vars = "issue_d2", col.vars = "loan_status2", type = c("f", "row.pct"), addmargins = FALSE)

crosstab(data[which(data$loan_status2!='Current'),], row.vars = "issue_d2", col.vars = "loan_status2", type = c("row.pct"), addmargins = FALSE)

# Taking the default % and created csv file for Time Series analysis.
default_ratio=read.csv("default_ratio.csv")

# Is there any Time Series components we need to consider???
TS = ts(data=default_ratio$Default,start=c(2007,7), frequency=12)
summary(TS)
plot(TS)

TScomponents_Additive <- decompose(TS, type = "additive", filter = NULL)
TScomponents_Multicative <- decompose(TS, type = "multiplicative", filter = NULL)

plot(TScomponents_Additive)
plot(TScomponents_Multicative)

# Default was in a decreasing trend from 2008 through 2010 and it startes trending up towards 2014.
# This may be solely because the age of the loans is fairly young and the defalut/write off has not started happening yet.


par(mfrow=c(2,1))
par(mar = c(1.5,2,2,1))
qqnorm(TScomponents_Additive$random, main = "Additive$Random")
qqline(TScomponents_Additive$random, col="red")
qqnorm(TScomponents_Multicative$random, main = "Multicative$Random")
qqline(TScomponents_Multicative$random, col="red")
par(mfrow=c(1,1))

# Just out of curiosity... Obviously this is wrong as I am using the incomplete 2014-2015 data that have 0% default because they have just started.
library(forecast)
Forecasts <- HoltWinters(TS, beta = NULL, gamma = NULL)
Forecasts_60 <- forecast(Forecasts,h=60)
plot(Forecasts_60)

summary(data$pymnt_plan) # only 10 people have the payment plan. Not significant enough for analysis.
head(data$url) # seems like a link to the loan account. It requires username and password to log in. Not useful for analysis.
summary(data$desc) # majority of the description is debt consolidation. 
# Maybe we could create two categories debt consolidation or Not debt consolidation.
# It requires text string search on this colum to extract ALL debt consolidation.

summary(data$purpose)# rather than using the "desc" above, this field as 14 different factor variables.
summary(data$title) # this may be too granular to use as a categorical variables.

summary(data$zip_code)
# I was vaguely remember reading an article or receiving a lecture that use of zip code for analytics could be viewed as discriminatory and immoral. 
# Maybe we need some research before determining whether we use zip or not?

summary(data$addr_state) # Way too many to use as categorical variable. 
# Maybe we could put them into regions and see how response variable woulr react based on the region?

#DTI (Debt to Income Ratio - this excludes mortgage and the requested LC loan)
summary(data$dti) #some people have 9999 times more debt than their income? Hard to believe those people actually get their loans approved. Typo?
head(sort(data$dti,decreasing = T),20)  # turned out only two people have dti of 9999. Only 11 people have dti higher than 100.
hist(data[which(data$dti<=100),]$dti) # majority of borrowers have dti between 0 and 40.

summary(data$delinq_2yrs)
row.names(data[is.na(data$delinq_2yrs),])
head(sort(data$delinq_2yrs,decreasing = T),100)
table(data$delinq_2yrs) # surprisingly there are handful of people who got delinquency more than twice. 
# I guess if a person can't pay, he/she can't pay any of the loan/debt?
# The question is is this information at the time of the data pull or at the time of loan approval.
# I am guessing the former as you would not approve loan to people who went delinquency 2+ times in the last 2 years?

summary(data$earliest_cr_line) # this may give us an idea how old those borrowers are?

summary(data$inq_last_6mths)
row.names(data[is.na(data$inq_last_6mths),]) # Same exact 29 records as delinq_2yrs are missing - needs imputation? Or dropped?

summary(data$open_acc)
row.names(data[is.na(data$open_acc),]) # Same exact 29 records as delinq_2yrs are missing - needs imputation? Or dropped?

summary(data$pub_rec)
row.names(data[is.na(data$pub_rec),])
length(row.names(data[is.na(data$pub_rec),])) # Same exact 29 records as delinq_2yrs are missing - needs imputation? Or dropped?

summary(data$revol_bal)
par(mar = c(2,4,4,2))
hist(data$revol_bal,breaks = 100)
head(sort(data$revol_bal,decreasing = T),300) #Top 235 have 400K+, Top 125 have 500+, Top 12 have 1M+ Revolving Balance
hist(data[which(data$revol_bal<400000),]$revol_bal,breaks = 100)

summary(data$revol_util)
row.names(data[is.na(data$revol_util),])
length(row.names(data[is.na(data$revol_util),])) # 502 variables missing

# According to the function below, previous 29 rows that had missing values for delinq_2yrs are also missing revol_util as well.
length(unique(c(row.names(data[is.na(data$revol_util),]),
                row.names(data[is.na(data$pub_rec),]),
                row.names(data[is.na(data$open_acc),]))))

summary(data$total_acc)
row.names(data[is.na(data$total_acc),])
length(row.names(data[is.na(data$total_acc),])) # Same exact 29 records as delinq_2yrs are missing - needs imputation? Or dropped?

summary(data$initial_list_status) #There are f and w, almost 50/50.

summary(data$out_prncp)

summary(data$out_prncp_inv)

summary(data$total_pymnt)

summary(data$total_pymnt_inv)

summary(data$total_rec_prncp)

summary(data$total_rec_int) 

summary(data$total_rec_late_fee) # We would not have this information for holdout set. Should not use for analysis (endogeneity).

summary(data$recoveries) # We would not have this information for holdout set. Should not use for analysis (endogeneity).

summary(data$collection_recovery_fee) # We would not have this information for holdout set. Should not use for analysis (endogeneity).

summary(data$last_pymnt_d)
data$last_pymnt_d2 <- as.yearmon(as.character(paste(substr(as.character(data$last_pymnt_d),5,8),substr(as.character(data$last_pymnt_d),1,3),sep="-")), "%Y-%b")
summary(data$last_pymnt_d2)

summary(data$last_pymnt_amnt)

summary(data$next_pymnt_d)
data$next_pymnt_d2 <- as.yearmon(as.character(paste(substr(as.character(data$next_pymnt_d),5,8),substr(as.character(data$next_pymnt_d),1,3),sep="-")), "%Y-%b")
summary(data$next_pymnt_d2)
unique(data[is.na(data$next_pymnt_d2),]$loan_status2) # Those people who have no next_pymnt_d2 are those who fully Paid or Default.

summary(data$last_credit_pull_d)
data$last_credit_pull_d2 <- as.yearmon(as.character(paste(substr(as.character(data$last_credit_pull_d),5,8),substr(as.character(data$last_credit_pull_d),1,3),sep="-")), "%Y-%b")
summary(data$last_credit_pull_d2) #53 has no last_credit_pull date

summary(data$collections_12_mths_ex_med)

summary(data$policy_code)
str(as.factor(data$policy_code)) # There is no policy_code = 2, Since this field only shows 1. this field has no use.

summary(data$application_type) #886868 INDIVIDUAL vs. 511 JOINT applications

summary(data$verification_status_joint) # Only those 511 Joint appliations have classifications

