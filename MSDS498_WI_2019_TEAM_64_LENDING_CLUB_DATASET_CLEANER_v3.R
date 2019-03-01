
getwd()
setwd("C:/Users/nishidh/Documents/Personal/MSPA/MSDS 498 Capstone")
data=read.csv("loan.csv")

data$profit = data$total_pymnt + data$recoveries - data$funded_amnt - data$collection_recovery_fee

data2 = data[,c("loan_amnt", "funded_amnt", "funded_amnt_inv", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length",
                "home_ownership", "annual_inc", "verification_status", "issue_d", "loan_status", "purpose", "zip_code", "addr_state",
                "dti", "delinq_2yrs", "earliest_cr_line", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record",
                "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "mths_since_last_major_derog",
                "application_type", "acc_now_delinq", "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim", "profit")]

data2$loan_status2 = as.factor(ifelse(data2$loan_status=='Charged Off','Default',
                                      ifelse(data2$loan_status=='Current','Current',
                                             ifelse(data2$loan_status=='Default','Default',
                                                    ifelse(data2$loan_status=='Does not meet the credit policy. Status:Charged Off','Default',
                                                           ifelse(data2$loan_status=='Does not meet the credit policy. Status:Fully Paid','Fully Paid',
                                                                  ifelse(data2$loan_status=='Fully Paid','Fully Paid','Current')))))))

######## NEW VARIABLE ########
data2$loan_default = as.factor(ifelse(data2$loan_status2 == "Default","YES","NO"))

library(zoo)
data2$issue_d2 <- as.yearmon(as.character(paste(substr(as.character(data2$issue_d),5,8),substr(as.character(data2$issue_d),1,3),sep="-")), "%Y-%b")
data2$earliest_cr_line2 <- as.yearmon(as.character(paste(substr(as.character(data2$earliest_cr_line),5,8),substr(as.character(data2$earliest_cr_line),1,3),sep="-")), "%Y-%b")
data2$mnth_sinc_earliest_cr_line = (data2$issue_d2-data2$earliest_cr_line2)*12

data2$mths_since_last_delinq[is.na(data2$mths_since_last_delinq)] = 9999
data2$mths_since_last_record[is.na(data2$mths_since_last_record)] = 9999
data2$mths_since_last_major_derog[is.na(data2$mths_since_last_major_derog)] = 9999


data2$home_ownership2 = as.factor(ifelse(data2$home_ownership=='MORTGAGE','MORTGAGE',
                                         ifelse(data2$home_ownership=='OWN','OWN',
                                                ifelse(data2$home_ownership=='RENT','RENT','RENT'))))
plot(data2[which(data2$loan_status2!='Current'),]$home_ownership2,data2[which(data2$loan_status2!='Current'),]$loan_status2)
summary(data2$home_ownership2)

pMiss <- function(x){sum(is.na(x))/length(x)*100}
#sort(format(apply(data2,2,pMiss),scientific=F),decreasing=T)

unique(data2$application_type)
data2=data2[which(data2$application_type=="INDIVIDUAL"),] #Remove 511 Joint Applications

which(colnames(data2)=="delinq_2yrs") # Remove those 29 applications that are commonly missing "delinq_2yrs", "inq_last_6mths", 
# "open_acc", "pub_rec", "total_acc", "acc_now_delinq", "earliest_cr_line2", "mnth_sinc_earliest_cr_line".
data2=data2[complete.cases(data2[,19]),]

#sort(format(apply(data2,2,pMiss),scientific=F),decreasing=T)
hist(data2$tot_coll_amt)
median(data2$tot_coll_amt,na.rm = T) # 0
mean(data2$tot_coll_amt,na.rm = T) # 225.7026
data2$tot_coll_amt[is.na(data2$tot_coll_amt)] = median(data2$tot_coll_amt,na.rm = T)

hist(data2$tot_cur_bal)
median(data2$tot_cur_bal,na.rm = T) # 80528
mean(data2$tot_cur_bal,na.rm = T) # 139438.7
data2$tot_cur_bal[is.na(data2$tot_cur_bal)] = median(data2$tot_cur_bal,na.rm = T)

hist(data2$total_rev_hi_lim)
median(data2$total_rev_hi_lim,na.rm = T) # 23700
mean(data2$total_rev_hi_lim,na.rm = T) # 32072.72
data2$total_rev_hi_lim[is.na(data2$total_rev_hi_lim)] = median(data2$total_rev_hi_lim,na.rm = T)

hist(data2$revol_util)
median(data2$revol_util,na.rm = T) # 56
mean(data2$revol_util,na.rm = T) # 55.06315
data2$revol_util[is.na(data2$revol_util)] = median(data2$revol_util,na.rm = T)

#creation of new_dti
data2$dti = data2$dti/100
data2$new_dti = ((((data2$dti)*data2$annual_inc)+data2$funded_amnt)/data2$annual_inc)
head(data2[,c(2,11,18,41)],10) # confirming the math

#creation of identifier for LC invested loans
data2$LC_invest = as.factor(ifelse((data2$funded_amnt - data2$funded_amnt_inv)>0,"Y","N"))

sort(format(apply(data2,2,pMiss),scientific=F),decreasing=T)
summary(data2)

names(data2)

data3 = data2[,c("loan_amnt", "funded_amnt", "funded_amnt_inv", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length",
                 "home_ownership2", "annual_inc", "verification_status", "issue_d2", "loan_status2", "loan_default", "purpose", "zip_code", "addr_state",
                 "dti", "new_dti", "delinq_2yrs", "mnth_sinc_earliest_cr_line", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record",
                 "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "mths_since_last_major_derog",
                 "acc_now_delinq", "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim","LC_invest","profit")]
colnames(data3)
library(corrplot)
# Selecting only numerical variables
corrplot(cor(data3[,c("loan_amnt", "funded_amnt", "funded_amnt_inv","int_rate", "installment", "annual_inc","dti",
                      "new_dti", "mnth_sinc_earliest_cr_line", "revol_bal", "revol_util", "total_acc",
                      "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim")]))

summary(data3)

# Created a function winsorising data that are <2% and 98%+
Winsorize <- function(x){
  quantiles <- quantile( x, c(.02, .98),na.rm = T)
  x[ x < quantiles[1] ] = quantiles[1]
  x[ x > quantiles[2] ] = quantiles[2]
  x
}

data3$original_funded_amnt = data3$funded_amnt
Winsorized_data3 <- apply(data3[,c("loan_amnt", "funded_amnt", "funded_amnt_inv","int_rate", "installment", "annual_inc","dti",
                                   "new_dti", "mnth_sinc_earliest_cr_line", "open_acc", "revol_bal", "revol_util", "total_acc",
                                   "mths_since_last_record","mths_since_last_delinq","mths_since_last_major_derog",
                                   "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim")],2,Winsorize)
summary(Winsorized_data3)

data3.1 = cbind(Winsorized_data3,data3[,c("term", "grade", "sub_grade", "emp_length", "home_ownership2", "verification_status",
                                          "issue_d2", "loan_status2", "loan_default", "purpose", "zip_code", "addr_state",
                                          "delinq_2yrs","inq_last_6mths", "pub_rec", "initial_list_status", "acc_now_delinq",
                                          "LC_invest","original_funded_amnt","profit")])

summary(data3.1)

data4 = data3.1[which(data3.1$loan_status2!='Current'),]
summary(data4)
data4$loan_amnt = log(data4$loan_amnt)
data4$funded_amnt = log(data4$funded_amnt)
data4$funded_amnt_inv = log(data4$funded_amnt_inv)
data4$int_rate = log(data4$int_rate/100)
data4$installment = log(data4$installment)
data4$annual_inc = log(data4$annual_inc)
data4$delinq_2yrs = as.factor(ifelse(data4$delinq_2yrs==0,0,ifelse(data4$delinq_2yrs==1,1,ifelse(data4$delinq_2yrs==2,2,"3+"))))
data4$mnth_sinc_earliest_cr_line = log(data4$mnth_sinc_earliest_cr_line)
data4$inq_last_6mths = as.factor(ifelse(data4$inq_last_6mths==0,0,ifelse(data4$inq_last_6mths==1,1,ifelse(data4$inq_last_6mths==2,2,"3+"))))
#summary(as.factor(data4$inq_last_6mths))
data4$mths_since_last_delinq = as.factor(ifelse(data4$mths_since_last_delinq==9999,0,1))
data4$mths_since_last_record = as.factor(ifelse(data4$mths_since_last_record==9999,0,1))
data4$open_acc = as.factor(ifelse(data4$open_acc <= 5,"1-5",ifelse(data4$open_acc <= 10, "6-10",
                                                                   ifelse(data4$open_acc <=15,"11-15",ifelse(data4$open_acc <= 20,"16-20",
                                                                                                             ifelse(data4$open_acc <=25,"21-25","26+"))))))
#summary(as.factor(data4$open_acc))
data4$pub_rec = as.factor(ifelse(data4$pub_rec==0,0,1)) 
#summary(as.factor(data4$pub_rec))
data4$revol_bal = log(data4$revol_bal)
data4$revol_util = log(data4$revol_util/100)
data4$total_acc = as.factor(ifelse(data4$total_acc <= 10,"1-10",ifelse(data4$total_acc <= 20, "11-20",
                                                                       ifelse(data4$total_acc <=30,"21-30",ifelse(data4$total_acc <= 40,"31-40",
                                                                                                                  ifelse(data4$total_acc <=50,"41-50","51+"))))))

data4$mths_since_last_major_derog = as.factor(ifelse(data4$mths_since_last_major_derog==9999,0,1))
data4$acc_now_delinq = as.factor(ifelse(data4$acc_now_delinq==0,0,1))
data4$tot_coll_amt = log(data4$tot_coll_amt+1)
data4$tot_cur_bal = log(data4$tot_cur_bal)
data4$total_rev_hi_lim = log(data4$total_rev_hi_lim+1)
data4$loan_status2 = factor(data4$loan_status2)

data4$is_emp_length_na = as.factor(ifelse(data4$emp_length=="n/a",1,0))
summary(data4$is_emp_length_na)
data4$is_small_business = as.factor(ifelse(data4$purpose=="small_business",1,0))
summary(data4$is_small_business)

summary(data4)
str(data4$loan_status2)

#write.csv(data4,"data4.csv")
#write.csv(train,"train.csv")
#write.csv(test,"test.csv")

set.seed(1)
data4$train_test <- runif(n=dim(data4)[1],min=0,max=1)

# Create train/test split;
train <- subset(data4, train_test<0.70);
test  <- subset(data4, train_test>=0.70);
names(train)

str(data4)

corrplot(cor(data4[,c("loan_amnt", "funded_amnt", "funded_amnt_inv","int_rate", "installment", "annual_inc","dti",
                      "new_dti", "mnth_sinc_earliest_cr_line", "revol_bal", "revol_util",
                      "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim")]))

#ggpairs(data4[,c("loan_amnt", "funded_amnt", "funded_amnt_inv", "term", "int_rate", "installment", "grade", "emp_length",
#                     "home_ownership2", "annual_inc", "verification_status", "loan_status2", "purpose",
#                     "dti", "new_dti", "delinq_2yrs", "mnth_sinc_earliest_cr_line", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record",
#                     "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "mths_since_last_major_derog",
#                     "acc_now_delinq", "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim","LC_invest")])

library(randomForest)
# Initial RF to select variables with higher impact and remove potential variables that have collinearity
rf.train=randomForest(loan_default~.-loan_status2-profit-zip_code-addr_state-issue_d2-train_test,data=train,ntree=25)
importance(rf.train)
plot(rf.train,main="Decison Tree Model")
varImpPlot(rf.train, main="Initial Model for Variable Selection")

# 2nd round RF variable selection
rf.train=randomForest(loan_default~new_dti+is_emp_length_na+grade+revol_util+mnth_sinc_earliest_cr_line+revol_bal+annual_inc
                      +tot_cur_bal+installment+total_rev_hi_lim+total_acc+funded_amnt_inv+LC_invest+verification_status
                      +home_ownership2+is_small_business+term,data=train,ntree=40)
importance(rf.train)
plot(rf.train,main="Decison Tree Model")
varImpPlot(rf.train)

# Supplimental EDA to determine the final variable selections
par(mfrow=c(2,2))
boxplot(train$new_dti~train$loan_default,main="new_dti")
boxplot(train$revol_util~train$loan_default,main="revol_util")
boxplot(train$mnth_sinc_earliest_cr_line~train$loan_default,main="mnth_since_earliest_cr_line")
boxplot(train$revol_bal~train$loan_default,main="revol_bal")

table_a=table(cut(train$mnth_sinc_earliest_cr_line,quantile(train$mnth_sinc_earliest_cr_line,probs=seq(0,1,0.1))),train$loan_default)
prop.table(table_a,1)

table_b=table(cut(train$revol_bal,quantile(train$revol_bal,probs=seq(0,1,0.1))),train$loan_default)
prop.table(table_b,1)

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(train, row.vars = "emp_length", col.vars = "loan_default", type = c("f", "row.pct"), addmargins = FALSE)

par(mfrow=c(3,2))
boxplot(train$installment~train$loan_default,main="installment")
boxplot(train$annual_inc~train$loan_default,main="annual_inc")
boxplot(exp(train$annual_inc)~train$grade,main="annual_inc by grade")
boxplot(train$tot_cur_bal~train$loan_default,main="tot_cur_bal")
plot(train$total_acc~train$loan_default,main="total_acc")
boxplot(train$total_rev_hi_lim~train$loan_default,main="total_rev_hi_lim")
boxplot(train$funded_amnt_inv~train$loan_default,main="funded_amnt_inv")
par(mfrow=c(1,1))

crosstab(train, row.vars = "grade", col.vars = "loan_default", type = c("f", "row.pct"), addmargins = FALSE)
crosstab(train, row.vars = "sub_grade", col.vars = "loan_default", type = c("f", "row.pct"), addmargins = FALSE)

crosstab(train, row.vars = "purpose", col.vars = "loan_default", type = c("f", "row.pct"), addmargins = FALSE)
crosstab(train, row.vars = "purpose", col.vars = "grade", type = "row.pct", addmargins = FALSE)

crosstab(train, row.vars = "verification_status", col.vars = "loan_default", type = c("f", "row.pct"), addmargins = FALSE)
crosstab(train, row.vars = "home_ownership2", col.vars = "loan_default", type = c("f", "row.pct"), addmargins = FALSE)
crosstab(train, row.vars = "term", col.vars = "loan_default", type = c("f", "row.pct"), addmargins = FALSE)

#why shorter term loans have lower default
crosstab(train, row.vars = "grade", col.vars = "term", type = c("f", "row.pct"), addmargins = FALSE)

crosstab(train, row.vars = "LC_invest", col.vars = "loan_default", type = c("f", "row.pct"), addmargins = FALSE)

# Why verification_status results counter-intuitive?
crosstab(train, row.vars = "grade", col.vars = "verification_status", type = c("f", "row.pct"), addmargins = FALSE)

# drop verification_status (makes no sense), swap out funded_amnt_inv with loan_amnt (may not be available at the time of application)
# rev_bal dropped due to high correlation with total_rev_hi_lim, total_acc dropped due to high correlation with mnth_since_earliest_cr_line

corrplot(cor(data4[,c("new_dti","revol_util","mnth_sinc_earliest_cr_line","annual_inc","tot_cur_bal","total_rev_hi_lim", 
                      "loan_amnt")]))

rf.train2=randomForest(loan_default~new_dti+is_emp_length_na+grade+revol_util+mnth_sinc_earliest_cr_line+annual_inc
                       +tot_cur_bal+total_rev_hi_lim+loan_amnt+total_acc+home_ownership2+is_small_business+term,data=train,ntree=25)
importance(rf.train2)
plot(rf.train2,main="Decison Tree Model")
varImpPlot(rf.train2, main = "RF - 2nd Variable Selection")

# Last round RF variable selection - dropping all variables that has lower than 2000 mean decrease Gini values
rf.train3=randomForest(loan_default~new_dti+mnth_sinc_earliest_cr_line+annual_inc+tot_cur_bal+loan_amnt+grade+revol_util, data=train,ntree=25)
importance(rf.train3)
plot(rf.train3,main="Decison Tree Model")
varImpPlot(rf.train3, main = "RF - Final Variable Selection")

names(train)


train2 = train[,c(8,9,6,18,1,21,12,28)]
test2 = test[,c(8,9,6,18,1,21,12,28)]

# CREATING A SUMMARY TABLE #
results = data.frame(x=seq(0.01,1,0.01))
#results2 = results
colnames(results) = 'Threshold'


library(MASS)
library(caret)
str(train2$loan_default)
model.log1 = glm(loan_default~.,train2, family=binomial("logit"))
#model.log1 = glm(loan_default~.-mnth_sinc_earliest_cr_line-loan_amnt,train2, family=binomial("logit"))
summary(model.log1)
pred.log1 = predict(model.log1, test, type="response")

pred.log1.df = data.frame(pred.log1,as.factor(ifelse(test2$loan_default=="YES",1,0)))
pred.log1.df = cbind(pred.log1.df,test[,c("profit","original_funded_amnt")])
colnames(pred.log1.df) = c("Prob","Actual","profit","funded_amnt")
#head(pred.log1.df)

profit_df = data.frame("Threshold"= seq(0.2,0.40,.0001) ,"Profit" = NA)
for(i in profit_df$Threshold){
  profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(pred.log1.df$Prob > i,0,1)*(pred.log1.df$profit+(pred.log1.df$funded_amnt*.03)))
}
profit_df[which.max(profit_df$Profit),]

confusionMatrix(as.factor(ifelse(pred.log1>=which.max(profit_df$Profit)/100,1,0)),as.factor(ifelse(test2$loan_default=="YES",1,0)),mode="everything",positive = "1")
confusionMatrix(as.factor(ifelse(pred.log1>=0.25,"YES","NO")),test2$loan_default,mode="everything", positive = "YES")
confusionMatrix(as.factor(ifelse(pred.log1>=0.26,"YES","NO")),test2$loan_default,mode="everything", positive = "YES") 
confusionMatrix(as.factor(ifelse(pred.log1>=0.27,"YES","NO")),test2$loan_default,mode="everything", positive = "YES") 
confusionMatrix(as.factor(ifelse(pred.log1>=0.28,"YES","NO")),test2$loan_default,mode="everything", positive = "YES")
confusionMatrix(as.factor(ifelse(pred.log1>=0.29,"YES","NO")),test2$loan_default,mode="everything", positive = "YES")
confusionMatrix(as.factor(ifelse(pred.log1>=0.30,"YES","NO")),test2$loan_default,mode="everything", positive = "YES")
confusionMatrix(as.factor(ifelse(pred.log1>=0.31,"YES","NO")),test2$loan_default,mode="everything", positive = "YES") 
confusionMatrix(as.factor(ifelse(pred.log1>=0.32,"YES","NO")),test2$loan_default,mode="everything", positive = "YES") 
confusionMatrix(as.factor(ifelse(pred.log1>=0.33,"YES","NO")),test2$loan_default,mode="everything", positive = "YES") 
confusionMatrix(as.factor(ifelse(pred.log1>=0.34,"YES","NO")),test2$loan_default,mode="everything", positive = "YES")
confusionMatrix(as.factor(ifelse(pred.log1>=0.35,"YES","NO")),test2$loan_default,mode="everything", positive = "YES")
confusionMatrix(as.factor(ifelse(pred.log1>=0.36,"YES","NO")),test2$loan_default,mode="everything", positive = "YES")
confusionMatrix(as.factor(ifelse(pred.log1>=0.37,"YES","NO")),test2$loan_default,mode="everything", positive = "YES") 
confusionMatrix(as.factor(ifelse(pred.log1>=0.38,"YES","NO")),test2$loan_default,mode="everything", positive = "YES")
confusionMatrix(as.factor(ifelse(pred.log1>=0.39,"YES","NO")),test2$loan_default,mode="everything", positive = "YES")
confusionMatrix(as.factor(ifelse(pred.log1>=0.40,"YES","NO")),test2$loan_default,mode="everything", positive = "YES") 

pred.log1 = predict(model.log1, test, type="response")
library(ROCR)
pred.log2.df = prediction(pred.log1,test$loan_default)


plot(unlist(performance(pred.log2.df, "f")@x.values), unlist(performance(pred.log2.df, "f")@y.values), 
     type="l", lwd=1, ylab="F1/Precision/Recall", xlab="Cutoff",xlim=c(0,1),ylim=c(0,1), main="Logistic Regression")
par(new=TRUE)
plot(unlist(performance(pred.log2.df, "acc")@x.values), unlist(performance(pred.log2.df, "acc")@y.values), 
     type="l", lwd=1, col='orange', ylab="", xlab="",xlim=c(0,1),ylim=c(0,1),xaxt='n')
par(new=TRUE)
plot(unlist(performance(pred.log2.df, "prbe")@x.values), unlist(performance(pred.log2.df, "prbe")@y.values), 
     type="p", lwd=3, col='green', ylab="", xlab="",xlim=c(0,1),ylim=c(0,1),xaxt='n')
par(new=TRUE)
plot(unlist(performance(pred.log2.df, "prec")@x.values), unlist(performance(pred.log2.df, "prec")@y.values), 
     type="l", lwd=1, col='pink', ylab="", xlab="",xlim=c(0,1),ylim=c(0,1),xaxt='n')
par(new=TRUE)
plot(unlist(performance(pred.log2.df, "rec")@x.values), unlist(performance(pred.log2.df, "rec")@y.values), 
     type="l", lwd=1, col='purple', ylab="", xlab="",xlim=c(0,1),ylim=c(0,1),xaxt='n')
par(new=TRUE)
with(results,plot(results$logitstic,lwd=1,col="darkblue",type="l",xlim=c(0,100), ylim=c(0,60000000), xlab = "",ylab="",xaxt='n',yaxt='n'))
axis(side=4)
mtext(side=4,line=3,'EXPECTED PROFIT')
lines(results$logistic,lwd=2,col="blue",xlab="", ylab="",xaxt='n',yaxt='n')
abline(v=29.7,col="red",lty=2)
legend(62,20000000,legend=c("F1","Accuracy","Precision_Recall_Balance_Points","Precision","Recall","Expected Profit","Highest Expected Profit (Reference)"),
       lty=1:1,lwd=3:3,col = c("black","orange","green","pink","purple","darkblue","red"),cex = 0.8)
profit_df[which.max(profit_df$Profit),]

profit_df = data.frame("Threshold"= seq(0.01,1,.01) ,"Profit" = NA)
for(i in profit_df$Threshold){
  profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(pred.log1.df$Prob > i,0,1)*(pred.log1.df$profit+(pred.log1.df$funded_amnt*.03)))
}

profit_df[which.max(profit_df$Profit),] #0.30, EXPECTED PROFOT = 53,161,285
confusionMatrix(as.factor(ifelse(pred.log1>=0.30,"YES","NO")),test2$loan_default,mode="everything", positive = "YES")
results = cbind(results, profit_df[2])
colnames(results)[2] = 'logistic'

# ############################
# # RIDGE REGRESSION / LASSO #
# ############################
# exp(cv.out_ridge$lambda.min) # 1.007065
# exp(cv.out_lasso$lambda.min) # 1.000087
# 
# library(caret)
# fitControl <- trainControl(method = "cv",number = 10, returnResamp = "final")
# glmGrid <- expand.grid(alpha = c(0,1), lambda = seq(0.02,0.04,0.002))
# 
# glmFit <- train(loan_default~.,data = train2, method = "glmnet", trControl = fitControl,
#                 tuneGrid = glmGrid, metric = "Accuracy") 
# glmFit
# plot(glmFit)
# 
# glm.pred = predict(glmFit, newdata = test, type = "prob")
# 
# glm.df = data.frame(glm.pred$YES,as.factor(ifelse(test$loan_default=="YES",1,0)))
# glm.df = cbind(glm.df,test[,c("profit","original_funded_amnt")])
# colnames(glm.df) = c("Prob","Actual","profit","funded_amnt")
# 
# profit_df = data.frame("Threshold"= seq(.01,1,.01) ,"Profit" = NA)
# for(i in profit_df$Threshold){
#   profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(glm.df$Prob > i,0,1)*(glm.df$profit+(glm.df$funded_amnt*.03)))
# }
# 
# profit_df[which.max(profit_df$Profit),]
# confusionMatrix(as.factor(ifelse(glm.df$Prob>=profit_df[which.max(profit_df$Profit),]$Threshold,1,0)),glm.df$Actual)
# 
# #########################################################
# # RIDGE REGRESSION / LASSO 2 - USING ACCURACY AS METRIC #
# #########################################################
# 
# library(caret)
# fitControl <- trainControl(summaryFunction = f1,method = "cv",number = 10, returnResamp = "final")
# glmGrid <- expand.grid(alpha = c(0,1), lambda = seq(0.02,0.04,0.002))
# 
# glmFit <- train(loan_default~.,data = train2, method = "glmnet", trControl = fitControl,
#                 tuneGrid = glmGrid, metric = "Accuracy")
# glmFit
# plot(glmFit)
# 
# glm.pred = predict(glmFit, newdata = test, type = "prob")
# 
# glm.df = data.frame(glm.pred$YES,as.factor(ifelse(test$loan_default=="YES",1,0)))
# glm.df = cbind(glm.df,test[,c("profit","original_funded_amnt")])
# colnames(glm.df) = c("Prob","Actual","profit","funded_amnt")
# #head(pred.log1.df)
# 
# profit_df = data.frame("Threshold"= seq(.01,1,.01) ,"Profit" = NA)
# for(i in profit_df$Threshold){
#   profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(glm.df$Prob > i,0,1)*(glm.df$profit+(glm.df$funded_amnt*.03)))
# }
# 
# profit_df[which.max(profit_df$Profit),]
# confusionMatrix(as.factor(ifelse(glm.df$Prob>=profit_df[which.max(profit_df$Profit),]$Threshold,1,0)),glm.df$Actual)
# 
# ##########################################################
# # RIDGE REGRESSION / LASSO 3 - USING PRECISION AS METRIC #
# ##########################################################
# 
# library(MLmetrics)
# precision <- function(data, lev = NULL, model = NULL) {
#   print(data)
#   precision_val <- Precision(y_pred = data$pred, y_true = data$obs)
#   c(precision = precision_val)
# }
# 
# library(caret)
# fitControl <- trainControl(summaryFunction = f1,method = "cv",number = 10, returnResamp = "final")
# glmGrid <- expand.grid(alpha = c(0,1), lambda = seq(0.02,0.04,0.002))
# 
# glmFit <- train(loan_default~.,data = train2, method = "glmnet", trControl = fitControl,
#                 tuneGrid = glmGrid, metric = "precision") 
# glmFit
# plot(glmFit)
# 
# glm.pred = predict(glmFit, newdata = test, type = "prob")
# 
# glm.df = data.frame(glm.pred$YES,as.factor(ifelse(test$loan_default=="YES",1,0)))
# glm.df = cbind(glm.df,test[,c("profit","original_funded_amnt")])
# colnames(glm.df) = c("Prob","Actual","profit","funded_amnt")
# #head(pred.log1.df)
# 
# profit_df = data.frame("Threshold"= seq(.01,1,.01) ,"Profit" = NA)
# for(i in profit_df$Threshold){
#   profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(glm.df$Prob > i,0,1)*(glm.df$profit+(glm.df$funded_amnt*.03)))
# }
# 
# profit_df[which.max(profit_df$Profit),]
# confusionMatrix(as.factor(ifelse(glm.df$Prob>=profit_df[which.max(profit_df$Profit),]$Threshold,1,0)),glm.df$Actual)
# 

####################
# RIDGE REGRESSION #
####################
library(MLmetrics)
f1 <- function(data, lev = NULL, model = NULL) {
  print(data)
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs)
  c(F1 = f1_val)
}

fitControl <- trainControl(method = "cv",number = 10, returnResamp = "final")
glmGrid <- expand.grid(alpha = 0, lambda = seq(0.02,0.04,0.002))

glmFit <- train(loan_default~., data = train2, method = "glmnet", trControl = fitControl,
                tuneGrid = glmGrid, metric = "F1")
names(glmFit)
coef(glmFit$finalModel, glmFit$bestTune$lambda)

plot(glmFit, main="Ridge Regression - Cost vs Accuracy")

glm.pred = predict(glmFit, newdata = test, type = "prob")

glm.df = data.frame(glm.pred$YES,as.factor(ifelse(test$loan_default=="YES",1,0)))
glm.df = cbind(glm.df,test[,c("profit","original_funded_amnt")])
colnames(glm.df) = c("Prob","Actual","profit","funded_amnt")
#head(pred.log1.df)

profit_df = data.frame("Threshold"= seq(.01,1,.01) ,"Profit" = NA)
for(i in profit_df$Threshold){
  profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(glm.df$Prob > i,0,1)*(glm.df$profit+(glm.df$funded_amnt*.03)))
}

profit_df[which.max(profit_df$Profit),]
confusionMatrix(as.factor(ifelse(glm.df$Prob>=profit_df[which.max(profit_df$Profit),]$Threshold,1,0)),glm2.df$Actual,mode="everything",positive = "1")

results = cbind(results, profit_df[2])
colnames(results)[3] = 'ridge'

#########
# LASSO #
#########

fitControl <- trainControl(method = "cv",number = 10, returnResamp = "final")
glmGrid <- expand.grid(alpha = 1, lambda = seq(0.02,0.04,0.002))

glmFit2 <- train(loan_default~., data = train2, method = "glmnet", trControl = fitControl,
                tuneGrid = glmGrid, metric = "F1")
glmFit2
plot(glmFit2)
coef(glmFit2$finalModel, glmFit2$bestTune$lambda)
glm2.pred = predict(glmFit2, newdata = test, type = "prob")

glm2.df = data.frame(glm2.pred$YES,as.factor(ifelse(test$loan_default=="YES",1,0)))
glm2.df = cbind(glm2.df,test[,c("profit","original_funded_amnt")])
colnames(glm2.df) = c("Prob","Actual","profit","funded_amnt")
#head(pred.log1.df)

profit_df = data.frame("Threshold"= seq(.01,1,.01) ,"Profit" = NA)
for(i in profit_df$Threshold){
  profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(glm2.df$Prob > i,0,1)*(glm2.df$profit+(glm2.df$funded_amnt*.03)))
}

profit_df[which.max(profit_df$Profit),]
confusionMatrix(as.factor(ifelse(glm2.df$Prob>=profit_df[which.max(profit_df$Profit),]$Threshold,1,0)),glm2.df$Actual,mode="everything",positive = "1")

results = cbind(results, profit_df[2])
colnames(results)[4] = 'lasso'

##################
# NEURAL NETWORK #
##################
names(getModelInfo())
modelLookup(model="nnet")
names(train2) #7 = loan_default
set.seed(1)
fitControl = trainControl(method = "cv",number = 3, returnResamp = "final")
nngrid = expand.grid(size=c(1,3,5,7,9,11),decay=2)
NNFit = train(train2[,1:7],train2[,8], method = "nnet", 
              trControl = fitControl, tuneGrid = nngrid, metric = "F1") 
NNFit
plot(NNFit)

#summary(model.log1) #There are 11 coefficients
library(nnet)
model.nn = nnet(loan_default~.,size = 11, data = train2)

pred.nn1 = predict(model.nn, test, type="raw")
head(pred.nn1,100)
pred.nn.df = data.frame(pred.nn1[,1],as.factor(ifelse(test$loan_default=="YES",1,0)))
pred.nn.df = cbind(pred.nn.df,test[,c("profit","original_funded_amnt")])
colnames(pred.nn.df) = c("Prob","Actual","profit","funded_amnt")

profit_df = data.frame("Threshold"= seq(.01,1,.01) ,"Profit" = NA)
for(i in profit_df$Threshold){
  profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(pred.nn.df$Prob > i,0,1)*(pred.nn.df$profit+(pred.nn.df$funded_amnt*.03)))
}

profit_df[which.max(profit_df$Profit),] # threshold 0.29, Expected Profit = 53,016,040
#results=results[,1:4]
results = cbind(results, profit_df[2])
colnames(results)[5] = 'nueralnet'

confusionMatrix(as.factor(ifelse(pred.nn.df[,1]>=0.29,1,0)),as.factor(ifelse(test$loan_default=="YES",1,0)),mode = "everything",positive = "1")

#################
# Random Forest #
#################

library(randomForest)
rf = randomForest(loan_default ~ ., data = train2, ntree = 500, importance = T)
pred.rf = predict(rf, test2,type = "prob")

table(pred/rf,test2[,"loan_default"])

importance(rf)
varImpPlot(rf)

head(pred.rf,100)
pred.rf.df = data.frame(pred.rf[,2],as.factor(ifelse(test$loan_default=="YES",1,0)))
pred.rf.df = cbind(pred.rf.df,test[,c("profit","original_funded_amnt")])
colnames(pred.rf.df) = c("Prob","Actual","profit","funded_amnt")

profit_df = data.frame("Threshold"= seq(.01,1,.01) ,"Profit" = NA)
for(i in profit_df$Threshold){
  profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(pred.rf.df$Prob > i,0,1)*(pred.rf.df$profit+(pred.rf.df$funded_amnt*.03)))
}
profit_df[which.max(profit_df$Profit),] #Threshold 0.32, Maximized Profit 52,307,003

results = cbind(results, profit_df[2])
colnames(results)[6] = 'rf'

confusionMatrix(as.factor(ifelse(pred.rf.df[,1]>=0.32,1,0)),as.factor(ifelse(test$loan_default=="YES",1,0)),mode = "everything",positive = "1")

##################
# Gradient Boost #
##################
# modelLookup(model="gbm")
# gbmGrid <- expand.grid(interaction.depth = c(1, 5, 9), n.trees = 500, 
#                        shrinkage = c(0.01,0.05,0.1),n.minobsinnode = 20)
# fitControl = trainControl(method = "cv",number = 3, returnResamp = "final")
# gbmFit <- train(loan_default~., data = train2,method = "gbm", trControl = fitControl, 
#                 verbose = FALSE, tuneGrid = gbmGrid, metric = "Accuracy")
# print(gbmfit)
# plot(gbmfit)
# plot(varImp(gbmfit))

library(gbm)
set.seed(1)
boostFit = gbm(ifelse(loan_default=="YES",1,0)~.,data=train2,distribution = "bernoulli",n.tree=500, interaction.depth = 5)
summary(boostFit)

plot(boostFit,i="grade")
plot(boostFit,i="new_dti")
plot(boostFit,i="annual_inc")
plot(boostFit,i="tot_cur_bal")
plot(boostFit,i="mnth_sinc_earliest_cr_line")
plot(boostFit,i="revol_util")
plot(boostFit,i="loan_amnt")

pred.gbm1 = predict(boostFit, test2, n.trees=500, type = "response")
head(pred.gbm1,100)
pred.gbm1.df = data.frame(pred.gbm1,as.factor(ifelse(test$loan_default=="YES",1,0)))
pred.gbm1.df = cbind(pred.gbm1.df,test[,c("profit","original_funded_amnt")])
colnames(pred.gbm1.df) = c("Prob","Actual","profit","funded_amnt")
head(pred.gbm1.df)

profit_df = data.frame("Threshold"= seq(.01,1,.01) ,"Profit" = NA)
for(i in profit_df$Threshold){
  profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(pred.gbm1.df$Prob > i,0,1)*(pred.gbm1.df$profit+(pred.gbm1.df$funded_amnt*.03)))
}

profit_df[which.max(profit_df$Profit),] # Threshold = 0.31, Profit 53,686,874
results = cbind(results, profit_df[2])
colnames(results)[7] = 'gbm'

confusionMatrix(as.factor(ifelse(pred.gbm1.df[,1]>=0.31,1,0)),as.factor(ifelse(test$loan_default=="YES",1,0)),mode = "everything",positive = "1")

#############
# LDA Model #
#############
library(MASS)
lda.fit = lda(loan_default~., data = train2)
pred.lda = predict(lda.fit, test)
names(pred.lda)
pred.lda$posterior[,2] # Default = Y

pred.lda1.df = data.frame(pred.lda$posterior[,2],as.factor(ifelse(test$loan_default=="YES",1,0)))
pred.lda1.df = cbind(pred.lda1.df,test[,c("profit","original_funded_amnt")])
colnames(pred.lda1.df) = c("Prob","Actual","profit","funded_amnt")
head(pred.lda1.df)
profit_df = data.frame("Threshold"= seq(.01,1,.01) ,"Profit" = NA)
for(i in profit_df$Threshold){
  profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(pred.lda1.df$Prob > i,0,1)*(pred.lda1.df$profit+(pred.lda1.df$funded_amnt*.03)))
}

profit_df[which.max(profit_df$Profit),] #Threshold = 0.32, Profit = 54,403,153

results = cbind(results, profit_df[2])
colnames(results)[8] = 'lda'

confusionMatrix(as.factor(ifelse(pred.lda1.df[,1]>=0.31,1,0)),as.factor(ifelse(test$loan_default=="YES",1,0)),mode = "everything",positive = "1")

#############
# QDA Model #
#############
qda.fit = qda(loan_default~., data = train2)
pred.qda = predict(qda.fit, test)
names(pred.qda)
pred.qda$posterior[,2] # Default = Y

pred.qda1.df = data.frame(pred.qda$posterior[,2],as.factor(ifelse(test$loan_default=="YES",1,0)))
pred.qda1.df = cbind(pred.qda1.df,test[,c("profit","original_funded_amnt")])
colnames(pred.qda1.df) = c("Prob","Actual","profit","funded_amnt")
head(pred.qda1.df)
profit_df = data.frame("Threshold"= seq(.01,1,.01) ,"Profit" = NA)
for(i in profit_df$Threshold){
  profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(pred.qda1.df$Prob > i,0,1)*(pred.qda1.df$profit+(pred.qda1.df$funded_amnt*.03)))
}

profit_df[which.max(profit_df$Profit),] #Threshold = 0.25, Profit = 51,624,287

results = cbind(results, profit_df[2])
colnames(results)[9] = 'qda'
confusionMatrix(as.factor(ifelse(pred.qda1.df[,1]>=0.28,1,0)),as.factor(ifelse(test$loan_default=="YES",1,0)),mode = "everything",positive = "1")

##############################################
# PARTIAL LEAST SQUARE DISCRIMINANT ANALYSIS #
##############################################

set.seed(1)
fitControl = trainControl(method = "cv",number = 5, returnResamp = "final")
plsfit = train(loan_default~., data = train2, method = "pls", trControl = fitControl,
               tuneLength = 10,preProc = c("center", "scale"),metric = "F1")
plsfit
plot(plsfit)

pls.pred = predict(plsfit, newdata = test, type = "prob")

pls.df = data.frame(pls.pred$YES,as.factor(ifelse(test$loan_default=="YES",1,0)))
pls.df = cbind(pls.df,test[,c("profit","original_funded_amnt")])
colnames(pls.df) = c("Prob","Actual","profit","funded_amnt")
#head(pred.log1.df)

profit_df = data.frame("Threshold"= seq(.01,1,.01) ,"Profit" = NA)
for(i in profit_df$Threshold){
  profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(pls.df$Prob > i,0,1)*(pls.df$profit+(pls.df$funded_amnt*.03)))
}

profit_df[which.max(profit_df$Profit),] #0.39 PROFIT 51,436,002
confusionMatrix(as.factor(ifelse(pls.df$Prob>=0.39,1,0)),pls.df$Actual,mode = "everything", positive = "1")

results = cbind(results, profit_df[2])
colnames(results)[10] = 'pls-da'

summary(results)


##########################
# SUPPORT VECTOR MACHINE #
##########################
library(caret)
library(e1071)
modelLookup(model="svmLinear") #Support Vector Machine Classifier

fitControl = trainControl(method = "cv",number = 3, returnResamp = "final")
svmGrid <- expand.grid(C = seq(1,3,1))
set.seed(1)
svmFit <- train(loan_default~., data = train2, method = "svmLinear", trControl = fitControl,
                preProc = c("center", "scale"),tuneGrid = svmGrid, metric = "F1")
svmFit   
plot(svmFit)

svm.pred = predict(svmFit, newdata = test, type="prob") #, type = "prob"

pred.svm1.df = data.frame(svm.pred$posterior[,2],as.factor(ifelse(test$loan_default=="YES",1,0)))
pred.svm1.df = cbind(pred.svm1.df,test[,c("profit","original_funded_amnt")])
colnames(pred.svm1.df) = c("Prob","Actual","profit","funded_amnt")
head(pred.svm1.df)
profit_df = data.frame("Threshold"= seq(.01,1,.01) ,"Profit" = NA)
for(i in profit_df$Threshold){
  profit_df[profit_df$Threshold==i,"Profit"] = sum(ifelse(pred.svm1.df$Prob > i,0,1)*(pred.svm1.df$profit+(pred.svm1.df$funded_amnt*.03)))
}
profit_df[which.max(profit_df$Profit),] #Threshold = 0.25, Profit = 51,624,287

results = cbind(results, profit_df[2])
colnames(results)[11] = 'svm'

summary(results)

#############
# knn Model #
#############

# knn() does require continuous variables as it calculates the distances.
# dummies package converts the categorical variables into dummy variables (0,1).

library(dummies)
names(train2)
train2_x= dummy.data.frame(train2[,1:6],all = T)
train2_y = train2[,7] # loan_status2Default only

test2_x= dummy.data.frame(test2[,1:6],all = T)
test2_y = test2[,7] # loan_status2Default only


# KNN Model
library(class)
knn.error = data.frame("k" = NA, "test.error" = NA)
for (i in 4:5){
  knn.pred = knn(train2_x, test2_x, train2_y, k = i)
  knn.error[i,1] = i
  knn.error[i,2] = sum(knn.pred == test2_y)/length(test2_y)
}
knn.error


#################

plot(results$logitstic,lwd=1,col="blue",type="l",xlim=c(0,100), ylim=c(0,60000000),ylab="Expected Profit",xlab="Threshold",main="All Classification Models" )
lines(results$logistic,lwd=1,col="blue")
lines(results$ridge,lwd=1,col="red")
lines(results$lasso,lwd=1,col="darkred")
lines(results$nueralnet,lwd=1,col="orange")
lines(results$rf,lwd=1,col="darkgreen")
lines(results$gbm,lwd=1,col="black")
lines(results$lda,lwd=1,col="magenta")
lines(results$qda,lwd=1,col="purple")
lines(results$"pls-da",lwd=1,col="gray")
abline(h=43904998,lwd=2,col="cyan",lty=2)
text(6,42500000,"Baseline - Today")
legend(60,40000000,legend=c("Logistic","RIDGE","LASSO","NEURAL NET","Random Forest","Gradient Boost","LDA","QDA","PLS-DA"),
       lty=1:1,lwd=3:3,col = c("blue","red","darkred","orange","darkgreen","black","magenta","purple","gray"),cex = 1.0)

#zoomed-in
plot(results$logitstic,lwd=1,col="blue",type="l",xlim=c(0,60), ylim=c(40000000,55000000),ylab="Expected Profit",xlab="Threshold",main="All Classification Models" )
lines(results$logistic,lwd=1,col="blue")
lines(results$ridge,lwd=1,col="red")
lines(results$lasso,lwd=1,col="darkred")
lines(results$nueralnet,lwd=1,col="orange")
lines(results$rf,lwd=1,col="darkgreen")
lines(results$gbm,lwd=1,col="black")
lines(results$lda,lwd=1,col="magenta")
lines(results$qda,lwd=1,col="purple")
lines(results$"pls-da",lwd=1,col="gray")
abline(h=43904998,lwd=2,col="cyan",lty=2)
text(3.2,43500000,"Baseline - Today")
legend(1,55000000,legend=c("Logistic","RIDGE","LASSO","NEURAL NET","Random Forest","Gradient Boost","LDA","QDA","PLS-DA"),
       lty=1:1,lwd=3:3,col = c("blue","red","darkred","orange","darkgreen","black","magenta","purple","gray"),cex = 1.0)
#results[100,2:10] #baseline = 43904998
