
getwd()
setwd("C:/Users/nishidh/Documents/Personal/MSPA/MSDS 498 Capstone")
data=read.csv("loan.csv")

data2 = data[,c("loan_amnt", "funded_amnt", "funded_amnt_inv", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length",
                "home_ownership", "annual_inc", "verification_status", "issue_d", "loan_status", "purpose", "zip_code", "addr_state",
                "dti", "delinq_2yrs", "earliest_cr_line", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record",
                "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "mths_since_last_major_derog",
                "application_type", "acc_now_delinq", "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim")]

data2$loan_status2 = as.factor(ifelse(data2$loan_status=='Charged Off','Default',
                                      ifelse(data2$loan_status=='Current','Current',
                                             ifelse(data2$loan_status=='Default','Default',
                                                    ifelse(data2$loan_status=='Does not meet the credit policy. Status:Charged Off','Default',
                                                           ifelse(data2$loan_status=='Does not meet the credit policy. Status:Fully Paid','Fully Paid',
                                                                  ifelse(data2$loan_status=='Fully Paid','Fully Paid','Current')))))))

library(zoo)
data2$issue_d2 <- as.yearmon(as.character(paste(substr(as.character(data2$issue_d),5,8),substr(as.character(data2$issue_d),1,3),sep="-")), "%Y-%b")
data2$earliest_cr_line2 <- as.yearmon(as.character(paste(substr(as.character(data2$earliest_cr_line),5,8),substr(as.character(data2$earliest_cr_line),1,3),sep="-")), "%Y-%b")
data2$mnth_sinc_earliest_cr_line = (data2$issue_d2-data2$earliest_cr_line2)*12

data2$mths_since_last_delinq[is.na(data2$mths_since_last_delinq)] = 9999
data2$mths_since_last_record[is.na(data2$mths_since_last_record)] = 9999
data2$mths_since_last_major_derog[is.na(data2$mths_since_last_major_derog)] = 9999

summary(data2$home_ownership2)
data2$home_ownership2 = as.factor(ifelse(data2$home_ownership=='MORTGAGE','MORTGAGE',
                                         ifelse(data2$home_ownership=='OWN','OWN',
                                                ifelse(data2$home_ownership=='RENT','RENT','RENT'))))
plot(data2[which(data2$loan_status2!='Current'),]$home_ownership2,data2[which(data2$loan_status2!='Current'),]$loan_status2)

pMiss <- function(x){sum(is.na(x))/length(x)*100}
sort(format(apply(data2,2,pMiss),scientific=F),decreasing=T)

unique(data2$application_type)
data2=data2[which(data2$application_type=="INDIVIDUAL"),] #Remove 511 Joint Applications

which(colnames(data2)=="delinq_2yrs") # Remove those 29 applications that are commonly missing "delinq_2yrs", "inq_last_6mths", 
# "open_acc", "pub_rec", "total_acc", "acc_now_delinq", "earliest_cr_line2", "mnth_sinc_earliest_cr_line".
data2=data2[complete.cases(data2[,19]),]

sort(format(apply(data2,2,pMiss),scientific=F),decreasing=T)
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
                 "home_ownership2", "annual_inc", "verification_status", "issue_d2", "loan_status2", "purpose", "zip_code", "addr_state",
                 "dti", "new_dti", "delinq_2yrs", "mnth_sinc_earliest_cr_line", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record",
                 "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "mths_since_last_major_derog",
                 "acc_now_delinq", "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim","LC_invest")]

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
Winsorized_data3 <- apply(data3[,c("loan_amnt", "funded_amnt", "funded_amnt_inv","int_rate", "installment", "annual_inc","dti",
                           "new_dti", "mnth_sinc_earliest_cr_line", "open_acc", "revol_bal", "revol_util", "total_acc",
                           "mths_since_last_record","mths_since_last_delinq","mths_since_last_major_derog",
                           "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim")],2,Winsorize)
summary(Winsorized_data3)

data3.1 = cbind(Winsorized_data3,data3[,c("term", "grade", "sub_grade", "emp_length", "home_ownership2", "verification_status",
                                        "issue_d2", "loan_status2", "purpose", "zip_code", "addr_state", "delinq_2yrs",
                                        "inq_last_6mths", "pub_rec", "initial_list_status",
                                         "acc_now_delinq", "LC_invest")])
                                        
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

summary(data4)
str(data4$loan_status2)

set.seed(1)
data4$train_test <- runif(n=dim(data4)[1],min=0,max=1)

# Create train/test split;
train <- subset(data4, train_test<0.70);
test  <- subset(data4, train_test>=0.70);
names(train)

str(data4)

corrplot(cor(data4[,c("loan_amnt", "funded_amnt", "funded_amnt_inv","int_rate", "installment", "annual_inc","dti",
                      "new_dti", "mnth_sinc_earliest_cr_line", "revol_bal", "revol_util", "total_acc",
                      "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim")]))

#ggpairs(data4[,c("loan_amnt", "funded_amnt", "funded_amnt_inv", "term", "int_rate", "installment", "grade", "emp_length",
#                     "home_ownership2", "annual_inc", "verification_status", "loan_status2", "purpose",
#                     "dti", "new_dti", "delinq_2yrs", "mnth_sinc_earliest_cr_line", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record",
#                     "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", "mths_since_last_major_derog",
#                     "acc_now_delinq", "tot_coll_amt", "tot_cur_bal", "total_rev_hi_lim","LC_invest")])

library(randomForest)
#rf.train=randomForest(loan_status2~funded_amnt+term+grade+emp_length+home_ownership2+annual_inc+verification_status+purpose+dti+new_dti+
#                  mnth_sinc_earliest_cr_line+mths_since_last_delinq+initial_list_status+LC_invest,data=train)
rf.train=randomForest(loan_status2~.-zip_code-addr_state-issue_d2-train_test,data=train,ntree=25)
rf.train=randomForest(loan_status2~new_dti+emp_length+grade+revol_util+mnth_sinc_earliest_cr_line+revol_bal+annual_inc
                     +tot_cur_bal+installment+total_rev_hi_lim+total_acc+funded_amnt_inv+LC_invest+verification_status
                     +home_ownership2+purpose+term,data=train,ntree=40)
importance(rf.train)
plot(rf.train,main="Decison Tree Model")
varImpPlot(rf.train)

par(mfrow=c(2,2))
boxplot(train$new_dti~train$loan_status2,main="new_dti")
boxplot(train$revol_util~train$loan_status2,main="revol_util")
boxplot(train$mnth_sinc_earliest_cr_line~train$loan_status2,main="mnth_since_earliest_cr_line")
boxplot(train$revol_bal~train$loan_status2,main="revol_bal")

table_a=table(cut(train$mnth_sinc_earliest_cr_line,quantile(train$mnth_sinc_earliest_cr_line,probs=seq(0,1,0.1))),train$loan_status2)
prop.table(table_a,1)

table_b=table(cut(train$revol_bal,quantile(train$revol_bal,probs=seq(0,1,0.1))),train$loan_status2)
prop.table(table_b,1)

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(train, row.vars = "emp_length", col.vars = "loan_status2", type = c("f", "row.pct"), addmargins = FALSE)

par(mfrow=c(3,2))
boxplot(train$installment~train$loan_status2,main="installment")
boxplot(train$annual_inc~train$loan_status2,main="annual_inc")
boxplot(train$tot_cur_bal~train$loan_status2,main="tot_cur_bal")
boxplot(train$total_acc~train$loan_status2,main="total_acc")
boxplot(train$total_rev_hi_lim~train$loan_status2,main="total_rev_hi_lim")
boxplot(train$funded_amnt_inv~train$loan_status2,main="funded_amnt_inv")
par(mfrow=c(1,1))

crosstab(train, row.vars = "grade", col.vars = "loan_status2", type = c("f", "row.pct"), addmargins = FALSE)
crosstab(train, row.vars = "sub_grade", col.vars = "loan_status2", type = c("f", "row.pct"), addmargins = FALSE)

crosstab(train, row.vars = "purpose", col.vars = "loan_status2", type = c("f", "row.pct"), addmargins = FALSE)
crosstab(train, row.vars = "purpose", col.vars = "grade", type = "row.pct", addmargins = FALSE)

crosstab(train, row.vars = "verification_status", col.vars = "loan_status2", type = c("f", "row.pct"), addmargins = FALSE)
crosstab(train, row.vars = "home_ownership2", col.vars = "loan_status2", type = c("f", "row.pct"), addmargins = FALSE)
crosstab(train, row.vars = "term", col.vars = "loan_status2", type = c("f", "row.pct"), addmargins = FALSE)

#why shorter term loans have lower default
crosstab(train, row.vars = "grade", col.vars = "term", type = c("f", "row.pct"), addmargins = FALSE)

crosstab(train, row.vars = "LC_invest", col.vars = "loan_status2", type = c("f", "row.pct"), addmargins = FALSE)

# Why verification_status results counter-intuitive?
crosstab(train, row.vars = "grade", col.vars = "verification_status", type = c("f", "row.pct"), addmargins = FALSE)

# drop verification_status (makes no sense), swap out funded_amnt_inv with loan_amnt (may not be available at the time of application)
# rev_bal dropped due to high correlation with total_rev_hi_lim, total_acc dropped due to high correlation with mnth_since_earliest_cr_line

rf.train2=randomForest(loan_status2~new_dti+emp_length+grade+revol_util+mnth_sinc_earliest_cr_line+annual_inc
                      +tot_cur_bal+total_rev_hi_lim+loan_amnt+total_acc+home_ownership2+purpose+term,data=train,ntree=40)
importance(rf.train2)
plot(rf.train2,main="Decison Tree Model")
varImpPlot(rf.train2)


library(MASS)
str(train$loan_status2)
model.log1 = glm(loan_status2~new_dti+emp_length+grade+revol_util+mnth_sinc_earliest_cr_line+annual_inc
                 +tot_cur_bal+total_rev_hi_lim+loan_amnt+total_acc+home_ownership2+purpose+term,train, family=binomial("logit"))
summary(model.log1)
pred.log1 = predict(model.log1, test, type="response")

pred.log1.df = data.frame(pred.log1,as.factor(ifelse(test$loan_status2=="Fully Paid",1,0)))
colnames(pred.log1.df) = c("Prob","Actual")
summary
head(pred.log1.df)

pred.log1.df$co_0.70 = as.factor(ifelse(pred.log1.df$Prob>=0.70,1,0))
pred.log1.df$co_0.71 = as.factor(ifelse(pred.log1.df$Prob>=0.71,1,0))
pred.log1.df$co_0.72 = as.factor(ifelse(pred.log1.df$Prob>=0.72,1,0))
pred.log1.df$co_0.73 = as.factor(ifelse(pred.log1.df$Prob>=0.73,1,0))
pred.log1.df$co_0.74 = as.factor(ifelse(pred.log1.df$Prob>=0.74,1,0))
pred.log1.df$co_0.75 = as.factor(ifelse(pred.log1.df$Prob>=0.75,1,0))
pred.log1.df$co_0.76 = as.factor(ifelse(pred.log1.df$Prob>=0.76,1,0))
pred.log1.df$co_0.77 = as.factor(ifelse(pred.log1.df$Prob>=0.77,1,0))
pred.log1.df$co_0.78 = as.factor(ifelse(pred.log1.df$Prob>=0.78,1,0))
pred.log1.df$co_0.79 = as.factor(ifelse(pred.log1.df$Prob>=0.79,1,0))
pred.log1.df$co_0.80 = as.factor(ifelse(pred.log1.df$Prob>=0.80,1,0))

library(caret)
confusionMatrix(pred.log1.df$co_0.70,pred.log1.df$Actual)
confusionMatrix(pred.log1.df$co_0.71,pred.log1.df$Actual)
confusionMatrix(pred.log1.df$co_0.72,pred.log1.df$Actual)
confusionMatrix(pred.log1.df$co_0.73,pred.log1.df$Actual)
confusionMatrix(pred.log1.df$co_0.74,pred.log1.df$Actual)
confusionMatrix(pred.log1.df$co_0.75,pred.log1.df$Actual)
confusionMatrix(pred.log1.df$co_0.76,pred.log1.df$Actual)
confusionMatrix(pred.log1.df$co_0.77,pred.log1.df$Actual)
confusionMatrix(pred.log1.df$co_0.78,pred.log1.df$Actual)
confusionMatrix(pred.log1.df$co_0.79,pred.log1.df$Actual)
confusionMatrix(pred.log1.df$co_0.80,pred.log1.df$Actual)



library(ROCR)
pred.log1.df = prediction(pred.log1,as.factor(ifelse(test$loan_status2=="Fully Paid",1,0)))

plot(unlist(performance(pred.log1.df, "sens")@x.values), unlist(performance(pred.log1.df, "sens")@y.values), 
     type="l", lwd=2, ylab="Specificity", xlab="Cutoff",main="Logistic Regression")
par(new=TRUE)
plot(unlist(performance(pred.log1.df, "spec")@x.values), unlist(performance(pred.log1.df, "spec")@y.values), 
     type="l", lwd=2, col='red', ylab="", xlab="")
axis(4, at=seq(0,1,0.2))
mtext("Specificity",side=4, padj=-2, col='red')

