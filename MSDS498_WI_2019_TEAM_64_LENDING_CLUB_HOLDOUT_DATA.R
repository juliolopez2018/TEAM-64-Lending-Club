library(RODBC)
con_ref = odbcDriverConnect('driver={SQL Server};server=NADULXXX8RT7QF2\\NISHIDA;database=HIRO_MISC;trusted_connection=true')

#input data
holdout= data.frame(sqlQuery(con_ref, "select * from LoanStats_2016_2018"))

library(plyr)
match_df(holdout,data) #Only selecting the variables available in the training set

colnames(holdout)[61] = "open_il_6m" #Assuming "open_act_il" is the same as "open_il_6m".

holdout2 = holdout[,c("id", "member_id", "loan_amnt", "funded_amnt", "funded_amnt_inv", "term", "int_rate", 
           "installment", "grade", "sub_grade", "emp_title", "emp_length", "home_ownership", "annual_inc", 
           "verification_status", "issue_d", "loan_status", "pymnt_plan", "url", "desc", "purpose", "title", 
           "zip_code", "addr_state", "dti", "delinq_2yrs", "earliest_cr_line", "inq_last_6mths", "mths_since_last_delinq", 
           "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "initial_list_status", 
           "out_prncp", "out_prncp_inv", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", "total_rec_int", "total_rec_late_fee", 
           "recoveries", "collection_recovery_fee", "last_pymnt_d", "last_pymnt_amnt", "next_pymnt_d", "last_credit_pull_d", 
           "collections_12_mths_ex_med", "mths_since_last_major_derog", "policy_code", "application_type", "annual_inc_joint", 
           "dti_joint", "verification_status_joint", "acc_now_delinq", "tot_coll_amt", "tot_cur_bal", "open_il_6m", "open_acc_6m",
           "open_il_12m", "open_il_24m", "mths_since_rcnt_il", "total_bal_il", "il_util", "open_rv_12m", "open_rv_24m", "max_bal_bc",
           "all_util", "total_rev_hi_lim", "inq_fi", "total_cu_tl", "inq_last_12m")]


pMiss <- function(x){sum(is.na(x))/length(x)*100}
sort(format(apply(holdout2,2,pMiss),scientific=F),decreasing=T)


