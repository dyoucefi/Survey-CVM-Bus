library(pracma)
library(maxLik)
library(dplyr)
library(stargazer)
library(stringr)
library(lmtest)
library(descr)
library(vtable)




##################

dichotomous=read.csv(paste0(path_data,"dichotomous.csv"))
dicho$type="2Dichotomous Choice"
dicho=dicho%>%filter( Q2 %in%c(1,3,4) )
dicho=dicho%>%filter((Q2==1&bus_question==0) | bus==1)



payment_card=read.csv(paste0(path_data,"payment_card.csv"))
payment_card$type="1Payment Card"
payment_card$value_all=NA
payment_card=payment_card%>%filter( Q2 %in%c(1,3,4) )
payment_card=payment_card%>%filter((Q2==1&bus_question==0) | bus==1)
payment_card$Q5_val=NA
payment_card$Q8_val=NA
payment_card$Q13_val=NA

merged.survey.j=rbind(dicho,payment_card)


var=c("current_time","new_time","additional.time","bus","log_income","num_kids","young_school_age","age","married","resp_female","child_female")
labs=c("Walking time to nearby school","Walking time to current school","Additional walking time to nearby school"
                     ,"Bus","Log of income","Married",
                     "Number of children in elementary and junior high school",  "Grade of the youngest child", "Age of respondent","Gender of the respondent (1 is female)","Gender of the child (1 is female)")




st(merged.survey.j,vars=var,summ=list(c('notNA(x)','min(x)','mean(x)','max(x)','sd(x)')),summ.names = list(
  c('N','Min','Mean','Max','Std. Dev.')),group="type",labels=labs,out="latex",file = paste0(path_results,"summary_stats.tex"))




print(table(payment_card$decision_all))



print(crosstab(dicho$value_all,dicho$decision_all))

















