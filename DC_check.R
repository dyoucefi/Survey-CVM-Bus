
library(pracma)
library(maxLik)
library(stargazer)
library(dplyr)
library(lmtest)
library(ggplot2)




###########We reproduce the dichotomous choice double hurdle regression using payment card data on elementary and junior high schools subset


pc_check=read.csv(paste0(path_data,"pc_check.csv"))
pc_check=pc_check%>%filter( Q2 %in%c(1,3,4) )
###keep only people that were either bussing or had no walking question
pc_check=pc_check%>%filter((Q2==1&bus_question==0) | bus==1)




n=nrow(pc_check)
reg1=cens_mode_est_dc(value_all~additional.time +additional.time*bus +bus,~1,pc_check)
reg2=cens_mode_est_dc(value_all~ current_time+new_time+bus+current_time*bus+new_time*bus+log_income+married+num_kids+young_school_age+age,~1,pc_check)
reg3=cens_mode_est_dc(value_all~additional.time+additional.time*bus +bus+log_income+ married+num_kids+young_school_age+age,~1,pc_check)
reg4=cens_mode_est_dc(value_all~additional.time+additional.time*bus +bus+log_income+ married+num_kids+young_school_age+age+child_female+resp_female
                      ,~1,pc_check)
reg5=cens_mode_est_dc(value_all~additional.time+additional.time*bus +bus+resp_female*additional.time+child_female*additional.time+young_school_age*additional.time+
                     log_income+ married+num_kids+young_school_age+age+child_female+resp_female
                   ,~1,pc_check)



######save to latex
stargazer(coeftest(reg1),coeftest(reg2),coeftest(reg3),coeftest(reg4),coeftest(reg5),
          
          dep.var.caption = c("DV: Willingness to accept (1,000 JPY)"),
          dep.var.labels=c("Elementary and Junior High Schools"),
          
          digits=2,
          column.labels = c("PC","PC","PC","PC","PC","PC","PC","PC","PC","PC"),
          
          order=      c("new_time","new_time:bus","current_time","current_time:bus","additional.time","additional.time:bus",
                        
                        "additional.time:resp_female:","additional.time:child_female","additional.time:young_school_age","bus","log_income","married","num_kids","young_school_age","age","resp_female","child_female","(Intercept) Probit","Std. Err."),
          omit.stat = c("chi2","ll"),
          
          covariate.labels=c("Walking time to nearby school","Walking time to nearby school*bus","Walking time to current school","Walking time to current school*bus","Additional walking time to nearby school","Additional walking time to nearby school*bus"
                             , "Additional time*female respondent","Additional time*female child","Additional time*age of child" ,"Bus","Log of income","Married",
                             "Number of children in elementary and junior high school",  "Grade of the youngest child", "Age of respondent","Gender of the respondent (1 is female)","Gender of the child (1 is female)"),
          
          
          
          title="Survey Double-hurdle Regression of Willingness to Accept for a School Merger for Elementary and Junior High Schools Using  Payment Card Answers for Bus-commuting Children Reproducing Dichotomous Answers",
          
          keep.stat="n",
          add.lines = list(c("Observations",rep(n,5))),
          type="latex",out=paste0(path_results,"dc_check_all.tex"))


