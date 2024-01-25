
library(pracma)
library(maxLik)
library(stargazer)
library(dplyr)
library(lmtest)
library(ggplot2)
payment_card=read.csv(paste0(path_data,"payment_card.csv"))


payment_card=payment_card%>%filter( Q2 %in%c(1,3,4) )

payment_card=payment_card%>%filter((Q2==1&bus_question==0) | bus==1)



n_pc=nrow(payment_card)
reg1_pc=cens_mode_est_pc(decision_all~additional.time +additional.time*bus +bus,~1,payment_card)
reg2_pc=cens_mode_est_pc(decision_all~ current_time+new_time+bus+current_time*bus+new_time*bus+log_income+married+num_kids+young_school_age+age,~1,payment_card)
reg3_pc=cens_mode_est_pc(decision_all~additional.time+additional.time*bus +bus+log_income+ married+num_kids+young_school_age+age,~1,payment_card)
reg4_pc=cens_mode_est_pc(decision_all~additional.time+additional.time*bus +bus+log_income+ married+num_kids+young_school_age+age+child_female+resp_female
                   ,~1,payment_card)
reg5_pc=cens_mode_est_pc(decision_all~additional.time+additional.time*bus +bus+resp_female*additional.time+child_female*additional.time+young_school_age*additional.time+
                     log_income+ married+num_kids+young_school_age+age+child_female+resp_female
                   ,~1,payment_card)

graph_cost(reg4_pc$estimate[2],-inv(reg4_pc$hessian)[2,2],reg4_pc$estimate[11],-inv(reg4_pc$hessian)[11,11],-inv(reg4_pc$hessian)[2,11],"graph_costs_all")


############################### DC

dichotomous=read.csv(paste0(path_data,"dichotomous.csv"))


dichotomous=dichotomous%>%filter( Q2 %in%c(1,3,4) )

dichotomous=dichotomous%>%filter((Q2==1&bus_question==0) | bus==1)






n_dc=nrow(dichotomous)
reg1_dc=cens_mode_est_dc(value_all~additional.time +additional.time*bus +bus,~1,dichotomous)
reg2_dc=cens_mode_est_dc(value_all~ current_time+new_time+bus+current_time*bus+new_time*bus+log_income+married+num_kids+young_school_age+age,~1,dichotomous)

reg3_dc=cens_mode_est_dc(value_all~additional.time+additional.time*bus +bus+log_income+ married+num_kids+young_school_age+age,~1,dichotomous)
reg4_dc=cens_mode_est_dc(value_all~additional.time+additional.time*bus +bus+log_income+ married+num_kids+young_school_age+age+child_female+resp_female
                         ,~1,dichotomous)
reg5_dc=cens_mode_est_dc(value_all~additional.time+additional.time*bus +bus+resp_female*additional.time+child_female*additional.time+young_school_age*additional.time+
                           log_income+ married+num_kids+young_school_age+age+child_female+resp_female
                         ,~1,dichotomous)



graph_cost(reg4_dc$estimate[2],-inv(reg4_dc$hessian)[2,2],reg4_dc$estimate[11],-inv(reg4_dc$hessian)[11,11],-inv(reg4_dc$hessian)[2,11],"graph_costs_dc_all")


stargazer(coeftest(reg1_pc),coeftest(reg2_pc),coeftest(reg3_pc),coeftest(reg4_pc),coeftest(reg5_pc),coeftest(reg1_dc),coeftest(reg2_dc),coeftest(reg3_dc),coeftest(reg4_dc),coeftest(reg5_dc),
          
          dep.var.caption = c("DV: Willingness to accept (1,000 JPY)"),
          dep.var.labels=c("Payment Card","Dichotomous Choice"),
          
          digits=2,
          column.labels = c("PC","PC","PC","PC","PC","DC","DC","DC","DC","DC"),
          
          order=      c("new_time","new_time:bus","current_time","current_time:bus","additional.time","additional.time:bus",
                        
                        "additional.time:resp_female:","additional.time:child_female","additional.time:young_school_age","bus","log_income","married","num_kids","young_school_age","age","resp_female","child_female","(Intercept) Probit","Std. Err."),
          omit.stat = c("chi2","ll"),
          
          covariate.labels=c("Walking time to nearby school","Walking time to nearby school*bus","Walking time to current school","Walking time to current school*bus","Additional walking time to nearby school","Additional walking time to nearby school*bus"
                             , "Additional time*female respondent","Additional time*female child","Additional time*age of child" ,"Bus","Log of income","Married",
                             "Number of children in elementary and junior high school",  "Grade of the youngest child", "Age of respondent","Gender of the respondent (1 is female)","Gender of the child (1 is female)"),
          
          
          
          
          title="Survey Double-hurdle Regression of Willingness to Accept for a School Merger for Elementary and Junior High  Schools Using Payment Card and Single-bounded Dichotomous Choice Answer",
          keep.stat="n",
          add.lines = list(c("Observations",rep(n_pc,5),rep(n_dc,5))),
          type="latex",out=paste0(path_results,"main_table.tex"))







