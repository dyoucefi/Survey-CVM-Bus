library(pracma)
library(maxLik)
library(stargazer)
library(dplyr)
library(lmtest)
library(ggplot2)
elementary_survey=read.csv(paste0(path_data,"payment_card.csv"))
elementary_survey=elementary_survey%>%filter(UF1==1 & Q2 %in%c(1,3,4) )
elementary_survey=elementary_survey%>%filter((Q2==1&bus_question==0) | bus==1)




n_elem=nrow(elementary_survey)
reg1_elem=cens_mode_est_pc(decision_all~additional.time +additional.time*bus +bus,~1,elementary_survey)
reg2_elem=cens_mode_est_pc(decision_all~ current_time+new_time+bus+current_time*bus+new_time*bus+log_income+married+num_kids_elem+young_school_age+age,~1,elementary_survey)

reg3_elem=cens_mode_est_pc(decision_all~additional.time+additional.time*bus +bus+log_income+ married+num_kids_elem+young_school_age+age,~1,elementary_survey)
reg4_elem=cens_mode_est_pc(decision_all~additional.time+additional.time*bus +bus+log_income+ married+num_kids_elem+young_school_age+age+child_female+resp_female
                      ,~1,elementary_survey)
reg5_elem=cens_mode_est_pc(decision_all~additional.time+additional.time*bus +bus+resp_female*additional.time+child_female*additional.time+young_school_age*additional.time+
                     log_income+ married+num_kids_elem+young_school_age+age+child_female+resp_female
                   ,~1,elementary_survey)



graph_cost(reg4_elem$estimate[2],-inv(reg4_elem$hessian)[2,2],reg4_elem$estimate[11],-inv(reg4_elem$hessian)[11,11],-inv(reg4_elem$hessian)[2,11],"graph_costs_elem")


junior_survey=read.csv(paste0(path_data,"payment_card.csv"))
junior_survey=junior_survey%>%filter(UF1==2 & Q2 %in%c(1,3,4) )
junior_survey=junior_survey%>%filter((Q2==1&bus_question==0) | bus==1)







n_jun=nrow(junior_survey)
reg1_jun=cens_mode_est_pc(decision_all~additional.time +additional.time*bus +bus,~1,junior_survey)
reg2_jun=cens_mode_est_pc(decision_all~ current_time+new_time+bus+current_time*bus+new_time*bus+log_income+married+num_kids_jun+young_school_age+age,~1,junior_survey)

reg3_jun=cens_mode_est_pc(decision_all~additional.time+additional.time*bus +bus+log_income+ married+num_kids_jun+young_school_age+age,~1,junior_survey)
reg4_jun=cens_mode_est_pc(decision_all~additional.time+additional.time*bus +bus+log_income+ married+num_kids_jun+young_school_age+age+child_female+resp_female
                          ,~1,junior_survey)
reg5_jun=cens_mode_est_pc(decision_all~additional.time+additional.time*bus +bus+resp_female*additional.time+child_female*additional.time+young_school_age*additional.time+
                         log_income+ married+num_kids_jun+young_school_age+age+child_female+resp_female
                       ,~1,junior_survey)






graph_cost(reg4_jun$estimate[2],-inv(reg4_jun$hessian)[2,2],reg4_jun$estimate[11],-inv(reg4_jun$hessian)[11,11],-inv(reg4_jun$hessian)[2,11],"graph_costs_jun")






stargazer(coeftest(reg1_elem),coeftest(reg2_elem),coeftest(reg3_elem),coeftest(reg4_elem),coeftest(reg5_elem),coeftest(reg1_jun),coeftest(reg2_jun),coeftest(reg3_jun),coeftest(reg4_jun),coeftest(reg5_jun),
          
          dep.var.caption = c("DV: Willingness to accept (1,000 JPY)"),
          dep.var.labels=c("Elementary School","Junior High School"),
          
          digits=2,
          column.labels = c("PC","PC","PC","PC","PC","PC","PC","PC","PC","PC"),
          
          order=      c("new_time","new_time:bus","current_time","current_time:bus","additional.time","additional.time:bus",
                        
                        "additional.time:resp_female:","additional.time:child_female","additional.time:young_school_age","bus","log_income","married","num_kids_elem","num_kids_jun","young_school_age","age","resp_female","child_female","(Intercept) Probit","Std. Err."),
          omit.stat = c("chi2","ll"),
          
          covariate.labels=c("Walking time to nearby school","Walking time to nearby school*bus","Walking time to current school","Walking time to current school*bus","Additional walking time to nearby school","Additional walking time to nearby school*bus"
                             , "Additional time*female respondent","Additional time*female child","Additional time*age of child" ,"Bus","Log of income","Married",
                             "Number of children in elementary school", "Number of children in junior high school", "Grade of the youngest child", "Age of respondent","Gender of the respondent (1 is female)","Gender of the child (1 is female)"),
          
          
          
          
          title="Survey Double-hurdle Regression of Willingness to Accept for a School Merger for Elementary and Junior High Schools Separately Using Payment Card Answers",
          keep.stat="n",
          add.lines = list(c("Observations",rep(n_elem,5),rep(n_jun,5))),
         type="latex",out=paste0(path_results,"pc_elem_jun.tex"))


