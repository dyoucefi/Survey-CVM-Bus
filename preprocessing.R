library(readxl) #yes
library(sf) #yes
library(rgdal) #yes
library(sp) #yes
library(dplyr) #yes
library(ggplot2) #yes
library(MASS) #yes
library(gridExtra) #yes
library(raster) #yes
library(maptools) #yes
library(rgeos) #yes
library(stringr)
library(stargazer)
library(vtable)
library(gtools)
library(openxlsx)
library(survival)
library(data.table)
library(tidyr)



merged.survey.j=read_xlsx(paste0(path_data,"survey_2.xlsx"),sheet="data")
###### relative speed of bus compared to walking
conv_factor=13.9/4

merged.survey.j$resp_female <- ifelse(merged.survey.j$F4==1, 1,0)
merged.survey.j$child_female <- ifelse(merged.survey.j$Q14==1, 1,0)


#####Convert the survey answers to the income related
merged.survey.j <- mutate(merged.survey.j,
                          income= case_when(
                            Q20 == 1 ~ 250000, 
                            Q20 == 2 ~ 750000,
                            Q20 == 3 ~ 1250000,
                            Q20 == 4 ~ 1750000, 
                            Q20 == 5 ~ 2250000,
                            Q20 == 6 ~ 2750000,
                            Q20 == 7 ~ 3500000, 
                            Q20 == 8 ~ 4500000,
                            Q20 == 9 ~ 5500000, 
                            Q20 == 10 ~ 6500000,
                            Q20 == 11 ~ 7500000,
                            Q20 == 12 ~ 8500000, 
                            Q20 == 13 ~ 9500000,
                            Q20 == 14 ~ 11250000,
                            Q20 == 15 ~ 13750000, 
                            Q20 == 16 ~ 20000000
                          ))

#### transorm to log
merged.survey.j$log_income <- log(merged.survey.j$income)



#additional time for walking 
merged.survey.j$current_time.walk<- ifelse(merged.survey.j$Q2==1, merged.survey.j$Q1, NA)
merged.survey.j$new_time.walk<-merged.survey.j$Q3
merged.survey.j$additional.time.walk=merged.survey.j$new_time.walk- merged.survey.j$current_time.walk





#additional time for biking
merged.survey.j$current_time.bike=ifelse(merged.survey.j$Q2==2, merged.survey.j$Q1, NA)
merged.survey.j$new_time.bike<-merged.survey.j$Q6
merged.survey.j$additional.time.bike=merged.survey.j$new_time.bike- merged.survey.j$current_time.bike

#additional time for bussing
### we convert the bus time to walking time using conv_factor
merged.survey.j$current_time.bus=ifelse(merged.survey.j$Q2 %in% c(3,4,5,6), merged.survey.j$Q1, NA)*conv_factor
merged.survey.j$new_time.bus=merged.survey.j$Q11*conv_factor
merged.survey.j$additional.time.bus=merged.survey.j$new_time.bus- merged.survey.j$current_time.bus



#####merge to one variable
merged.survey.j <- merged.survey.j %>%
  mutate(additional.time=ifelse(!is.na(additional.time.walk), additional.time.walk,
                                ifelse(!is.na(additional.time.bike), additional.time.bike,
                                       ifelse(!is.na(additional.time.bus), additional.time.bus, NA))))


####remove the observations with no additional time
merged.survey.j=merged.survey.j%>%filter(!is.na(additional.time))


merged.survey.j <- merged.survey.j %>%
  mutate(current_time=rowSums(merged.survey.j[, c("current_time.bus","current_time.walk","current_time.bike")], na.rm=T))

merged.survey.j <- merged.survey.j %>%
  mutate(new_time=rowSums(merged.survey.j[, c("new_time.bus","new_time.walk","new_time.bike")], na.rm=T))

####dummy for taking the bus
merged.survey.j$bus=ifelse(merged.survey.j$Q2 %in% c(3,4),1,0)

####we keep only the walking and bussing individuals
merged.survey.j=merged.survey.j%>%filter( Q2 %in%c(1,3,4) )


#dummy for maried
merged.survey.j$married <- ifelse(merged.survey.j$Q17==1, 1, 0)
#DUMMY FOR LIVING IN THE 10 RICHEST PREFECTURES
merged.survey.j$rich_pref <- ifelse(merged.survey.j$SQW3==13 | merged.survey.j$SQW3==23 | merged.survey.j$SQW3==22 | merged.survey.j$SQW3==9 | merged.survey.j$SQW3==16 | merged.survey.j$SQW3==24 | merged.survey.j$SQW3==35 | merged.survey.j$SQW3==34 | merged.survey.j$SQW3==25 | merged.survey.j$SQW3==10, 1, 0)
#DUMMY FOR LIVING IN THE 10 LOWEST INCOME PREFECTURES
merged.survey.j$low_income_pref <- ifelse(merged.survey.j$SQW3==47 | merged.survey.j$SQW3==31 | merged.survey.j$SQW3==42 | merged.survey.j$SQW3==45 | merged.survey.j$SQW3==43 | merged.survey.j$SQW3==46 | merged.survey.j$SQW3==2 | merged.survey.j$SQW3==32 | merged.survey.j$SQW3==41 | merged.survey.j$SQW3==5, 1, 0)
#RESPONDENTS AGE
merged.survey.j$age <- merged.survey.j$F5

#WORKING STATUS
merged.survey.j$employed <- ifelse(merged.survey.j$Q18_1!=14, 1, 0)
###current child allowance
merged.survey.j <- mutate(merged.survey.j,
                          child_allowance= case_when(
                            Q15 == 1 ~ 5000, 
                            Q15 == 2 ~ 10000,
                            Q15 == 3 ~ 15000,
                            Q15 == 4 ~ 20000, 
                            Q15 == 5 ~ 0
                          ))
#NUMBER OF KIDS IN ELEMENTARY 
merged.survey.j <- merged.survey.j %>%
  mutate(num_kids_elem=F1_1+F1_2+F1_3+F1_4+F1_5+F1_6)
#NUMBER OF KIDS IN JUNIOR

merged.survey.j <- merged.survey.j %>%
  mutate(num_kids_jun=F1_7+F1_8+F1_9)
#TOTAL KIDS
merged.survey.j <- merged.survey.j %>%
  mutate(num_kids=F1_1+F1_2+F1_3+F1_4+F1_5+F1_6+F1_7+F1_8+F1_9)



#######remove people with more than 7 kids
merged.survey.j=merged.survey.j%>%filter(num_kids<=7)


#SCHOOL AGE OF THE YOUNGEST CHILD
merged.survey.j$young_school_age <- merged.survey.j$SF1
#Children's school expenses Q10
merged.survey.j$school_expenses <- merged.survey.j$Q10


########Link responses of the professions of the household
#####Single
merged.survey.j=merged.survey.j%>%mutate(single_profession=case_when(
Q19==1~"Regular staff",
  Q19==2~"Pato",
  Q19==3~"Part-time job",
  Q19==4~"Temporary employees",
  Q19==5~"Contract employee",
  Q19==6~"Commission",
  Q19==7~"Others",
  Q19==8~"Officers of companies",
  Q19==9~"Self-employed (w emp)",
  Q19==10~"Self-employed (no emp)",
  Q19==11~"Direct sales assistance",
  Q19==12~"Home job",
  Q19==13~"On chilbirth/care leave",
  Q19==14~"Not working"))


###Married for husband
merged.survey.j=merged.survey.j%>%mutate(married_profession_husband=case_when(
  Q18_1==1~"Regular staff",
  Q18_1==2~"Pato",
  Q18_1==3~"Part-time job",
  Q18_1==4~"Temporary employees",
  Q18_1==5~"Contract employee",
  Q18_1==6~"Commission",
  Q18_1==7~"Others",
  Q18_1==8~"Officers of companies",
  Q18_1==9~"Self-employed (w emp)",
  Q18_1==10~"Self-employed (no emp)",
  Q18_1==11~"Direct sales assistance",
  Q18_1==12~"Home job",
  Q18_1==13~"On chilbirth/care leave",
  Q18_1==14~"Not working"))

####Married for wife
merged.survey.j=merged.survey.j%>%mutate(married_profession_wife=case_when(
  Q18_2==1~"Regular staff",
  Q18_2==2~"Pato",
  Q18_2==3~"Part-time job",
  Q18_2==4~"Temporary employees",
  Q18_2==5~"Contract employee",
  Q18_2==6~"Commission",
  Q18_2==7~"Others",
  Q18_2==8~"Officers of companies",
  Q18_2==9~"Self-employed (w emp)",
  Q18_2==10~"Self-employed (no emp)",
  Q18_2==11~"Direct sales assistance",
  Q18_2==12~"Home job",
  Q18_2==13~"On chilbirth/care leave",
  Q18_2==14~"Not working"))

#####intervert the variables if the respondent is the wife
merged.survey.j[merged.survey.j$resp_female==1,c("married_profession_wife","married_profession_husband")]=merged.survey.j[merged.survey.j$resp_female==1,c("married_profession_husband","married_profession_wife")]


####we remove the responses where income is too high given the professions of the household
full_time_profession=c(1,8,4,5,611)

###yearly minimum wage
min_wage=850*130*12

### we remove if full time but has a lower wage than the minium
merged.survey.j=merged.survey.j%>%filter(!( Q19 %in% full_time_profession & income<min_wage ))

###same for couples
merged.survey.j=merged.survey.j%>%filter(! (Q18_1 %in% full_time_profession &Q18_2 %in% full_time_profession & income<2*min_wage ))
merged.survey.j=merged.survey.j%>%filter(! ((Q18_1 %in% full_time_profession |Q18_1 %in% full_time_profession )& income<min_wage ))

####remove the outliers in the distribution
single=merged.survey.j%>%filter(!is.na(single_profession))


single=single%>%group_by(single_profession)%>%mutate(H=1.5* IQR(income, na.rm =T),
                                                     q25=quantile(income, probs=c(.25, .75), na.rm =T)[1],
                                                     q75=quantile(income, probs=c(.25, .75), na.rm =T)[2])  %>%
                                        filter(income<q75+H& income>q25-H)%>%ungroup()



####same for married 
married=merged.survey.j%>%filter(is.na(single_profession))
married=married%>%
  group_by(val_1 = pmin(married_profession_husband, married_profession_wife), val_2 = pmax(married_profession_husband, married_profession_wife))

married=married%>%group_by(val_1)%>%mutate(H=1.5* IQR(income, na.rm =T),
                                                     q25=quantile(income, probs=c(.25, .75), na.rm =T)[1],
                                                     q75=quantile(income, probs=c(.25, .75), na.rm =T)[2])  %>%
  filter(income<q75+H& income>q25-H)%>%ungroup()


married=married%>%group_by(val_2)%>%mutate(H=1.5* IQR(income, na.rm =T),
                                           q25=quantile(income, probs=c(.25, .75), na.rm =T)[1],
                                           q75=quantile(income, probs=c(.25, .75), na.rm =T)[2])  %>%
  filter(income<q75+H& income>q25-H)%>%ungroup()


married=married%>%dplyr::select(-c(val_1,val_2))


#####recombine the two datasets now without irrational values in profession and income
merged.survey.j=rbind(single,married)







##### we create a dummmy for people that did not had a bus question
merged.survey.j$bus_question=1

merged.survey.j <-merged.survey.j%>%mutate(bus_question=replace(bus_question,is.na(merged.survey.j$SQ4)&
                                                                  is.na(merged.survey.j$SQ7)&
                                                                  is.na(merged.survey.j$SQ12),0))




####we only keep the ones that did not had a bus question or had a coherent response on the bus question
merged.survey.j <- merged.survey.j%>%filter( (bus_question==0) |(((TQ4==2) |
                                                                    (TQ7==2) | 
                                                                    (TQ12==2))))

################# All database
write.csv(merged.survey.j,paste0(path_data,"processed_survey_final.csv"))





#############Payment Card

payment_card=data.frame(merged.survey.j)

### we keep the household that knew in which grade was their child 
#### had a coherent reponse on payment card
payment_card <- payment_card[payment_card$UF1!=3, ]
payment_card <- filter(payment_card, ((payment_card$SQ5==1 & payment_card$TQ5==2) |
                                        (payment_card$SQ8==1 & payment_card$TQ8==2) | 
                                        (payment_card$SQ13==1 & payment_card$TQ13==2)))






#### we keep the lowest value accepted in increasing the child allowance
payment_card$Q5_decision <- ifelse(payment_card$TQ5==2, 8-rowSums(payment_card[,paste("Q5",1:7,sep="_")]-1), NA)
payment_card$Q8_decision <- ifelse(payment_card$TQ8==2, 8-rowSums(payment_card[,paste("Q8",1:7,sep="_")]-1), NA)
payment_card$Q13_decision <- ifelse(payment_card$TQ13==2, 8-rowSums(payment_card[,paste("Q13",1:7,sep="_")]-1), NA)




####merge to one variable
payment_card <- payment_card %>%
  mutate(decision_all=rowSums(payment_card[, c("Q5_decision","Q8_decision","Q13_decision")], na.rm=T))


write.csv(payment_card,paste0(path_data,"payment_card.csv"))




############### Dichotomous Choice

dichotomous=data.frame(merged.survey.j)

### we keep the household that knew in which grade was their child 
#### had a coherent reponse on dichotomous choice

dichotomous <- dichotomous[dichotomous$UF1!=3, ]
dichotomous <- filter(dichotomous, ((dichotomous$SQ5==2 & dichotomous$TQ5==2) |
                                      (dichotomous$SQ8==2 & dichotomous$TQ8==2) | 
                                      (dichotomous$SQ13==2 & dichotomous$TQ13==2)))




#####we find the value that wa proposed
###walking
Q5_val=which( !is.na(dichotomous[,paste("Q5",1:7,sep="_")]), arr.ind=TRUE)
Q5_val=data.frame(Q5_val[,"col"],row.names = Q5_val[,"row"])
colnames(Q5_val)="Q5_val"


#####we find if the respondent has accepted or refused the offer
dichotomous=merge(dichotomous, Q5_val, by=0, all.x=TRUE)
dichotomous$Q5_decision=rowSums(dichotomous[,paste("Q5",1:7,sep="_")],na.rm=T)
dichotomous=dichotomous[,2:ncol(dichotomous)]

###biking
Q8_val=which( !is.na(dichotomous[,paste("Q8",1:7,sep="_")]), arr.ind=TRUE)
Q8_val=data.frame(Q8_val[,"col"],row.names = Q8_val[,"row"])
colnames(Q8_val)="Q8_val"

dichotomous=merge(dichotomous, Q8_val, by=0, all.x=TRUE)
dichotomous$Q8_decision=rowSums(dichotomous[,paste("Q8",1:7,sep="_")],na.rm=T)
dichotomous=dichotomous[,2:ncol(dichotomous)]


###bussing
Q13_val=which( !is.na(dichotomous[,paste("Q13",1:7,sep="_")]), arr.ind=TRUE)
Q13_val=data.frame(Q13_val[,"col"],row.names = Q13_val[,"row"])
colnames(Q13_val)="Q13_val"

dichotomous=merge(dichotomous, Q13_val, by=0, all.x=TRUE)
dichotomous$Q13_decision=rowSums(dichotomous[,paste("Q13",1:7,sep="_")],na.rm=T)
dichotomous=dichotomous[,2:ncol(dichotomous)]


####we merge all into two variables
dichotomous$decision_all=rowSums(dichotomous[,paste(c("Q5","Q8","Q13"),"decision",sep="_")])
dichotomous$decision_all=as.numeric(dichotomous$decision_all)


dichotomous$value_all=rowSums(dichotomous[,paste(c("Q5","Q8","Q13"),"val",sep="_")],na.rm=T)
dichotomous$value_all=as.numeric(dichotomous$value_all)




write.csv(dichotomous,paste0(path_data,"dichotomous.csv"))












###########

pc_check=data_frame(payment_card)


####we generate a random value of the card
pc_check$value_all=sample(1:7,nrow(pc_check),replace=T)
### we create the hypotetical answers if it was a dichotomous choice instead
pc_check$decision_all2=ifelse(pc_check$value_all>=pc_check$decision_all,2,1)
pc_check$decision_all=pc_check$decision_all2

write.csv(pc_check,paste0(path_data,"pc_check.csv"))


