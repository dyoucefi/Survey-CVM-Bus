library(pracma)
library(maxLik)
library(stargazer)
library(dplyr)
library(lmtest)
library(ggplot2)



path="/Users/utokyoresearch/Desktop/survey paper final"

path_data=paste0(path,"/data_survey/")
  
path_code=paste0(path,"/code/")
  
path_results=paste0(path,"/results/")



source(paste0(path_code,"functions.R"))
source(paste0(path_code,"preprocessing.R"))
source(paste0(path_code,"main_tables.R"))
source(paste0(path_code,"PC_elem_jun.R"))
source(paste0(path_code,"DC_elem_jun.R"))
source(paste0(path_code,"DC_check.R"))
source(paste0(path_code,"DC_elem_jun.R"))
source(paste0(path_code,"summary_stats.R"))