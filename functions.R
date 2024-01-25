library(pracma)
library(maxLik)
library(stargazer)
library(dplyr)
library(lmtest)
library(ggplot2)






cens_mode_est_pc=function(formula,formula2,data){
  
  
  ### Model for the regression model
  X1=model.matrix(formula, data = data, rhs = 1)
  
  #### Probit model
  X2=model.matrix(formula2,data=data,rhs=1)
  ### Output
  Y=data[,all.vars(formula)[1]]
  ### Get variables for the summary model
  varnames=c(c(colnames(X1)[1:length(colnames(X1))],paste(colnames(X2)[1:length(colnames(X2))],"Probit"),"Std. Dev."))

  #### Survey values
  
  ### Bind both model vectors
  X_tot=data.frame(cbind(Y,X1,X2))
  #### define the likelihood function
  likelihood_min=function(params){
    
    #### set the parameters beta :for censored regressiond, gamma: probit regression, s: standard errors
    beta=params[1:ncol(X1)]
    gamma=params[(ncol(X1)+1):(ncol(X1)+ncol(X2))]
    s=as.numeric(params[length(params)])
    
    likelihood=function(X){
      ########The first walue was 3,000¥ for bussing children
      #its in 1000¥/year
      if(X["bus"]==0){decision_vec=c(0,5000*12/1000,10000*12/1000,15000*12/1000,20000*12/1000,50000*12/1000,100000*12/1000)}
      else{decision_vec=c(3000*12/1000,5000*12/1000,10000*12/1000,15000*12/1000,20000*12/1000,50000*12/1000,100000*12/1000)}
      
      
      # y: output, x1: censored data, x2: protesters
      y=as.numeric(X[1])
      x1=as.matrix(X[2:(ncol(X1)+1)])
      x2=as.matrix(X[(ncol(X1)+2):(ncol(X1)+ncol(X2)+1)])
      
      # multip1 & multip2: scalar product
      multip1=as.numeric(dot(x1,beta))
      multip2=as.numeric(dot(x2,gamma))
      #if we observe 0 on the survey
      if (y==1){
        
        ret=(as.numeric((pnorm(multip2))*pnorm((decision_vec[y]-multip1)/s)))
        
        
      }
      # if we observe that avary payment is refused
      else if(y==8){ ret=as.numeric((1-pnorm((decision_vec[y-1]-multip1)/s)*(pnorm(multip2)) ))}
      # other responses
      else{
        ret=(as.numeric(((pnorm(multip2))*(pnorm((decision_vec[y]-multip1)/s)-pnorm((decision_vec[y-1]-multip1)/s)))))}
      
      
      
      
      
      return(ret)
      
      
      
    }
    
    # apply the log likelihood to each observations
    return(log((apply(X_tot,1,likelihood))))
    
    
    
    
    
    
    
  }
  
  # start with a random vector, I specify s= 100 to avoid infinite  starting values
  start=c(rnorm(ncol(X1)+ncol(X2),0,0.001),100)
  # Maximum likelihood estimator
  res_optim=maxLik(likelihood_min,start = start,method="BHHH")
  # set the variable names
  names(res_optim$estimate)=varnames
  return(res_optim)
}




cens_mode_est_dc=function(formula,formula2,data){
  
  
  ### Model for the regression model
  X1=model.matrix(formula, data = data, rhs = 1)
  
  #### Probit model
  X2=model.matrix(formula2,data=data,rhs=1)
  ### Output
  Y=data[,all.vars(formula)[1]]
  dec_vec=data[,"decision_all"]
  
  
  ### Get variables for the summary model
  varnames=c(c(colnames(X1)[1:length(colnames(X1))],paste(colnames(X2)[1:length(colnames(X2))],"Probit"),"Std. Dev."))
  #### Survey values
  
  ### Bind both model vectors
  
  
  X_tot=data.frame(cbind(Y,dec_vec,X1,X2))
  
  #### define the likelihood function
  likelihood_min=function(params){
    
    
    #### set the parameters beta :for censored regressiond, gamma: probit regression, s: standard errors
    beta=params[1:ncol(X1)]
    gamma=params[(ncol(X1)+1):(ncol(X1)+ncol(X2))]
    
    s=as.numeric(params[length(params)])
    
    likelihood=function(X){
      
      ########The first walue was 3,000¥ for bussing children 
      #its in 1000¥/year
      if(X["bus"]==0){decision_vec=c(0,5000*12/1000,10000*12/1000,15000*12/1000,20000*12/1000,50000*12/1000,100000*12/1000)}
      else{decision_vec=c(3000*12/1000,5000*12/1000,10000*12/1000,15000*12/1000,20000*12/1000,50000*12/1000,100000*12/1000)}
      
      # y: output, x1: censored data, x2: protesters
      y=as.numeric(X[1])
      dec=as.numeric(X[2])
      x1=as.matrix(X[3:(ncol(X1)+2)])
      x2=as.matrix(X[(ncol(X1)+3):(ncol(X1)+ncol(X2)+2)])
      
      # multip1 & multip2: scalar product
      
      multip1=as.numeric(dot(x1,beta))
      multip2=as.numeric(dot(x2,gamma))
      #if we observe 0 on the survey
      # if we observe refusers
      if(dec==1){ ret=as.numeric((1-pnorm((decision_vec[y]-multip1)/s)*(pnorm(multip2)) ))}
      # other responses
      
      else{
        ret=(as.numeric((pnorm(multip2))*pnorm((decision_vec[y]-multip1)/s)))}
      
      
      
      
      
      
      return(ret)
      
      
      
    }
    
    # apply the log likelihood to each observations
    return((log((apply(X_tot,1,likelihood)))))
    
    
    
    
    
    
    
  }
  
  # start with a random vector, I specify s= 10 to avoid infinite values
  start=c(rnorm(ncol(X1)+ncol(X2),0,0.0001),100)
  # Maximum likelihood estimator
  res_optim=maxLik(likelihood_min,start = start,method="BHHH")
  # set the variable names
  names(res_optim$estimate)=varnames
  return(res_optim)
}











##########Producing the costs graphs
#### the inputs are the coefficients the variance and the covariance of the addtional time and additional time* bus 
##### coefficients. we specify as well the file name

graph_cost=function(walk_coeff,walk_var,bus_coeff0,bus_var,cov_coeff,file){
  #### create a grid
  add_time=seq(0,90,by=0.2)
  ### get se
  walk_se=sqrt(walk_var)
  bus_se=sqrt(bus_var)
  
  #### get the estimate of the bus disutility as it it the sum of the 2 estimates
  bus_coeff=walk_coeff+bus_coeff0
  ### get the se of the sum of the estimates
  bus_se=sqrt(walk_se**2+bus_se**2+ 2*cov_coeff)
  
  #### yearly individual bus cost
  bust_cost=196.1
  
  ####create the walking lines
  ###estimate lines
  walk_line=walk_coeff*add_time
  ###upper 95% confidence line
  walk_line_upp=(walk_coeff+1.96*walk_se)*add_time
  ###lower 95% confidence line
  walk_line_low=(walk_coeff-1.96*walk_se)*add_time
  
  
  
  ####create the bussing lines
  bus_line=bus_coeff*add_time+bust_cost
  ###upper 95% confidence line
  bus_line_upp=(bus_coeff+1.96*bus_se)*add_time+bust_cost
  ###lower 95% confidence line
  bus_line_low= (bus_coeff-1.96*bus_se)*add_time+bust_cost
  
  
  ###merge into one dataframe
  df=data.frame(add_time,walk_line,walk_line_low,walk_line_upp,bus_line,bus_line_low,bus_line_upp)
  
  
  
  ######make the ggplot
  ggp <- ggplot(df, aes(x = add_time)) +    
    geom_line(aes(y = walk_line,color="Walking disutility estimate")) +
    geom_line(aes(y = bus_line,color="Bus disitulity estimate + cost"))
  
  ggp +                              
    
    geom_ribbon(aes(x = add_time,
                    ymin = bus_line_low,
                    ymax = bus_line_upp,fill="Bus 95% confidence band"),
                
                alpha = 0.4)+
    
    geom_ribbon(aes(x = add_time,
                    ymin = walk_line_low,
                    ymax = walk_line_upp,fill="Walking 95% confidence band"),
                
                alpha = 0.2)+
    labs(x="Additional walking time to school (in min)",y ="Costs estimated (in 1,000 yens)")+ scale_color_manual("",values = c("Walking disutility estimate" = "blue", 
                                                                                                                                "Bus disitulity estimate + cost" = "black")) + 
    scale_linetype_manual(values = c("Walking disutility estimate" = "solid", 
                                     "Bus disitulity estimate + cost" = "solid"))+
    
    scale_fill_manual("",values=c("Walking 95% confidence band"="blue","Bus 95% confidence band"="gray"))+
    scale_x_continuous(breaks = seq(0, 90, by = 5))+
    scale_y_continuous(breaks = seq(0, 350, by = 50))+ coord_cartesian(ylim=c(0, 350))
  
  
  
  ######saving
  ggsave(paste0(path_results,sprintf("%s.png",file)))}



