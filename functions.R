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
      
      if(X["bus"]==0){decision_vec=c(0,5000*12/1000,10000*12/1000,15000*12/1000,20000*12/1000,50000*12/1000,100000*12/1000)}
      else{decision_vec=c(3000*12/1000,5000*12/1000,10000*12/1000,15000*12/1000,20000*12/1000,50000*12/1000,100000*12/1000)}
      
      
      # y: output, x1: censored data, x2: regression
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
      # if we observe refusers
      else if(y==8){ ret=as.numeric((1-pnorm((decision_vec[y-1]-multip1)/s)*(pnorm(multip2)) ))}
      # other responses
      else{
        ret=(as.numeric(((pnorm(multip2))*(pnorm((decision_vec[y]-multip1)/s)-pnorm((decision_vec[y-1]-multip1)/s)))))}
      
      
      
      
      
      return(ret)
      
      
      
    }
    
    # apply the log likelihood to each observations
    return(log((apply(X_tot,1,likelihood))))
    
    
    
    
    
    
    
  }
  
  # start with a random vector, I specify s= 10 to avoid infinite values
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
      
      
      if(X["bus"]==0){decision_vec=c(0,5000*12/1000,10000*12/1000,15000*12/1000,20000*12/1000,50000*12/1000,100000*12/1000)}
      else{decision_vec=c(3000*12/1000,5000*12/1000,10000*12/1000,15000*12/1000,20000*12/1000,50000*12/1000,100000*12/1000)}
      
      # y: output, x1: censored data, x2: regression
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














graph_cost=function(walk_coeff,walk_var,bus_coeff0,bus_var,cov_coeff,file){
  add_time=seq(0,90,by=0.2)
  
  walk_se=sqrt(walk_var)
  bus_se=sqrt(bus_var)
  bus_coeff=walk_coeff+bus_coeff0
  bus_se=sqrt(walk_se**2+bus_se**2+ 2*cov_coeff)
  
  
  
  walk_line=walk_coeff*add_time
  walk_line_upp=(walk_coeff+2*walk_se)*add_time
  walk_line_low=(walk_coeff-2*walk_se)*add_time
  
  
  
  
  bus_line=bus_coeff*add_time+196.1
  bus_line_upp=(bus_coeff+2*bus_se)*add_time+196.1
  bus_line_low= (bus_coeff-2*bus_se)*add_time+196.1
  
  df=data.frame(add_time,walk_line,walk_line_low,walk_line_upp,bus_line,bus_line_low,bus_line_upp)
  
  
  ggp <- ggplot(df, aes(x = add_time)) +    # Create default ggplot2 line plot
    geom_line(aes(y = walk_line,color="Walking disutility estimate")) +
    geom_line(aes(y = bus_line,color="Bus disitulity estimate + cost"))
  
  ggp +                                # Add shading between lines
    
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
  
  
  ggsave(paste0(path_results,sprintf("%s.png",file)))}



