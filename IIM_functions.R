###############################################################################
####################LITERATURE REFERENCES for Equation#########################
###############################################################################
#(A) Haimes, Yacov Y., et al. "Inoperability input-output model for interdependent infrastructure sectors. I: Theory and methodology" Journal of Infrastructure Systems 11.2 (2005): 67-79.
#(B) Haimes, Yacov Y., et al. "Inoperability input-output model for interdependent infrastructure sectors. II: Case studies." Journal of Infrastructure Systems 11.2 (2005): 80-92.
#(C) Lian, Chenyang, and Yacov Y. Haimes. "Managing the risk of terrorism to interdependent infrastructure systems through the dynamic inoperability input–output model." Systems Engineering 9.3 (2006): 241-258.
#(D) 


###############################################################################
###########################LEGEND OF SYMBOLS###################################
###############################################################################
#x = total industry output vector;
#x_{deg} = degraded total industry output vector
#P = Transformation matrix
#Z = 
#A = technical coefficient matrix
#A* = Demand-based interdependency matrix 
#theta = Interdependency index
#ro = Interdependency ratio
#q = inoperability vector (between 0 and 1)
#c* = reduced final demand vector
#c = final demand vector
#c_{deg} = degraded final demand

###############################################################################
##############INOPERABILITY VARIABLE INITIALIZATION############################
###############################################################################

#TECHNICAL COEFFICIENT MATRIX A
#INPUT: Z, x; OUTPUT A
A_create <- function(Z_,x_){
  A_ <- matrix(0,length(x_),length(x_))
  A_ <- Z_%*%diag(as.vector(vect_inv(x_)))
  return(A_)
}


A_create2 <- function(Z_,x_){
  A_ <- matrix(0,length(x_),length(x_))
  for(i in 1:length(x_)){
    for(j in 1:length(x_)){
      A_[i,j] <- Z_[i,j] / x_[j]
    }
  }
  return(A_)
}

#SET IGRAPH OBJECT OF A and x
A_igraph_obj_create <- function(A_,x_){
  A_nodes_ <- data.frame(rownames(A_),x_)
  colnames(A_nodes_) <- c("id","size")
  A_links_ <- as.matrix(A_)
  IO_net_ <- graph_from_adjacency_matrix(A_links_,weighted = TRUE,add.rownames=TRUE)
  V(IO_net_)$size <- A_nodes_$size/max(A_nodes_$size)
  return(IO_net_)
}



#TRANSFORMATION MATRIX (Equation 35 from A)
#INPUT: x; OUTPUT: P
P_create <- function(x_){
  x_i_ <- vect_inv(x_)
  return(diag(as.vector(x_i_)))
}

#TRANSFORMATION MATRIX INVERSE (Equation 35 from A)
#INPUT:  x; OUTPUT: P^{-1}
P_inv_create <- function(x_){
  return(diag(as.vector(x_)))
}

#DEMAND-BASED INTERDEPENDENCY MATRIX A* (Equation 33 from A)
#INPUT: x, A; OUTPUT: A*
A_star <- function(A_,x_){
  return(as.matrix(P_create(x_)) %*% 
         as.matrix(A_) %*% 
         as.matrix(P_inv_create(x_)))
}

#INPUT: P^{-1}, A, P; OUTPUT: A*
A_star2 <- function(P_,A_,P_i_){
  return(as.matrix(P_) %*% as.matrix(A_) %*% as.matrix(P_i_))
}

#INTERDEPENDENCY INDEX FOR SECTOR i (Equation 31 from C)
#INPUT: A; OUTPUT: Theta
theta_create <- function(A_){
  theta_ <- matrix(0,1,length(A_[1,]))
  for(i in 1:length(A_[1,])){
    theta_[i] <- 1 - A_[i,i]
  }
  return(theta_)
}

theta_create2 <- function(A_){
  theta_ <- matrix(0,1,length(A_[1,]))
  for(i in 1:length(A_[1,])){
    theta_[i] <- A_[i,i]
  }
  return(theta_)
}

#INTERDEPENDENCY RATIO OF SECTOR i AND j  (Equation 32 from C)
#INPUT: theta; OUTPUT: ro
ro_create <- function(theta_){
  ro_ <- matrix(0,length(theta_),length(theta_))
  for(i in 1:length(theta_)){
    for(j in 1:length(theta_)){
      ro_[i,j] <- theta_[i] / theta_[j]
    }
  }
  return(ro_)
}


#REDUCED DEMAND VECTOR c* (Equation 32 from A)
#INPUT: c, c_perc, x; OUTPUT: c*
c_star <- function(c_,c_perc_,x_){
  # avoid divisions by 0 (if the production is 0, c_ and then c_s will be 0)
  x_[ which(x_==0) ] <- 1
  return(matrix(c_ * c_perc_ / x_,length(x_),1))
}


#MAX VALUES FOR REDUCED DEMAND VECTOR c*_{max} (Equation 32 from A)
#INPUT: c, x; OUTPUT: c*_{max}
c_star_max <- function(c_,x_){
  return(matrix(c_ / x_,1,length(x_)))
}



#INOPERABILITY VECTOR q (EQuation 34 from A)
#INPUT: x, x_deg; OUTPUT: q 



#INOPERABILITY VECTOR q (EQuation 25 from C)
#INPUT: x, x_deg; OUTPUT: q 
q_infinite <- function(A_s_,c_s_){
  return(inv(I_matr(length(c_s_))-A_s_) %*% c_s_)
}


#INDUSTRY RESILIENCE COEFFICIENTS K (Equation 60 from A)
#INPUT: ; OUTPUT: 
#K_create <- function(){
#  return(matrix(log(q_0_[2]/q2_inf[2])*(1-A_s[i,i])/t2
#                ,1,tot_sector))
#}


#industry resilience coefficients manually defined
K_create <- function(n_,k_ = 1){
  return(as.matrix(diag(as.vector(matrix(k_,1,n_)))))
}



###############################################################################
#####################INOPERABILITY DYNAMIC MODEL###############################
###############################################################################

#DISCRETE MODEL
q_discrete <- function(K_,q_k_,A_s_,c_s_){
  return( pmin( matrix(1, length(q_k_), 1), 
                 q_k_ + K_ %*%   (A_s_ %*% q_k_ + c_s_ - q_k_ )))
}

#CONTINUOUS MODEL
q_continuous <- function(K_, q_0_, q_inf_, A_s_, c_s_, t_){
  I_ <- I_matr( length(q_0) )
  return(
    pmin(matrix(1,length(q_0_),1),
         q_inf_ + 
         expm(-K_ %*% (I_ - A_s_) * t_) %*%
         (q_0_ - q_inf_)
        ))
}



###############################################################################
#############################ECONOMIC LOSS#####################################
###############################################################################
economic_loss <- function(x_, q_dyn_, time_def_ = "day"){
  Q_dyn_ <- q_dyn_
  if(time_def_ == "day"){
    for(i in 1:length(q_dyn_[,1])){
      Q_dyn_[i,] <- q_dyn_[i,]*x_/365    
      }
  }
  else if(time_def_ == "year"){
    for(i in 1:length(q_dyn_[,1])){
      Q_dyn_[i,] <- q_dyn_[i,]*x_    
    }
  } 
  return(Q_dyn_)
}

cumulative_economic_loss <- function(x_, q_dyn_, time_def_ = "day"){
  Q_dyn_cum_ <- q_dyn_
  if(time_def_ == "day"){
    Q_dyn_cum_[1,] <- q_dyn_[1,]*x_/365    
    
    for(i in 2:length(Q_dyn_cum_[,1])){
      Q_dyn_cum_[i,] <- Q_dyn_cum_[i-1,] + q_dyn_[i,]*x_/365    
    }
  }
  else if(time_def_ == "year"){
    Q_dyn_cum_[1,] <- q_dyn_[1,]*x_    
    for(i in 2:length(Q_dyn_cum_[,1])){
      Q_dyn_cum_[i,] <- Q_dyn_cum_[i-1,] + q_dyn_[i,]*x_    
    }
  } 
  return(Q_dyn_cum_)
}


total_economic_loss <- function(x_, q_dyn_, time_def_ = "day"){
  Q_ <- matrix(NA,length(q_dyn_[1,]),1)
  
  if(time_def_ == "day"){
    for(i in 1:length(q_dyn_[1,])){
      Q_[i] <- sum(q_dyn_[,i]) / 365
    }
  }
  else if(time_def_ == "year"){
    for(i in 1:length(q_dyn_[1,])){
        Q_[i] <- sum(q_dyn_[,i])
    }
  } 
  Q_ <- x_ * Q_
  return(Q_)
}



###############################################################################
##############################GENERIC OPERATORS################################
###############################################################################
#Inverse of a generic vector x^{-1}
vect_inv <- function(x_){
  return(ifelse(x_==0,0,1/x_))
}

#Sum Vector s 
#!!!!!!!?????????SHRINK NAME TO s!!!!!!????????
sum_vect <- function(n_){
  return(matrix(1, 1, n_))
}

#Identity Matrix 
#!!!!!!!?????????SHRINK NAME TO I!!!!!!????????
I_matr <- function(n_){
  return(diag(as.vector(sum_vect(n_))))
}

#Random vector
rand_vect <- function(n_, min_ = 0.0, max_ = 1.0){
  return(matrix(runif(n_, min=min_, max=max_),n_,1))
}

###############################################################################
###############Modified functions from IOtable library#########################
###############################################################################


# returns the column index of the total if it exists and include_total = TRUE,
# or the last columns before total otherwise
# NB: 'cpa_u' %in% tolower ( names (it_supply_iim)) returns FALSE even though it_supply_iim[65,1] = CPA_U (though it_supply_iim[1,66] = U..??)
quadrant_separator <- function(data_table, 
                               include_total = FALSE) {
  last_column <- 2
  if ( any(c("total", "cpa_total") %in% tolower(names(data_table))) ) {
    last_column <- which(tolower(names(data_table))  %in% c("total", "cpa_total") )
    if ( ! include_total ) last_column <- last_column -1 #if total columns are not needed, the last but total
  } else if ( any(c("households", "p13_s14") %in% tolower(names(data_table)))) {
    last_column <- which(tolower(names(data_table)) %in% c("households", "p13_s14")-1 )
  } else  if ( 'cpa_u' %in% tolower ( names (data_table)) ) { 
    last_column <- which(tolower(names(data_table)) =='cpa_u')
  } else if ( 'cpa_t' %in% tolower (names(data_table))) { 
    last_column <- which(tolower(names(data_table)) =='cpa_t')
  } else if ( 'cpa_s96' %in% tolower (names(data_table))) { 
    last_column <- which(tolower(names(data_table)) =='cpa_s96')
  } else if ( 'other_services_group' %in% tolower (names(data_table))) { 
    last_column <- which(tolower(names(data_table)) =='other_services_group')
  }
  
  if ( last_column == 2) {
    warning ( "The last column was not found")
  }
  last_column
}


###############################################################################
########################Random Generator for Z, c and x########################
###############################################################################
rand_matrix <- function(n_, max_val_ = 1000){
  Z_ <- matrix(0,n_,n_)
  for(i in 1:n_){
    for(j in 1:n_){
      if(i==j){
        Z_[i,j] <- runif(1, 0, max_val_/10) 
      }else{
        Z_[i,j] <- runif(1, 0, max_val_)
      }
    }
  }
  return(Z_)
}


rand_finaldemand <- function(n_, max_val_ = 1000){
  c_ <- matrix(0,n_,1)
  for(i in 1:n_){
    c_[i] <- runif(1, 0, max_val_) 
  }
  return(c_)
}


rand_tot_output <- function(Z_,c_){
  n_ <- length(c_)
  x_ <- matrix(0,n_,1)
  for(i in 1:n_){
    x_[i] <- sum(Z_[i,]) + c_[i]
  }
  return(x_)
}





###############################################################################
###########################Recursive functions#################################
###############################################################################

rec_matrix_prod <- function(X_,n_) {
  if(n_<=1){
    return(X_)
  }else{
    return(X_ %*% rec_matrix_prod(X_,n_-1))
  }
}

factorial <- function(n_) {
  if(n_ <= 1) {
    return(1)
  } else { 
    return(n_ * factorial(n_-1))
  }
}




###############################################################################
##################Air Emission Account functions###############################
###############################################################################

#Setup extended-environmentally matrix B according to A (names and order)
#INPUT: aea_,A_ ; OUTPUT: B_
B_setup <- function(aea_,A_){
  
  B_temp_ <- matrix(NA,length(unique(aea_$airpol)),length(unique(aea_$nace_r2)))
  row.names(B_temp_) <- as.matrix(unique(aea_$airpol))
  colnames(B_temp_) <- as.matrix(unique(aea_$nace_r2))
  
  for(i in 1: length(colnames(B_temp_))){
    if(str_sub(colnames(B_temp_),4,4)[i] == "-" | str_sub(colnames(B_temp_),4,4)[i] == "_"){
      colnames(B_temp_)[i] <-  gsub('^(.{4}).', '\\1', colnames(B_temp_)[i])
    }
  }
  
  for(i in 1:length(unique(aea_$airpol))){
    for(j in 1:length(unique(aea_$nace_r2))){
      B_temp_[i,j] <- filter(aea_, airpol == unique(aea_$airpol)[i] & 
                         nace_r2 == unique(aea_$nace_r2)[j])$values
    }
  }
  
  B_int_ <- intersect(colnames(B_temp_),colnames(A_))
  B_<-B_temp_[,B_int_[order(match(B_int_,colnames(A_)))]]
  return(B_)
}








###############################################################################
##################PERMUTATIONS OF SCENARIOS####################################
###############################################################################

init_scen <- function(scen_){
  myDF_ <- data.frame(q=I(list()),qmin=I(list()),qmax=I(list()))
  naces_ <- rownames(scen_)
  #INITIALIZE DF WITH VALUES FROM SCENARIOS
  for(i in 1:ncol(scen_)){
    myDF_[[i, 'q']] <- t(as.matrix(scen_[,i]));
    colnames(myDF_[[i, 'q']]) <- as.matrix(naces);
    
    myDF_[[i, 'qmin']] <- t(as.matrix(scen_[,i]));
    colnames(myDF_[[i, 'qmin']]) <- as.matrix(naces);
    
    myDF_[[i, 'qmax']] <- t(as.matrix(scen_[,i]));
    colnames(myDF_[[i, 'qmax']]) <- as.matrix(naces);
  }
  return(myDF_)
}


scen_dynamic <- function(myDF_,scen_){

  for(i in 1:ncol(scen_)){
    for(k in 1:(t1-1)){
      df_temp_ <- data.frame(q=I(list()),qmin=I(list()),qmax=I(list()))
      df_temp_[[i,'q']] <- as.matrix(myDF_$q[[i]][k,])
      df_temp_[[i,'qmin']] <- as.matrix(myDF_$q[[i]][k,])
      df_temp_[[i,'qmax']] <- as.matrix(myDF_$q[[i]][k,])
      
      for(l in 1:nrow(scen_)){
        if(k<30){
          df_temp_$q[[i]][l] <- max(scen_[l,i],t(q_discrete(K,myDF_$q[[i]][k,],A1_ii_s,c_s))[l])
          df_temp_$qmin[[i]][l] <- max(scen_[l,i],t(q_discrete(K2,myDF_$qmin[[i]][k,],A1_ii_s,c_s))[l])
          df_temp_$qmax[[i]][l] <- max(scen_[l,i],t(q_discrete(K1,myDF_$qmax[[i]][k,],A1_ii_s,c_s))[l])
          
        }else{
          df_temp_$q[[i]][l] <- t(q_discrete(K,myDF_$q[[i]][k,],A1_ii_s,c_s))[l]
          df_temp_$qmin[[i]][l] <- t(q_discrete(K2,myDF_$qmin[[i]][k,],A1_ii_s,c_s))[l]
          df_temp_$qmax[[i]][l] <- t(q_discrete(K1,myDF_$qmax[[i]][k,],A1_ii_s,c_s))[l]
          
        }
      }
      myDF_$q[[i]] <- rbind(myDF_$q[[i]],t(df_temp_$q[[i]]))
      myDF_$qmin[[i]] <- rbind(myDF_$qmin[[i]],t(df_temp_$qmin[[i]]))
      myDF_$qmax[[i]] <- rbind(myDF_$qmax[[i]],t(df_temp_$qmax[[i]]))
    }
    rownames(myDF_$q[[i]]) <- seq(1:t1)
    rownames(myDF_$qmin[[i]]) <- seq(1:t1)
    rownames(myDF_$qmax[[i]]) <- seq(1:t1)
  }
  return(myDF_)
}





DF_two <- function(scen_){
  #TWO SECTOR CLOSED
  string_temp_ <- "NA"
  DF_two_ <- matrix(nrow = nrow(scen_))
  for (i in 1:ncol(scen_)) {
    for(ii in 1:ncol(scen_)){
      if(ii>i){
        string_temp_ <- cbind(string_temp_,
                             paste0(colnames(scen_[i]),
                                    "+",
                                    colnames(scen_[ii])))
        DF_two_ <-cbind(DF_two_,scen_[i]+scen_[ii])
      }
    }
  }
  colnames(DF_two_) <- string_temp_
  DF_two_[1] <- NULL
  return(DF_two_)
}




DF_three <- function(scen_){
  #THREE SECTOR CLOSED
  string_temp_ <- "NA"
  DF_three_ <- matrix(nrow = nrow(scen_))
  for (i in 1:ncol(scen_)) {
    for(ii in 1:ncol(scen_)){
      for(iii in 1:ncol(scen_)){
        if(ii>i && iii>ii){
          string_temp_ <- cbind(string_temp_,
                               paste0(colnames(scen_[i]),
                                      "+",
                                      colnames(scen_[ii]),
                                      "+",
                                      colnames(scen_[iii])))
          DF_three_ <-cbind(DF_three_,scen_[i]+scen_[ii]+scen_[iii])
        }      
      }
    }
  }
  colnames(DF_three_) <- string_temp_
  DF_three_[1] <- NULL
  return(DF_three_)
}



DF_four <- function(scen_){
  #FOUR SECTOR CLOSED
  string_temp_ <- "NA"
  DF_four_ <- matrix(nrow = nrow(scen_))
  for (i in 1:ncol(scen_)) {
    for(ii in 1:ncol(scen_)){
      for(iii in 1:ncol(scen_)){
        for(iiii in 1:ncol(scen_)){
          if(ii>i & iii>ii & iiii>iii){
            string_temp_ <- cbind(string_temp_,
                                 paste0(colnames(scen_[i]),
                                        "+",
                                        colnames(scen_[ii]),
                                        "+",
                                        colnames(scen_[iii]),
                                        "+",
                                        colnames(scen_[iiii])))
            DF_four_ <-cbind(DF_four_,scen_[i]+scen_[ii]+
                               scen_[iii]+scen_[iiii])
          }      
        }
      }
    }
  }
  colnames(DF_four_) <- string_temp_
  DF_four_[1] <- NULL
  return(DF_four_)
}



DF_all <- function(scen_){
  #ALL SECTOR CLOSED
  DF_all_ <- 0
  for(i in 1:ncol(scen_)){
    DF_all_ <- DF_all_+ scen_[i]
  }
  colnames(DF_all_) <- "all sectors"
  return(DF_all_)
  
}




#COMPUTE ECONOMIC LOSSES (THIS USES X1 global VAR)
Q_scen <- function(myDF_,scen_){
  Q_scen_ <- data.frame(
    scenario=matrix(NA,length(scen_),1),
    ymed=matrix(NA,length(scen_),1),
    ymin=matrix(NA,length(scen_),1),
    ymax=matrix(NA,length(scen_),1)
  )
  
  for(i in 1:ncol(scen_)){
    Q_scen_['scenario'][i,] <- paste0(i,". ",colnames(scen_)[i])
    Q_scen_['ymed'][i,] <- sum(total_economic_loss(x1,myDF_$q[[i]]))
    Q_scen_['ymin'][i,] <- sum(total_economic_loss(x1,myDF_$qmin[[i]]))
    Q_scen_['ymax'][i,] <- sum(total_economic_loss(x1,myDF_$qmax[[i]]))  
  }
  return(Q_scen_)
  
}

Q_scen_plot <- function(Q_plot_){
  plot_ <- ggplot(data=Q_plot_)+
    geom_bar(aes(x=scenario, y=ymed),stat="identity")+
    geom_errorbar(aes(x=scenario,ymin=ymin,ymax=ymax),
                  width=0.4, colour="orange", alpha=0.9)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=12),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12))+
    labs(y = "Economic losses [mln euro]")
  return(plot_)
}


B_scen <- function(myDF_,scen_){
  myDF2_<- myDF_
  for(i in 1:ncol(scen_)){
    myDF2_$q[[i]] <- myDF2_$q[[i]][ , -which(colnames(q_disc) %in% c("L68B"))]
    myDF2_$qmin[[i]] <- myDF2_$qmin[[i]][ , -which(colnames(q_disc) %in% c("L68B"))]
    myDF2_$qmax[[i]] <- myDF2_$qmax[[i]][ , -which(colnames(q_disc) %in% c("L68B"))]
  }
  
  #2015
  #1: "ACG" - 2: "CH4" - 3: "CH4_CO2E" - 4: "CH4_NMVOCE" - 5: "CO" - 6: "CO2" - 
  #7: "CO2_BIO" - 8: "CO_NMVOCE" - 9: "GHG" - 10: "HFC_CO2E" - 11: "N2O" - 
  #12: "N2O_CO2E" - 13: "NF3_SF6_CO2E" - 14: "NH3" - 15: "NH3_SO2E"   
  j <- 9 #it corresponds to B rows - B[j,] 
  for(s in 1:ncol(scen_)){
    for(i in 1:length(scen_[1,])){
      myDF2_$q[[s]][,i] <- myDF2_$q[[s]][,i]*B[j,i]/365
      myDF2_$qmin[[s]][,i] <- myDF2_$qmin[[s]][,i]*B[j,i]/365
      myDF2_$qmax[[s]][,i] <- myDF2_$qmax[[s]][,i]*B[j,i]/365     
    }
  }
  
  B_scen_ <- data.frame(
    scenario=matrix(NA,length(scen_),1),
    ymed=matrix(NA,length(scen_),1),
    ymin=matrix(NA,length(scen_),1),
    ymax=matrix(NA,length(scen_),1)
  )
  
  for(i in 1:ncol(scen_)){
    B_scen_['scenario'][i,] <- paste0(i,". ",colnames(scen_)[i])
    B_scen_['ymed'][i,] <- sum(myDF2_$q[[i]])
    B_scen_['ymin'][i,] <- sum(myDF2_$qmin[[i]])
    B_scen_['ymax'][i,] <- sum(myDF2_$qmax[[i]]) 
  }

  return(B_scen_)
}

B_scen_plot <- function(B_plot_){
  B_plot_ <- ggplot(data=B_plot_)+
    geom_bar(aes(x=scenario, y=ymed),stat="identity")+
    geom_errorbar(aes(x=scenario,ymin=ymin,ymax=ymax),
                  width=0.4, colour="orange", alpha=0.9)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=12),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=12))+
    labs(y = expression(paste('Avoided GHG emission [kt CO '[2],'eq]')))
  return(B_plot_)
}