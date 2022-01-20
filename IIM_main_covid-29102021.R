# Title     : COVID-19 Inoperability
# Objective : Evaluate impact of COVID-19 on italian national economy and environment through Inoperability and IO Tables
# Created by: plynyo & emmegamma (Github user)
# Created on: 03/06/20


#NOTATION
# _t represents the transpose of a matrix
# _i represents the inverse of a matrix/vector
# _d represents the diagonal matrix of a vector
# _n normalized matrix/vector
# _s star matrix (Inoperability formalism)
# CAPITAL LETTERS represent MATRICES
# LOW CASE LETTERS represent VECTORS
# 0 <- data directly from SIOT EUROSTAT
# 1 <- Formalism of "Inoperability Input-Output Model .. Theory and Methodology
# 2 <- test examples




###############################################################################
#########################IMPORT NECESSARY PACKAGES#############################
###############################################################################
want = c(
  "matlib","expm",
  "dplyr","stringr","tidyverse",
  "rstudioapi","RColorBrewer","colorRamps",
  "igraph","ggplot2","plotly","htmlwidgets","pheatmap",
  "eurostat","iotables",
  "knitr","xlsx",
  "patchwork"
)


have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
rm(have,want,junk)

rm(list = ls()) # Remove all objects from the workspace

###############################################################################
#########################AUTOMATICALLY SET FOLDER PATH#########################
###############################################################################
setwd(dirname(getActiveDocumentContext()$path))
#getwd()

#Folder where to save data
data_directory <- file.path(getwd(),'data')
if ( ! dir.exists(data_directory) ) dir.create (data_directory)

#Folder where to save temporary data
temporary_directory <- file.path(getwd(),'data','temp')
if ( ! dir.exists(temporary_directory) ) dir.create (temporary_directory)


###############################################################################
###################IMPORT SECONDARY FILES WITH FUNCTIONS#######################
###############################################################################
source("IIM_build_matrices.R")
source("IIM_functions.R")
source("IIM_label_functions.R")
source("IIM_plot_functions.R")




###############################################################################
###############################################################################
#######################Z, A directly from EUROSTAT naio DB#####################
#####################            METHOD 2               #######################
###############################################################################
###############################################################################

################################################
################## CASE PANDEMIC ###############
# q forced to 1 for some sectors and some time #
################## c_s = 0 #####################
################################################
# Read DPCM q from Excel
nace_file_name <- file.path(data_directory,"DPCM-mod.xlsx")
dpcm <- xlsx::read.xlsx(file = nace_file_name, sheetName = "Inoperability_NACE", colIndex = c(1:11), endRow = 67, header = TRUE, 
                        colClasses = c('character',rep(c('numeric'),10)) )
# Store the naces and remove from matrix dpcm
dpcm_naces <- dpcm[-1,1]
dpcm <- dpcm[,-1]

# a table with the time (in days) of effect of each DPCM, after t=0 (DPCM 8.3)
#dpcm_times_old <- rbind(c(0,2,4,13,16,37,57,71,99))
dpcm_dates <- as.Date(as.character(dpcm[1,]), tryFormats = "%d.%m")
dpcm_times <- matrix(0, nrow =1, ncol = length(dpcm_dates))
for (i in 2:length(dpcm_dates)) {
  dpcm_times[i] <- difftime(dpcm_dates[i], dpcm_dates[1], units = "days")
}
colnames(dpcm_times) <- colnames(dpcm)

# Setup matrix dpcm
dpcm <- dpcm[-1,]
rownames(dpcm) <- as.matrix(dpcm_naces)


tot_sector <- nrow(dpcm)

# q(0) is the first column of dpcm
q_0 <- as.matrix(dpcm[,1])
q_0 <- vector_setup(q_0,Z0_ii,tot_sector)


# Final demand is not reduced
c_s = matrix(0, tot_sector, 1)


# Compute q(infinite) from A* and c*
q_inf <- q_infinite(A1_ii_s,c_s)
q_inf <- vector_setup(q_inf,Z0_ii,tot_sector)



###############################################################################
################# INDUSTRY RESILIENCE COEFFICIENTS K ##########################
###############################################################################
#industry resilience coefficients
K <- K_create(tot_sector,0.2)
K1 <- K_create(tot_sector,0.1)
K2 <- K_create(tot_sector,0.3)

###############################################################################
################################ TIME #########################################
###############################################################################
#Number of steps
t <- 200


###############################################################################
##########NEW ATTEMPT!!!!!!! CORRECTED VERSION!!!!!############################
###############################################################################
#RATIONALE: 
#IF SECTOR CLOSE/PARTIALLY CLOSED (dpcm[l,column]!=0) SET q_disc[l] <- dpcm[l,column]
#IF SECTOR OPEN, compute dynamic depending on other closed sectors!


#TO DO LIST
#QUESTIONS/SCENARIOS TO BE ANALYZE): 
# 1) now when sector are opened, q decay from q>0 to q = 0 (or tend to 0 due to other closed sectors)
# How fast they will decay? ANALYZE DIFFERENT SCENARIOS WITH DIFFERENT K
# 2) At the same way, how fast sectors are affected by other sectors, when they 
# are open (i.e. when they move from q=0 to q>0)??
## 3) DONE - TO BE CORRECTED: IF q=1 OK to set inoperability = 1. But If 0<q<1??? 
# it's wrong to set the sector fixed to q because it is partially opened
# ---> use the q of the DPCM as the minimum value: this means that closed sectors stay closed
# IMPORTANT: this way, open sectors CANNOT improve their production over pre-pandemic values! (reasonable assumption, to be discussed)


q_disc <- t(q_0)
q_disc1 <- t(q_0)
q_disc2 <- t(q_0)
# index to select the right column in dpcm
column <- 1


for(k in 1:(t-1)){
  #q_disc <- rbind(q_disc, t(q_discrete(K,q_disc[k,],A1_ii_s,c_s) ) )
  # when t enters a new interval, change column

  if (column < dim(dpcm_times)[2] )
    if (k >= dpcm_times[column+1]) {
      column <- column + 1 
    }

  #REWRITE IT WITH MUTATE AT AND DECODE FUNCTIONS
  temp <- as.matrix(q_disc[k,])
  temp1 <- as.matrix(q_disc1[k,])  
  temp2 <- as.matrix(q_disc2[k,])  
  
  for(l in 1:nrow(dpcm)) {
   temp[l] <- max(dpcm[l,column],t(q_discrete(K,q_disc[k,],A1_ii_s,c_s))[l])
   temp1[l] <- max(dpcm[l,column],t(q_discrete(K1,q_disc1[k,],A1_ii_s,c_s))[l])
   temp2[l] <- max(dpcm[l,column],t(q_discrete(K2,q_disc2[k,],A1_ii_s,c_s))[l])
   
   }
  q_disc <- rbind(q_disc,t(temp))
  q_disc1 <- rbind(q_disc1,t(temp1))
  q_disc2 <- rbind(q_disc2,t(temp2))
  
}


#############################################################
###########################PLOT AND MANIPULATION#############
#############################################################

#HEATMAP
plot_A_heatmap(A0_ii)
plot_A_pheatmap(A0_ii,"none")




#Dyanmic of q
q_plot <- plot_qdyn(q_disc1[,],t)
q_plot




q_navigablePlot <- plot_2D(q_disc1, "Inoperability q")
q_navigablePlot

#q_plot <- plot_qdyn(q_disc)




#SET COLOR PALETTE
#colors <- colorRampPalette(brewer.pal(9,"Set1"))(16)
colors1 <- primary.colors(16, steps = 3, no.white = TRUE)
# changing yellows to more visible colors...if you like it, we can use colors1 for all plots (they're all the same anyway!)
colors1[6] <- '#67000D'
colors1[11] <- "#FEB24C"#'#FED976'

q1_plot <- plot_2D(q_disc2[,1:16], "Inoperability q", 150,colors1,TRUE)
q1_plot

#colors2 <- primary.colors(16, steps = 3, no.white = TRUE)
q2_plot <- plot_2D(q_disc2[,17:32], "Inoperability q", 150,colors1,TRUE)
q2_plot

#colors3 <- primary.colors(16, steps = 3, no.white = TRUE)
q3_plot <- plot_2D(q_disc2[,33:48], "Inoperability q", 150,colors1,TRUE)
q3_plot

#colors4 <- primary.colors(17, steps = 3, no.white = TRUE)
colors1 <- primary.colors(17, steps = 3, no.white = TRUE)
colors1[6] <- '#67000D'
colors1[11] <- "#FEB24C"#'#FED976'
q4_plot <- plot_2D(q_disc2[,49:65], "Inoperability q", 150,colors1,TRUE)
q4_plot


saveWidget(q_plot,
           file.path(paste0(getwd(),"/graph/"),"q_DPCM_new.html"),
           selfcontained = TRUE,libdir = "lib")


#3D plot of the dynamic of q
q_plot3D <- plot_qdyn3D(q_disc)
q_plot3D
saveWidget(q_plot3D,
           file.path(paste0(getwd(),"/graph/"),"q_DPCM_3D.html"),
           selfcontained = TRUE,libdir = "lib")


#Save data to xlsx
q_disc_file <- file.path(getwd(),'data', 'temp', "q_disc.xlsx")
xlsx::write.xlsx ( q_disc, file = q_disc_file, sheetName = "q_disc",
                   col.names=TRUE, row.names=TRUE, append=FALSE)
xlsx::write.xlsx ( dpcm, file = q_disc_file, sheetName = "dpcm",
                   col.names=TRUE, row.names=TRUE, append=TRUE)
xlsx::write.xlsx ( dpcm_times, file = q_disc_file, sheetName = "dpcm_times",
                   col.names=TRUE, row.names=TRUE, append=TRUE)




#PERCENTAGE OF CLOSING TIME OVER THE TOTAL
q_perc_med <- matrix(NA,length(q_disc[1,]),1)
q_perc <- matrix(NA,length(q_disc[1,]),3)

for(k in 1:tot_sector){
  q_perc_med[k] <- sum(q_disc[,k])/t
  q_perc[k,1] <- sum(q_disc[,k])/t
  q_perc[k,2] <- sum(q_disc1[,k])/t
  q_perc[k,3] <- sum(q_disc2[,k])/t
}
rownames(q_perc) <- colnames(q_disc)
rownames(q_perc_med) <- colnames(q_disc)

q_perc_plot <- plot_qperc_hist(q_perc_med)
q_perc_plot

#with error bar
q_perc_err <- data.frame(
  NACE=matrix(NA,length(q_disc[1,]),1),
  ymed=matrix(NA,length(q_disc[1,]),1),
  ymin=matrix(NA,length(q_disc[1,]),1),
  ymax=matrix(NA,length(q_disc[1,]),1)
)

for(k in 1:tot_sector){
  q_perc_err['ymed'][k,] <- q_perc[k,1]
  q_perc_err['ymin'][k,] <- q_perc[k,2]
  q_perc_err['ymax'][k,] <- q_perc[k,3]
  q_perc_err['NACE'][k,] <- rownames(q_perc)[k]
}

q_perc_plot <- plot_qperc_hist_with_errorbar(q_perc_err)
q_perc_plot


#ECONOMIC LOSS - STEP BY STEP
Q_dyn <- economic_loss(x1,q_disc)
#Q_plot <- plot_Qdyn(Q_dyn)
Q_plot <- plot_2D(Q_dyn, "Daily economic loss (mln €)")
Q_plot

#CUMULATIVE ECONOMIC LOSS - STEP BY STEP
Q_dyn_cum <- cumulative_economic_loss(x1,q_disc)
#Q_cum_plot <- plot_Qdyn_cumulative(Q_dyn_cum)
Q_cum_plot <- plot_2D(Q_dyn_cum, "Cumulative economic loss (mln €)")
Q_cum_plot
saveWidget(Q_cum_plot,
           file.path(paste0(getwd(),"/graph/"),"q_loss_cum.html"),
           selfcontained = TRUE,libdir = "lib")


#TOTAL ECONOMIC LOSS
Q_tot <- total_economic_loss(x1,q_disc)
Q_tot_plot <- plot_QTot_hist(Q_tot)
Q_tot_plot


#with error bar
Q_tot1 <- total_economic_loss(x1,q_disc1)
Q_tot2 <- total_economic_loss(x1,q_disc2)
Q_tot_err <- data.frame(
  NACE=matrix(NA,length(q_disc[1,]),1),
  ymed=matrix(NA,length(q_disc[1,]),1),
  ymin=matrix(NA,length(q_disc[1,]),1),
  ymax=matrix(NA,length(q_disc[1,]),1)
)

for(k in 1:tot_sector){
  Q_tot_err['ymed'][k,] <- Q_tot[k]
  Q_tot_err['ymin'][k,] <- Q_tot1[k]
  Q_tot_err['ymax'][k,] <- Q_tot2[k]
  Q_tot_err['NACE'][k,] <- rownames(Q_tot)[k]
}

Q_tot_plot_err <- plot_QTot_hist_with_errorbar(Q_tot_err)
Q_tot_plot_err
###############################################################################
############################PREPARE AND SETUP B################################
#####################ENVIRONMeNTALLY EXTENDED IO TABLE#########################
###############################################################################
#ExPLANATION
#https://ec.europa.eu/eurostat/cache/metadata/en/env_ac_ainah_r2_esms.htm
#The air emissions [env_ac_ainah_r2] and the bridging items [env_ac_aibrid_r2] are presented in tonnes and thousand tonnes, as well as grams per capita and kilograms per capita.
#Air emissions intensities [env_ac_aeint_r2] are presented in grams per euro and kilograms per euro.

#Search for keywords within Eurostat databases
string_to_search <- "Air emissions accounts by NACE" #id = env_ac_ainah_r2
#string_to_search <- "Air emissions intensities by NACE" #id = env_ac_aeint_r2

search_eurostat(string_to_search)$title

#GET ID OF DATASET and DOWNLOAD DATA
id <- search_eurostat(string_to_search)$code[1]
if ( ! file.exists(file.path(getwd(),'data','temp','aea.rda')) )
  aea <- get_eurostat(id, time_format = "num")
#aea <- get_eurostat_data("env_ac_ainah_r2")

#########################################################
#save/load data
save (aea, file = file.path(getwd(),'data','temp','aea.rda'))
#load (file = file.path(getwd(),'data','temp','aea.rda') )
#########################################################


#FILTER DATA FOR ITALY, YEAR AND UNIT
#unit = THS_T (Thousand tonnes); T (Tonnes)
aea_IT <- subset(aea, geo == "IT" & time == 2015 & unit == "THS_T")
aea_IT_2015 <- subset(aea, geo == "IT" & time == 2015 & unit == "THS_T")
aea_IT_2018 <- subset(aea, geo == "IT" & time == 2018 & unit == "THS_T")


#FILTER AND ORDER INDUSTRIAL SECTORS ACCORDING TO A 
B <- B_setup(aea_IT,A0_ii)
B_2015 <- B_setup(aea_IT_2015,A0_ii)
B_2018 <- B_setup(aea_IT_2018,A0_ii)
B
B_2015
B_2018
tail(B)
###############################################################################
###################COMPUTE AIR EMISSION DYNAMIC################################
###############################################################################
#Remove sector L68B from q because of in B it doesn't exist!
q_IT <- q_disc[ , -which(colnames(q_disc) %in% c("L68B"))]
q_IT1 <- q_disc1[ , -which(colnames(q_disc) %in% c("L68B"))]
q_IT2 <- q_disc2[ , -which(colnames(q_disc) %in% c("L68B"))]


#2015
#1: "ACG" - 2: "CH4" - 3: "CH4_CO2E" - 4: "CH4_NMVOCE" - 5: "CO" - 6: "CO2" - 
#7: "CO2_BIO" - 8: "CO_NMVOCE" - 9: "GHG" - 10: "HFC_CO2E" - 11: "N2O" - 
#12: "N2O_CO2E" - 13: "NF3_SF6_CO2E" - 14: "NH3" - 15: "NH3_SO2E"   
j <- 9 #it corresponds to B rows - B[j,] 


#2018
#1: "CH4" - 2: "CH4_CO2E" - 3: "CH4_NMVOCE" - 4: "CO2" - 5: "GHG"         
#6: "HFC_CO2E" - 7: "N2O" - 8: "N2O_CO2E" - 9: "NF3_SF6_CO2E" - 10: "PFC_CO2E"    
#j <- 5 #it corresponds to B rows - B[j,] 

#Air Emissions reduced (Avoided emissions)
#*********** TO BE CHECKED *************
ae_red <- q_IT #initialize ae with same dim of q_IT
ae_red1 <- q_IT1
ae_red2 <- q_IT2

for(i in 1:length(q_IT[1,])){
  ae_red[,i] <- q_IT[,i]*B[j,i]/365
  ae_red1[,i] <- q_IT1[,i]*B[j,i]/365
  ae_red2[,i] <- q_IT2[,i]*B[j,i]/365
}

ae_red_plot <- plot_2D(ae_red, "Avoided air emissions (units??)")
ae_red_plot


#air emissions total by sector
ae <- q_IT #initialize ae with same dim of q_IT
ae1 <- q_IT1
ae2 <- q_IT2
for(i in 1:length(q_IT[1,])){
  ae[,i] <- (1-q_IT[,i])*B[j,i]/365
  ae1[,i] <- (1-q_IT1[,i])*B[j,i]/365
  ae2[,i] <- (1-q_IT2[,i])*B[j,i]/365
}
ae_plot <- plot_2D(ae, "Total air emissions (units??)")
ae_plot


#TOTAL OVER THE PERIOD
ae_tot <- ae[1,]
ae_tot1 <- ae1[1,]
ae_tot2 <- ae2[1,]

for(i in 1:length(ae[1,])){
  ae_tot[i] <- sum(ae[,i]) 
  ae_tot1[i] <- sum(ae1[,i]) 
  ae_tot2[i] <- sum(ae2[,i]) 
}

ae_tot_plot <- plot_QTot_hist(as.matrix(ae_tot))
ae_tot_plot



ae_red_tot <- ae_red[1,]
ae_red_tot1 <- ae_red1[1,]
ae_red_tot2 <- ae_red2[1,]

for(i in 1:length(ae_red[1,])){
  ae_red_tot[i] <- sum(ae_red[,i]) 
  ae_red_tot1[i] <- sum(ae_red1[,i]) 
  ae_red_tot2[i] <- sum(ae_red2[,i])
}

ae_red_tot_plot <- plot_QTot_hist(as.matrix(ae_red_tot))
ae_red_tot_plot

# Histograms with error bars for k

AE_tot_err <- data.frame(
  NACE=matrix(NA,length(q_disc[1,]),1),
  ymed=matrix(NA,length(q_disc[1,]),1),
  ymin=matrix(NA,length(q_disc[1,]),1),
  ymax=matrix(NA,length(q_disc[1,]),1)
)

for(k in 1:tot_sector){
  AE_tot_err['ymed'][k,] <- ae_tot[k]
  AE_tot_err['ymin'][k,] <- ae_tot1[k]
  AE_tot_err['ymax'][k,] <- ae_tot2[k]
  AE_tot_err['NACE'][k,] <- rownames(Q_tot)[k]
}
AE_tot_err
Q_tot_plot_err <- plot_QTot_hist_with_errorbar(AE_tot_err,expression(paste('Avoided GHG emission [kt CO '[2],'eq]')))
Q_tot_plot_err



AE_red_tot_err <- data.frame(
  NACE=matrix(NA,length(q_disc[1,]),1),
  ymed=matrix(NA,length(q_disc[1,]),1),
  ymin=matrix(NA,length(q_disc[1,]),1),
  ymax=matrix(NA,length(q_disc[1,]),1)
)

for(k in 1:tot_sector){
  AE_red_tot_err['ymed'][k,] <- ae_red_tot[k]
  AE_red_tot_err['ymin'][k,] <- ae_red_tot1[k]
  AE_red_tot_err['ymax'][k,] <- ae_red_tot2[k]
  AE_red_tot_err['NACE'][k,] <- rownames(Q_tot)[k]
}
AE_red_tot_err
Q_red_tot_plot_err <- plot_QTot_hist_with_errorbar(AE_red_tot_err,expression(paste('Avoided GHG emission [kt CO '[2],'eq]')))
Q_red_tot_plot_err

###############################################################################
############ Calculate reduction in % to compare with Copernicus ##############
###############################################################################
t_cop <- 145 # as.integer(round(difftime("2020-07-31", dpcm_dates[1], units = "days")))

#1: "ACG" - 2: "CH4" - 3: "CH4_CO2E" - 4: "CH4_NMVOCE" - 5: "CO" - 6: "CO2" - 
#7: "CO2_BIO" - 8: "CO_NMVOCE" - 9: "GHG" - 10: "HFC_CO2E" - 11: "N2O" - 
#12: "N2O_CO2E" - 13: "NF3_SF6_CO2E" - 14: "NH3" - 15: "NH3_SO2E"   
j <- 7 #it corresponds to B rows - B[j,] 

ae_cop_red <- q_IT[1:t_cop,] #initialize ae_cop with same dim of q_IT
ae_cop_red1 <- q_IT1[1:t_cop,]
ae_cop_red2 <- q_IT2[1:t_cop,]

for(i in 1:length(q_IT[1,])){
  ae_cop_red[,i] <- q_IT[1:t_cop,i]*B[j,i]/365
  ae_cop_red1[,i] <- q_IT1[1:t_cop,i]*B[j,i]/365
  ae_cop_red2[,i] <- q_IT2[1:t_cop,i]*B[j,i]/365
}

# ae_cop_red_plot <- plot_2D(ae_cop_red, "Avoided air emissions (units??)")
# ae_cop_red_plot


#air emissions total by sector
ae_cop <- q_IT[1:t_cop,] #initialize ae_cop with same dim of q_IT
ae_cop1 <- q_IT1[1:t_cop,]
ae_cop2 <- q_IT2[1:t_cop,]
for(i in 1:length(q_IT[1,])){
  ae_cop[,i] <- (1-q_IT[1:t_cop,i])*B[j,i]/365
  ae_cop1[,i] <- (1-q_IT1[1:t_cop,i])*B[j,i]/365
  ae_cop2[,i] <- (1-q_IT2[1:t_cop,i])*B[j,i]/365
}
# ae_cop_plot <- plot_2D(ae_cop, "Total air emissions (units??)")
# ae_cop_plot

ae_cop_plot_SUM <- plot_2D(ae_cop+ae_cop_red, "Planned air emissions")
ae_cop_plot_SUM

#TOTAL OVER THE PERIOD
ae_cop_tot <- ae_cop[1,]
ae_cop_tot1 <- ae_cop1[1,]
ae_cop_tot2 <- ae_cop2[1,]

for(i in 1:length(ae_cop[1,])){
  ae_cop_tot[i] <- sum(ae_cop[,i]) 
  ae_cop_tot1[i] <- sum(ae_cop1[,i]) 
  ae_cop_tot2[i] <- sum(ae_cop2[,i]) 
}

# ae_cop_tot_plot <- plot_QTot_hist(as.matrix(ae_cop_tot))
# ae_cop_tot_plot



ae_cop_red_tot <- ae_cop_red[1,]
ae_cop_red_tot1 <- ae_cop_red1[1,]
ae_cop_red_tot2 <- ae_cop_red2[1,]

for(i in 1:length(ae_cop_red[1,])){
  ae_cop_red_tot[i] <- sum(ae_cop_red[,i]) 
  ae_cop_red_tot1[i] <- sum(ae_cop_red1[,i]) 
  ae_cop_red_tot2[i] <- sum(ae_cop_red2[,i])
}

# ae_cop_red_tot_plot <- plot_QTot_hist(as.matrix(ae_cop_red_tot))
# ae_cop_red_tot_plot

# Calculate % reduction
# cols_industries <- which(startsWith(names(ae_cop_tot),"C"))
# CO_reduced <- sum(ae_cop_red_tot[cols_industries])
# CO_tot <- sum(ae_cop_tot[cols_industries])
# CO_perc_reduction <- CO_reduced/CO_usual*100

 # Histograms with error bars for k
 
 AE_cop_tot_err <- data.frame(
   NACE=matrix(NA,length(q_disc[1,]),1),
   ymed=matrix(NA,length(q_disc[1,]),1),
   ymin=matrix(NA,length(q_disc[1,]),1),
   ymax=matrix(NA,length(q_disc[1,]),1)
 )
 
 for(k in 1:tot_sector){
   AE_cop_tot_err['ymed'][k,] <- ae_cop_tot[k]
   AE_cop_tot_err['ymin'][k,] <- ae_cop_tot1[k]
   AE_cop_tot_err['ymax'][k,] <- ae_cop_tot2[k]
   AE_cop_tot_err['NACE'][k,] <- rownames(Q_tot)[k]
 }
 # AE_cop_tot_err
 # Q_tot_plot_err <- plot_QTot_hist_with_errorbar(AE_cop_tot_err,expression(paste('Avoided GHG emission [kt CO '[2],'eq]')))
 # Q_tot_plot_err
 
 AE_cop_red_tot_err <- data.frame(
   NACE=matrix(NA,length(q_disc[1,]),1),
   ymed=matrix(NA,length(q_disc[1,]),1),
   ymin=matrix(NA,length(q_disc[1,]),1),
   ymax=matrix(NA,length(q_disc[1,]),1)
 )
 
 for(k in 1:tot_sector){
   AE_cop_red_tot_err['ymed'][k,] <- ae_cop_red_tot[k]
   AE_cop_red_tot_err['ymin'][k,] <- ae_cop_red_tot1[k]
   AE_cop_red_tot_err['ymax'][k,] <- ae_cop_red_tot2[k]
   AE_cop_red_tot_err['NACE'][k,] <- rownames(Q_tot)[k]
 }
 # AE_cop_red_tot_err
 # Q_red_tot_plot_err <- plot_QTot_hist_with_errorbar(AE_cop_red_tot_err,expression(paste('Avoided GHG emission [kt CO '[2],'eq]')))
 # Q_red_tot_plot_err

# Calculate % reduction for different k's
# filter only C group
cols_industries <- which(startsWith(names(ae_cop_tot),"C"))
AE_CO_reduced <- dplyr::filter(AE_cop_red_tot_err,startsWith(NACE,"C"))
AE_CO <- dplyr::filter(AE_cop_tot_err,startsWith(NACE,"C"))
# sum over ymed, ymin, and ymax
AE_CO_reduced_tot <- c(sum(select(AE_CO_reduced,"ymed")),sum(select(AE_CO_reduced,"ymin")),sum(select(AE_CO_reduced,"ymax")))
AE_CO_tot <- c(sum(select(AE_CO,"ymed")),sum(select(AE_CO,"ymin")),sum(select(AE_CO,"ymax")))

# calculate reduction (3 values: ymed, ymin, ymax)
AE_CO_planned <- B[j,cols_industries]/365*length(ae_cop[,1]) # length(..) should be the same as t_cop
AE_CO_planned_tot <- sum(AE_CO_planned)

AE_CO_reduction_perc <- AE_CO_reduced_tot / AE_CO_planned_tot*100
AE_CO_covid_perc <- AE_CO_tot / AE_CO_planned_tot*100
AE_CO_reduction_perc + AE_CO_covid_perc
AE_CO_reduction_perc

# CO: (3 values: ymed, ymin, ymax)
# for t=145 (up to 31st july 2020): 35.21802 43.83843 32.17497
# for t=200 (original): 25.55077 32.17869 23.33808
# Copernicus (see xlsx): -26.7904109589041

# CH4:        16.37842 19.22863 15.35074 ( t= 145)
# CH4_CO2E:   16.37842 19.22863 15.35074
# CH4_NMVOCE: 16.37843 19.22866 15.35075
# they're all the same!
# t=200 : 11.88727 14.17306 11.13834
# Copernicus (CH4, see xlsx): -19.6301369863014

# CO2:     25.28126 30.51908 23.43373
# CO2_BIO: 15.31942 17.94696 14.39481
# Copernicus:
# CO2_ff	-24.3856164383561
# CO2_bf	-17.8904109589041



############################################################
#########################SCENARIOS OLD#########################
############################################################
# Read DPCM q from Excel
nace_file_name <- file.path(data_directory,"DPCM-mod2.xlsx")
scenarios <- xlsx::read.xlsx(file = nace_file_name, sheetName = "Scenarios", colIndex = c(1:6), endRow = 66, header = TRUE, 
                        colClasses = c('character',rep(c('numeric'),10)) )
scenarios
# Store the naces and remove from matrix dpcm
naces <- scenarios[,1]
scenarios <- scenarios[,-1]
rownames(scenarios) <- as.matrix(naces)
t1 <- 100


#1-5 : scenarios - a,b -> different K
q1 <- as.matrix(scenarios[,1]);
rownames(q1) <- as.matrix(naces);
q1 <- t(q1)
q1a <- q1
q1b <- q1

q2 <- as.matrix(scenarios[,2]);
rownames(q2) <- as.matrix(naces);
q2 <- t(q2)
q2a <- q2
q2b <-q2

q3 <- as.matrix(scenarios[,3]);
rownames(q3) <- as.matrix(naces);
q3 <- t(q3)
q3a <- q3
q3b <-q3

q4 <- as.matrix(scenarios[,4]);
rownames(q4) <- as.matrix(naces);
q4 <- t(q4)
q4a <- q4
q4b <- q4

q5 <- as.matrix(scenarios[,5]);
rownames(q5) <- as.matrix(naces);
q5 <- t(q5)
q5a <- q5
q5b <- q5

for(k in 1:(t1-1)){
  q_temp1 <- as.matrix(q1[k,])
  q_temp1a <- as.matrix(q1a[k,])
  q_temp1b <- as.matrix(q1b[k,])

  q_temp2 <- as.matrix(q2[k,])  
  q_temp2a <- as.matrix(q2a[k,])  
  q_temp2b <- as.matrix(q2b[k,])  
    
  q_temp3 <- as.matrix(q3[k,])  
  q_temp3a <- as.matrix(q3a[k,])  
  q_temp3b <- as.matrix(q3b[k,])  

  q_temp4 <- as.matrix(q4[k,])  
  q_temp4a <- as.matrix(q4a[k,])  
  q_temp4b <- as.matrix(q4b[k,])  

  q_temp5 <- as.matrix(q5[k,])  
  q_temp5a <- as.matrix(q5a[k,])  
  q_temp5b <- as.matrix(q5b[k,])  
  
  for(l in 1:nrow(scenarios)) {
    if(k<30){
      q_temp1[l] <- max(scenarios[l,1],t(q_discrete(K,q1[k,],A1_ii_s,c_s))[l])
      q_temp1a[l] <- max(scenarios[l,1],t(q_discrete(K1,q1a[k,],A1_ii_s,c_s))[l])
      q_temp1b[l] <- max(scenarios[l,1],t(q_discrete(K2,q1b[k,],A1_ii_s,c_s))[l])
      q_temp2[l] <- max(scenarios[l,2],t(q_discrete(K,q2[k,],A1_ii_s,c_s))[l])
      q_temp2a[l] <- max(scenarios[l,2],t(q_discrete(K1,q2a[k,],A1_ii_s,c_s))[l])
      q_temp2b[l] <- max(scenarios[l,2],t(q_discrete(K2,q2b[k,],A1_ii_s,c_s))[l])
      q_temp3[l] <- max(scenarios[l,3],t(q_discrete(K,q3[k,],A1_ii_s,c_s))[l])
      q_temp3a[l] <- max(scenarios[l,3],t(q_discrete(K1,q3a[k,],A1_ii_s,c_s))[l])
      q_temp3b[l] <- max(scenarios[l,3],t(q_discrete(K2,q3b[k,],A1_ii_s,c_s))[l])
      q_temp4[l] <- max(scenarios[l,4],t(q_discrete(K,q4[k,],A1_ii_s,c_s))[l])
      q_temp4a[l] <- max(scenarios[l,4],t(q_discrete(K1,q4a[k,],A1_ii_s,c_s))[l])
      q_temp4b[l] <- max(scenarios[l,4],t(q_discrete(K2,q4b[k,],A1_ii_s,c_s))[l])
      q_temp5[l] <- max(scenarios[l,5],t(q_discrete(K,q5[k,],A1_ii_s,c_s))[l])
      q_temp5a[l] <- max(scenarios[l,5],t(q_discrete(K1,q5a[k,],A1_ii_s,c_s))[l])
      q_temp5b[l] <- max(scenarios[l,5],t(q_discrete(K2,q5b[k,],A1_ii_s,c_s))[l])
          }else{
      q_temp1[l] <- t(q_discrete(K,q1[k,],A1_ii_s,c_s))[l]
      q_temp1a[l] <- t(q_discrete(K1,q1a[k,],A1_ii_s,c_s))[l]
      q_temp1b[l] <- t(q_discrete(K2,q1b[k,],A1_ii_s,c_s))[l]
      q_temp2[l] <- t(q_discrete(K,q2[k,],A1_ii_s,c_s))[l]
      q_temp2a[l] <- t(q_discrete(K1,q2a[k,],A1_ii_s,c_s))[l]
      q_temp2b[l] <- t(q_discrete(K2,q2b[k,],A1_ii_s,c_s))[l]
      q_temp3[l] <- t(q_discrete(K,q3[k,],A1_ii_s,c_s))[l]
      q_temp3a[l] <- t(q_discrete(K1,q3a[k,],A1_ii_s,c_s))[l]
      q_temp3b[l] <- t(q_discrete(K2,q3b[k,],A1_ii_s,c_s))[l]
      q_temp4[l] <- t(q_discrete(K,q4[k,],A1_ii_s,c_s))[l]
      q_temp4a[l] <- t(q_discrete(K1,q4a[k,],A1_ii_s,c_s))[l]
      q_temp4b[l] <- t(q_discrete(K2,q4b[k,],A1_ii_s,c_s))[l]
      q_temp5[l] <- t(q_discrete(K,q5[k,],A1_ii_s,c_s))[l]
      q_temp5a[l] <- t(q_discrete(K1,q5a[k,],A1_ii_s,c_s))[l]
      q_temp5b[l] <- t(q_discrete(K2,q5b[k,],A1_ii_s,c_s))[l]
    }
    
  }
  q1 <- rbind(q1,t(q_temp1))
  q1a <- rbind(q1a,t(q_temp1a))
  q1b <- rbind(q1b,t(q_temp1b))
  q2 <- rbind(q2,t(q_temp2))
  q2a <- rbind(q2a,t(q_temp2a))
  q2b <- rbind(q2b,t(q_temp2b))
  q3 <- rbind(q3,t(q_temp3))
  q3a <- rbind(q3a,t(q_temp3a))
  q3b <- rbind(q3b,t(q_temp3b))
  q4 <- rbind(q4,t(q_temp4))
  q4a <- rbind(q4a,t(q_temp4a))
  q4b <- rbind(q4b,t(q_temp4b))
  q5 <- rbind(q5,t(q_temp5))
  q5a <- rbind(q5a,t(q_temp5a))
  q5b <- rbind(q5b,t(q_temp5b))
}

rownames(q1) <- seq(1:t1)
rownames(q2) <- seq(1:t1)
rownames(q3) <- seq(1:t1)
rownames(q4) <- seq(1:t1)
rownames(q5) <- seq(1:t1)

#Dynamic of q
q1_plot <- plot_qdyn(q4[,],100)
q1_plot

#q_plot <- plot_qdyn(q_disc)
q1_plot <- plot_2D(q2[,], "Inoperability q",100,-1,FALSE)
q1_plot


scen1 <- plot_qdyn3D_mod(q1)
scen2 <- plot_qdyn3D_mod(q2)
scen3 <- plot_qdyn3D_mod(q3)
scen4 <- plot_qdyn3D_mod(q4)
scen5 <- plot_qdyn3D_mod(q5)

#q5_plot <- plot_2D(q5[,], "Inoperability q",100,-1,TRUE)

saveWidget(scen1,
           file.path(paste0(getwd(),"/graph/"),"scenario1.html"),
           selfcontained = TRUE,libdir = "lib")
saveWidget(scen2,
           file.path(paste0(getwd(),"/graph/"),"scenario2.html"),
           selfcontained = TRUE,libdir = "lib")
saveWidget(scen3,
           file.path(paste0(getwd(),"/graph/"),"scenario3.html"),
           selfcontained = TRUE,libdir = "lib")
saveWidget(scen4,
           file.path(paste0(getwd(),"/graph/"),"scenario4.html"),
           selfcontained = TRUE,libdir = "lib")
saveWidget(scen5,
           file.path(paste0(getwd(),"/graph/"),"scenario5.html"),
           selfcontained = TRUE,libdir = "lib")


Q_scenario <- data.frame(
  scenario=matrix(NA,length(scenarios),1),
  ymed=matrix(NA,length(scenarios),1),
  ymin=matrix(NA,length(scenarios),1),
  ymax=matrix(NA,length(scenarios),1)
)

Q_scenario['scenario'][1,] <- 'Scenario 1'
Q_scenario['ymed'][1,] <- sum(total_economic_loss(x1,q1))
Q_scenario['ymin'][1,] <- sum(total_economic_loss(x1,q1a))
Q_scenario['ymax'][1,] <- sum(total_economic_loss(x1,q1b))

Q_scenario['scenario'][2,] <- 'Scenario 2'
Q_scenario['ymed'][2,] <- sum(total_economic_loss(x1,q2))
Q_scenario['ymin'][2,] <- sum(total_economic_loss(x1,q2a))
Q_scenario['ymax'][2,] <- sum(total_economic_loss(x1,q2b))

Q_scenario['scenario'][3,] <- 'Scenario 3'
Q_scenario['ymed'][3,] <- sum(total_economic_loss(x1,q3))
Q_scenario['ymin'][3,] <- sum(total_economic_loss(x1,q3a))
Q_scenario['ymax'][3,] <- sum(total_economic_loss(x1,q3b))

Q_scenario['scenario'][4,] <- 'Scenario 4'
Q_scenario['ymed'][4,] <- sum(total_economic_loss(x1,q4))
Q_scenario['ymin'][4,] <- sum(total_economic_loss(x1,q4a))
Q_scenario['ymax'][4,] <- sum(total_economic_loss(x1,q4b))

Q_scenario['scenario'][5,] <- 'Scenario 5'
Q_scenario['ymed'][5,] <- sum(total_economic_loss(x1,q5))
Q_scenario['ymin'][5,] <- sum(total_economic_loss(x1,q5a))
Q_scenario['ymax'][5,] <- sum(total_economic_loss(x1,q5b))



Q_plot_scenario <- ggplot(data=Q_scenario)+
  geom_bar(aes(x=scenario, y=ymed),stat="identity")+
  geom_errorbar(aes(x=scenario,ymin=ymin,ymax=ymax),
                width=0.4, colour="orange", alpha=0.9)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=12),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12))+
  labs(y = "Economic losses [mln euro]")
Q_plot_scenario




#AIR EMISSION REDUCTION FOR SCENARIOS
q1_ <- q1[ , -which(colnames(q_disc) %in% c("L68B"))]
q1a_ <- q1a[ , -which(colnames(q_disc) %in% c("L68B"))]
q1b_ <- q1b[ , -which(colnames(q_disc) %in% c("L68B"))]
q2_ <- q2[ , -which(colnames(q_disc) %in% c("L68B"))]
q2a_ <- q2a[ , -which(colnames(q_disc) %in% c("L68B"))]
q2b_ <- q2b[ , -which(colnames(q_disc) %in% c("L68B"))]
q3_ <- q3[ , -which(colnames(q_disc) %in% c("L68B"))]
q3a_ <- q3a[ , -which(colnames(q_disc) %in% c("L68B"))]
q3b_ <- q3b[ , -which(colnames(q_disc) %in% c("L68B"))]
q4_ <- q4[ , -which(colnames(q_disc) %in% c("L68B"))]
q4a_ <- q4a[ , -which(colnames(q_disc) %in% c("L68B"))]
q4b_ <- q4b[ , -which(colnames(q_disc) %in% c("L68B"))]
q5_ <- q5[ , -which(colnames(q_disc) %in% c("L68B"))]
q5a_ <- q5a[ , -which(colnames(q_disc) %in% c("L68B"))]
q5b_ <- q5b[ , -which(colnames(q_disc) %in% c("L68B"))]

#2015
#1: "ACG" - 2: "CH4" - 3: "CH4_CO2E" - 4: "CH4_NMVOCE" - 5: "CO" - 6: "CO2" - 
#7: "CO2_BIO" - 8: "CO_NMVOCE" - 9: "GHG" - 10: "HFC_CO2E" - 11: "N2O" - 
#12: "N2O_CO2E" - 13: "NF3_SF6_CO2E" - 14: "NH3" - 15: "NH3_SO2E"   
j <- 9 #it corresponds to B rows - B[j,] 


red1_ <- q1[ , -which(colnames(q_disc) %in% c("L68B"))]
red1a_ <- q1a[ , -which(colnames(q_disc) %in% c("L68B"))]
red1b_ <- q1b[ , -which(colnames(q_disc) %in% c("L68B"))]
red2_ <- q2[ , -which(colnames(q_disc) %in% c("L68B"))]
red2a_ <- q2a[ , -which(colnames(q_disc) %in% c("L68B"))]
red2b_ <- q2b[ , -which(colnames(q_disc) %in% c("L68B"))]
red3_ <- q3[ , -which(colnames(q_disc) %in% c("L68B"))]
red3a_ <- q3a[ , -which(colnames(q_disc) %in% c("L68B"))]
red3b_ <- q3b[ , -which(colnames(q_disc) %in% c("L68B"))]
red4_ <- q4[ , -which(colnames(q_disc) %in% c("L68B"))]
red4a_ <- q4a[ , -which(colnames(q_disc) %in% c("L68B"))]
red4b_ <- q4b[ , -which(colnames(q_disc) %in% c("L68B"))]
red5_ <- q5[ , -which(colnames(q_disc) %in% c("L68B"))]
red5a_ <- q5a[ , -which(colnames(q_disc) %in% c("L68B"))]
red5b_ <- q5b[ , -which(colnames(q_disc) %in% c("L68B"))]



for(i in 1:length(q1_[1,])){
  red1_[,i] <- q1_[,i]*B[j,i]/365
  red1a_[,i] <- q1a_[,i]*B[j,i]/365
  red1b_[,i] <- q1b_[,i]*B[j,i]/365
  red2_[,i] <- q2_[,i]*B[j,i]/365
  red2a_[,i] <- q2a_[,i]*B[j,i]/365
  red2b_[,i] <- q2b_[,i]*B[j,i]/365
  red3_[,i] <- q3_[,i]*B[j,i]/365
  red3a_[,i] <- q3a_[,i]*B[j,i]/365
  red3b_[,i] <- q3b_[,i]*B[j,i]/365
  red4_[,i] <- q4_[,i]*B[j,i]/365
  red4a_[,i] <- q4a_[,i]*B[j,i]/365
  red4b_[,i] <- q4b_[,i]*B[j,i]/365
  red5_[,i] <- q5_[,i]*B[j,i]/365
  red5a_[,i] <- q5a_[,i]*B[j,i]/365
  red5b_[,i] <- q5b_[,i]*B[j,i]/365
}




B_scenario <- data.frame(
  scenario=matrix(NA,length(scenarios),1),
  ymed=matrix(NA,length(scenarios),1),
  ymin=matrix(NA,length(scenarios),1),
  ymax=matrix(NA,length(scenarios),1)
)

B_scenario['scenario'][1,] <- 'Scenario 1'
B_scenario['ymed'][1,] <- sum(red1_)
B_scenario['ymin'][1,] <- sum(red1a_)
B_scenario['ymax'][1,] <- sum(red1b_)

B_scenario['scenario'][2,] <- 'Scenario 2'
B_scenario['ymed'][2,] <- sum(red2_)
B_scenario['ymin'][2,] <- sum(red2a_)
B_scenario['ymax'][2,] <- sum(red2b_)

B_scenario['scenario'][3,] <- 'Scenario 3'
B_scenario['ymed'][3,] <- sum(red3_)
B_scenario['ymin'][3,] <- sum(red3a_)
B_scenario['ymax'][3,] <- sum(red3b_)

B_scenario['scenario'][4,] <- 'Scenario 4'
B_scenario['ymed'][4,] <- sum(red4_)
B_scenario['ymin'][4,] <- sum(red4a_)
B_scenario['ymax'][4,] <- sum(red4b_)

B_scenario['scenario'][5,] <- 'Scenario 5'
B_scenario['ymed'][5,] <- sum(red5_)
B_scenario['ymin'][5,] <- sum(red5a_)
B_scenario['ymax'][5,] <- sum(red5b_)



B_plot_scenario <- ggplot(data=B_scenario)+
  geom_bar(aes(x=scenario, y=ymed),stat="identity")+
  geom_errorbar(aes(x=scenario,ymin=ymin,ymax=ymax),
                width=0.4, colour="orange", alpha=0.9)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=12),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12))+
  labs(y = expression(paste('Avoided GHG emission [kt CO '[2],'eq]')))
B_plot_scenario


Q_plot_scenario + B_plot_scenario







##################################################
##########SIMULATION ON SCENARIOS#################
##################################################
# Read DPCM q from Excel
nace_file_name <- file.path(data_directory,"DPCM-mod2.xlsx")
scenarios2 <- xlsx::read.xlsx(file = nace_file_name, sheetName = "Scenarios2", colIndex = c(1:6), endRow = 66, header = TRUE, 
                             colClasses = c('character',rep(c('numeric'),10)) )
scenarios2

#initialize single sectors closed
naces <- scenarios2[,1]
scenarios2 <- scenarios2[,-1]
rownames(scenarios2) <- as.matrix(naces)

#initialize permutations for two, three, four closed sectors.
scen_1 <- scenarios2
scen_2 <- DF_two(scen_1)
scen_3 <- DF_three(scen_1)
scen_4 <- DF_all(scen_1)


#Set time steps
t1 <- 100


#INITIALIZE DF for each case
DF_1 <- init_scen(scen_1)
DF_2 <- init_scen(scen_2)
DF_3 <- init_scen(scen_3)
DF_4 <- init_scen(scen_4)

#COMPUTE DYNAMIC FOR EACH CASE
DF_1 <- scen_dynamic(DF_1,scen_1) 
DF_2 <- scen_dynamic(DF_2,scen_2) 
DF_3 <- scen_dynamic(DF_3,scen_3) 
DF_4 <- scen_dynamic(DF_4,scen_4) 


#SEE RESULTS OF THE DYNAMICS
plot1 <- plot_qdyn(DF_1$q[[1]][,],100)
plot1

plot2 <- plot_2D(DF_1$q[[1]][,], "Inoperability q",100,-1,FALSE)
plot2

plot3 <- plot_qdyn3D_mod(DF_1$q[[1]])
plot3


#COMPUTE ECONOMIC LOSSES
Q_1 <- Q_scen(DF_1,scen_1)
Q_2 <- Q_scen(DF_2,scen_2)
Q_3 <- Q_scen(DF_3,scen_3)
Q_4 <- Q_scen(DF_4,scen_4)

#PLOT TOTAL ECONOMIC LOSSES
Q_plot1 <- Q_scen_plot(Q_1)
Q_plot2 <- Q_scen_plot(Q_2)
Q_plot3 <- Q_scen_plot(Q_3)
Q_plot4 <- Q_scen_plot(Q_4)

Q_plot1
Q_plot2
Q_plot3
Q_plot4

#COMPUTE AIR EMISSION REDUCTION
B_1 <- B_scen(DF_1,scen_1)
B_2 <- B_scen(DF_2,scen_2)
B_3 <- B_scen(DF_3,scen_3)
B_4 <- B_scen(DF_4,scen_4)

#PLOT EMISSION REDUCTION
B_plot1 <- B_scen_plot(B_1)
B_plot2 <- B_scen_plot(B_2)
B_plot3 <- B_scen_plot(B_3)
B_plot4 <- B_scen_plot(B_4)


B_plot1
B_plot2
B_plot3
B_plot4

#MANIPULATE TO PLOT FINAL RESULTS OF SCENARIOS
Q_final <- data.frame()
Q_final <- rbind(Q_final,c("1 sector",
                           min(Q_1$ymed),min(Q_1$ymin),min(Q_1$ymax)))
Q_final <- rbind(Q_final,c("2 sectors",
                           min(Q_2$ymed),min(Q_2$ymin),min(Q_2$ymax)))
Q_final <- rbind(Q_final,c("3 sectors",
                           min(Q_3$ymed),min(Q_3$ymin),min(Q_3$ymax)))
Q_final <- rbind(Q_final,c("4 sectors",
                           min(Q_4$ymed),min(Q_4$ymin),min(Q_4$ymax)))
colnames(Q_final) <- c("scenario","ymed","ymin","ymax")
Q_final <- transform(Q_final, 
                     ymed = as.double(ymed), 
                     ymin = as.double(ymin),
                     ymax = as.double(ymax))

B_final <- data.frame()
B_final <- rbind(B_final,c("1 sector",
                           min(B_1$ymed),min(B_1$ymin),min(B_1$ymax)))
B_final <- rbind(B_final,c("2 sectors",
                           min(B_2$ymed),min(B_2$ymin),min(B_2$ymax)))
B_final <- rbind(B_final,c("3 sectors",
                           min(B_3$ymed),min(B_3$ymin),min(B_3$ymax)))
B_final <- rbind(B_final,c("4 sectors",
                           min(B_4$ymed),min(B_4$ymin),min(B_4$ymax)))
colnames(B_final) <- c("scenario","ymed","ymin","ymax")
B_final <- transform(B_final, 
                     ymed = as.double(ymed), 
                     ymin = as.double(ymin),
                     ymax = as.double(ymax))

#PLOT FINAL RESULTS (Qtot e GHGtot)
Q_final_plot <- Q_scen_plot(Q_final)
B_final_plot <- B_scen_plot(B_final)
Q_final_plot
B_final_plot



#TABLE WITH FINAL RESULTS

#WRITE ON AN EXCEL
filename <- file.path(getwd(),'graph',paste0("Q_tot.xlsx"))
xlsx::write.xlsx (Q_1, file = filename, sheetName = "1 sector",
                  col.names=TRUE, row.names = FALSE, append=FALSE)
xlsx::write.xlsx (Q_2, file = filename, sheetName = "2 sectors",
                  col.names=TRUE, row.names = FALSE, append=TRUE)
xlsx::write.xlsx (Q_3, file = filename, sheetName = "3 sectors",
                  col.names=TRUE, row.names = FALSE, append=TRUE)
xlsx::write.xlsx (Q_4, file = filename, sheetName = "all sectors",
                  col.names=TRUE, row.names = FALSE, append=TRUE)

filename <- file.path(getwd(),'graph',paste0("B_tot.xlsx"))
xlsx::write.xlsx (B_1, file = filename, sheetName = "1 sector",
                  col.names=TRUE, row.names = FALSE, append=FALSE)
xlsx::write.xlsx (B_2, file = filename, sheetName = "2 sectors",
                  col.names=TRUE, row.names = FALSE, append=TRUE)
xlsx::write.xlsx (B_3, file = filename, sheetName = "3 sectors",
                  col.names=TRUE, row.names = FALSE, append=TRUE)
xlsx::write.xlsx (B_4, file = filename, sheetName = "all sectors",
                  col.names=TRUE, row.names = FALSE, append=TRUE)






#MANIPULATE TO PLOT FINAL RESULTS OF SCENARIOS
#Q_final <- data.frame()
#Q_final <- rbind(Q_final,c("1 sector",
#                           mean(Q_1$ymed),min(Q_1$ymed),max(Q_1$ymed)))
# Q_final <- rbind(Q_final,c("2 sectors",
#                            mean(Q_2$ymed),min(Q_2$ymed),max(Q_2$ymed)))
# Q_final <- rbind(Q_final,c("3 sectors",
#                            mean(Q_3$ymed),min(Q_3$ymed),max(Q_3$ymed)))
# Q_final <- rbind(Q_final,c("4 sectors",
#                            mean(Q_4$ymed),min(Q_4$ymed),max(Q_4$ymed)))
# colnames(Q_final) <- c("scenario","ymed","ymin","ymax")
# Q_final <- transform(Q_final, 
#                      ymed = as.double(ymed), 
#                      ymin = as.double(ymin),
#                      ymax = as.double(ymax))
# 
# B_final <- data.frame()
# B_final <- rbind(B_final,c("1 sector",
#                            mean(B_1$ymed),min(B_1$ymed),max(B_1$ymed)))
# B_final <- rbind(B_final,c("2 sectors",
#                            mean(B_2$ymed),min(B_2$ymed),max(B_2$ymed)))
# B_final <- rbind(B_final,c("3 sectors",
#                            mean(B_3$ymed),min(B_3$ymed),max(B_3$ymed)))
# B_final <- rbind(B_final,c("4 sectors",
#                            mean(B_4$ymed),min(B_4$ymed),max(B_4$ymed)))
# colnames(B_final) <- c("scenario","ymed","ymin","ymax")
# B_final <- transform(B_final, 
#                      ymed = as.double(ymed), 
#                      ymin = as.double(ymin),
#                      ymax = as.double(ymax))
# 
# #PLOT FINAL RESULTS
# Q_final_plot <- Q_scen_plot(Q_final)
# B_final_plot <- B_scen_plot(B_final)
# Q_final_plot
# B_final_plot









###################################################################
#########################SCENARIOS 3D PLOT#########################
###################################################################
# Read DPCM q from Excel
nace_file_name <- file.path(data_directory,"DPCM-mod2.xlsx")
scenarios <- xlsx::read.xlsx(file = nace_file_name, sheetName = "Scenarios3", colIndex = c(1:6), endRow = 66, header = TRUE, 
                             colClasses = c('character',rep(c('numeric'),10)) )
scenarios
# Store the naces and remove from matrix dpcm
naces <- scenarios[,1]
scenarios <- scenarios[,-1]
rownames(scenarios) <- as.matrix(naces)
t1 <- 100


#1-5 : scenarios - a,b -> different K
q1 <- as.matrix(scenarios[,1]);
rownames(q1) <- as.matrix(naces);
q1 <- t(q1)
q1a <- q1
q1b <- q1

q2 <- as.matrix(scenarios[,2]);
rownames(q2) <- as.matrix(naces);
q2 <- t(q2)
q2a <- q2
q2b <-q2

q3 <- as.matrix(scenarios[,3]);
rownames(q3) <- as.matrix(naces);
q3 <- t(q3)
q3a <- q3
q3b <-q3

q4 <- as.matrix(scenarios[,4]);
rownames(q4) <- as.matrix(naces);
q4 <- t(q4)
q4a <- q4
q4b <- q4

for(k in 1:(t1-1)){
  q_temp1 <- as.matrix(q1[k,])
  q_temp1a <- as.matrix(q1a[k,])
  q_temp1b <- as.matrix(q1b[k,])
  
  q_temp2 <- as.matrix(q2[k,])  
  q_temp2a <- as.matrix(q2a[k,])  
  q_temp2b <- as.matrix(q2b[k,])  
  
  q_temp3 <- as.matrix(q3[k,])  
  q_temp3a <- as.matrix(q3a[k,])  
  q_temp3b <- as.matrix(q3b[k,])  
  
  q_temp4 <- as.matrix(q4[k,])  
  q_temp4a <- as.matrix(q4a[k,])  
  q_temp4b <- as.matrix(q4b[k,])  
  

  for(l in 1:nrow(scenarios)) {
    if(k<30){
      q_temp1[l] <- max(scenarios[l,1],t(q_discrete(K,q1[k,],A1_ii_s,c_s))[l])
      q_temp1a[l] <- max(scenarios[l,1],t(q_discrete(K1,q1a[k,],A1_ii_s,c_s))[l])
      q_temp1b[l] <- max(scenarios[l,1],t(q_discrete(K2,q1b[k,],A1_ii_s,c_s))[l])
      q_temp2[l] <- max(scenarios[l,2],t(q_discrete(K,q2[k,],A1_ii_s,c_s))[l])
      q_temp2a[l] <- max(scenarios[l,2],t(q_discrete(K1,q2a[k,],A1_ii_s,c_s))[l])
      q_temp2b[l] <- max(scenarios[l,2],t(q_discrete(K2,q2b[k,],A1_ii_s,c_s))[l])
      q_temp3[l] <- max(scenarios[l,3],t(q_discrete(K,q3[k,],A1_ii_s,c_s))[l])
      q_temp3a[l] <- max(scenarios[l,3],t(q_discrete(K1,q3a[k,],A1_ii_s,c_s))[l])
      q_temp3b[l] <- max(scenarios[l,3],t(q_discrete(K2,q3b[k,],A1_ii_s,c_s))[l])
      q_temp4[l] <- max(scenarios[l,4],t(q_discrete(K,q4[k,],A1_ii_s,c_s))[l])
      q_temp4a[l] <- max(scenarios[l,4],t(q_discrete(K1,q4a[k,],A1_ii_s,c_s))[l])
      q_temp4b[l] <- max(scenarios[l,4],t(q_discrete(K2,q4b[k,],A1_ii_s,c_s))[l])
    }else{
      q_temp1[l] <- t(q_discrete(K,q1[k,],A1_ii_s,c_s))[l]
      q_temp1a[l] <- t(q_discrete(K1,q1a[k,],A1_ii_s,c_s))[l]
      q_temp1b[l] <- t(q_discrete(K2,q1b[k,],A1_ii_s,c_s))[l]
      q_temp2[l] <- t(q_discrete(K,q2[k,],A1_ii_s,c_s))[l]
      q_temp2a[l] <- t(q_discrete(K1,q2a[k,],A1_ii_s,c_s))[l]
      q_temp2b[l] <- t(q_discrete(K2,q2b[k,],A1_ii_s,c_s))[l]
      q_temp3[l] <- t(q_discrete(K,q3[k,],A1_ii_s,c_s))[l]
      q_temp3a[l] <- t(q_discrete(K1,q3a[k,],A1_ii_s,c_s))[l]
      q_temp3b[l] <- t(q_discrete(K2,q3b[k,],A1_ii_s,c_s))[l]
      q_temp4[l] <- t(q_discrete(K,q4[k,],A1_ii_s,c_s))[l]
      q_temp4a[l] <- t(q_discrete(K1,q4a[k,],A1_ii_s,c_s))[l]
      q_temp4b[l] <- t(q_discrete(K2,q4b[k,],A1_ii_s,c_s))[l]
    }
    
  }
  q1 <- rbind(q1,t(q_temp1))
  q1a <- rbind(q1a,t(q_temp1a))
  q1b <- rbind(q1b,t(q_temp1b))
  q2 <- rbind(q2,t(q_temp2))
  q2a <- rbind(q2a,t(q_temp2a))
  q2b <- rbind(q2b,t(q_temp2b))
  q3 <- rbind(q3,t(q_temp3))
  q3a <- rbind(q3a,t(q_temp3a))
  q3b <- rbind(q3b,t(q_temp3b))
  q4 <- rbind(q4,t(q_temp4))
  q4a <- rbind(q4a,t(q_temp4a))
  q4b <- rbind(q4b,t(q_temp4b))
}

rownames(q1) <- seq(1:t1)
rownames(q2) <- seq(1:t1)
rownames(q3) <- seq(1:t1)
rownames(q4) <- seq(1:t1)

#Dynamic of q
q1_plot <- plot_qdyn(q4[,],100)
q1_plot

#q_plot <- plot_qdyn(q_disc)
q1_plot <- plot_2D(q2[,], "Inoperability q",100,-1,FALSE)
q1_plot


scen1 <- plot_qdyn3D_mod(q1)
scen2 <- plot_qdyn3D_mod(q2)
scen3 <- plot_qdyn3D_mod(q3)
scen4 <- plot_qdyn3D_mod(q4)

#q5_plot <- plot_2D(q5[,], "Inoperability q",100,-1,TRUE)

saveWidget(scen1,
           file.path(paste0(getwd(),"/graph/def29102021/"),"scenario1.html"),
           selfcontained = TRUE,libdir = "lib")
saveWidget(scen2,
           file.path(paste0(getwd(),"/graph/def29102021/"),"scenario2.html"),
           selfcontained = TRUE,libdir = "lib")
saveWidget(scen3,
           file.path(paste0(getwd(),"/graph/def29102021/"),"scenario3.html"),
           selfcontained = TRUE,libdir = "lib")
saveWidget(scen4,
           file.path(paste0(getwd(),"/graph/def29102021/"),"scenario4.html"),
           selfcontained = TRUE,libdir = "lib")


Q_scenario <- data.frame(
  scenario=matrix(NA,length(scenarios),1),
  ymed=matrix(NA,length(scenarios),1),
  ymin=matrix(NA,length(scenarios),1),
  ymax=matrix(NA,length(scenarios),1)
)

Q_scenario['scenario'][1,] <- 'Scenario 1'
Q_scenario['ymed'][1,] <- sum(total_economic_loss(x1,q1))
Q_scenario['ymin'][1,] <- sum(total_economic_loss(x1,q1a))
Q_scenario['ymax'][1,] <- sum(total_economic_loss(x1,q1b))

Q_scenario['scenario'][2,] <- 'Scenario 2'
Q_scenario['ymed'][2,] <- sum(total_economic_loss(x1,q2))
Q_scenario['ymin'][2,] <- sum(total_economic_loss(x1,q2a))
Q_scenario['ymax'][2,] <- sum(total_economic_loss(x1,q2b))

Q_scenario['scenario'][3,] <- 'Scenario 3'
Q_scenario['ymed'][3,] <- sum(total_economic_loss(x1,q3))
Q_scenario['ymin'][3,] <- sum(total_economic_loss(x1,q3a))
Q_scenario['ymax'][3,] <- sum(total_economic_loss(x1,q3b))

Q_scenario['scenario'][4,] <- 'Scenario 4'
Q_scenario['ymed'][4,] <- sum(total_economic_loss(x1,q4))
Q_scenario['ymin'][4,] <- sum(total_economic_loss(x1,q4a))
Q_scenario['ymax'][4,] <- sum(total_economic_loss(x1,q4b))

Q_scenario['scenario'][5,] <- 'Scenario 5'
Q_scenario['ymed'][5,] <- sum(total_economic_loss(x1,q5))
Q_scenario['ymin'][5,] <- sum(total_economic_loss(x1,q5a))
Q_scenario['ymax'][5,] <- sum(total_economic_loss(x1,q5b))



Q_plot_scenario <- ggplot(data=Q_scenario)+
  geom_bar(aes(x=scenario, y=ymed),stat="identity")+
  geom_errorbar(aes(x=scenario,ymin=ymin,ymax=ymax),
                width=0.4, colour="orange", alpha=0.9)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=12),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12))+
  labs(y = "Economic losses [mln euro]")
Q_plot_scenario




#AIR EMISSION REDUCTION FOR SCENARIOS
q1_ <- q1[ , -which(colnames(q_disc) %in% c("L68B"))]
q1a_ <- q1a[ , -which(colnames(q_disc) %in% c("L68B"))]
q1b_ <- q1b[ , -which(colnames(q_disc) %in% c("L68B"))]
q2_ <- q2[ , -which(colnames(q_disc) %in% c("L68B"))]
q2a_ <- q2a[ , -which(colnames(q_disc) %in% c("L68B"))]
q2b_ <- q2b[ , -which(colnames(q_disc) %in% c("L68B"))]
q3_ <- q3[ , -which(colnames(q_disc) %in% c("L68B"))]
q3a_ <- q3a[ , -which(colnames(q_disc) %in% c("L68B"))]
q3b_ <- q3b[ , -which(colnames(q_disc) %in% c("L68B"))]
q4_ <- q4[ , -which(colnames(q_disc) %in% c("L68B"))]
q4a_ <- q4a[ , -which(colnames(q_disc) %in% c("L68B"))]
q4b_ <- q4b[ , -which(colnames(q_disc) %in% c("L68B"))]
q5_ <- q5[ , -which(colnames(q_disc) %in% c("L68B"))]
q5a_ <- q5a[ , -which(colnames(q_disc) %in% c("L68B"))]
q5b_ <- q5b[ , -which(colnames(q_disc) %in% c("L68B"))]

#2015
#1: "ACG" - 2: "CH4" - 3: "CH4_CO2E" - 4: "CH4_NMVOCE" - 5: "CO" - 6: "CO2" - 
#7: "CO2_BIO" - 8: "CO_NMVOCE" - 9: "GHG" - 10: "HFC_CO2E" - 11: "N2O" - 
#12: "N2O_CO2E" - 13: "NF3_SF6_CO2E" - 14: "NH3" - 15: "NH3_SO2E"   
j <- 9 #it corresponds to B rows - B[j,] 


red1_ <- q1[ , -which(colnames(q_disc) %in% c("L68B"))]
red1a_ <- q1a[ , -which(colnames(q_disc) %in% c("L68B"))]
red1b_ <- q1b[ , -which(colnames(q_disc) %in% c("L68B"))]
red2_ <- q2[ , -which(colnames(q_disc) %in% c("L68B"))]
red2a_ <- q2a[ , -which(colnames(q_disc) %in% c("L68B"))]
red2b_ <- q2b[ , -which(colnames(q_disc) %in% c("L68B"))]
red3_ <- q3[ , -which(colnames(q_disc) %in% c("L68B"))]
red3a_ <- q3a[ , -which(colnames(q_disc) %in% c("L68B"))]
red3b_ <- q3b[ , -which(colnames(q_disc) %in% c("L68B"))]
red4_ <- q4[ , -which(colnames(q_disc) %in% c("L68B"))]
red4a_ <- q4a[ , -which(colnames(q_disc) %in% c("L68B"))]
red4b_ <- q4b[ , -which(colnames(q_disc) %in% c("L68B"))]
red5_ <- q5[ , -which(colnames(q_disc) %in% c("L68B"))]
red5a_ <- q5a[ , -which(colnames(q_disc) %in% c("L68B"))]
red5b_ <- q5b[ , -which(colnames(q_disc) %in% c("L68B"))]



for(i in 1:length(q1_[1,])){
  red1_[,i] <- q1_[,i]*B[j,i]/365
  red1a_[,i] <- q1a_[,i]*B[j,i]/365
  red1b_[,i] <- q1b_[,i]*B[j,i]/365
  red2_[,i] <- q2_[,i]*B[j,i]/365
  red2a_[,i] <- q2a_[,i]*B[j,i]/365
  red2b_[,i] <- q2b_[,i]*B[j,i]/365
  red3_[,i] <- q3_[,i]*B[j,i]/365
  red3a_[,i] <- q3a_[,i]*B[j,i]/365
  red3b_[,i] <- q3b_[,i]*B[j,i]/365
  red4_[,i] <- q4_[,i]*B[j,i]/365
  red4a_[,i] <- q4a_[,i]*B[j,i]/365
  red4b_[,i] <- q4b_[,i]*B[j,i]/365
  red5_[,i] <- q5_[,i]*B[j,i]/365
  red5a_[,i] <- q5a_[,i]*B[j,i]/365
  red5b_[,i] <- q5b_[,i]*B[j,i]/365
}




B_scenario <- data.frame(
  scenario=matrix(NA,length(scenarios),1),
  ymed=matrix(NA,length(scenarios),1),
  ymin=matrix(NA,length(scenarios),1),
  ymax=matrix(NA,length(scenarios),1)
)

B_scenario['scenario'][1,] <- 'Scenario 1'
B_scenario['ymed'][1,] <- sum(red1_)
B_scenario['ymin'][1,] <- sum(red1a_)
B_scenario['ymax'][1,] <- sum(red1b_)

B_scenario['scenario'][2,] <- 'Scenario 2'
B_scenario['ymed'][2,] <- sum(red2_)
B_scenario['ymin'][2,] <- sum(red2a_)
B_scenario['ymax'][2,] <- sum(red2b_)

B_scenario['scenario'][3,] <- 'Scenario 3'
B_scenario['ymed'][3,] <- sum(red3_)
B_scenario['ymin'][3,] <- sum(red3a_)
B_scenario['ymax'][3,] <- sum(red3b_)

B_scenario['scenario'][4,] <- 'Scenario 4'
B_scenario['ymed'][4,] <- sum(red4_)
B_scenario['ymin'][4,] <- sum(red4a_)
B_scenario['ymax'][4,] <- sum(red4b_)

B_scenario['scenario'][5,] <- 'Scenario 5'
B_scenario['ymed'][5,] <- sum(red5_)
B_scenario['ymin'][5,] <- sum(red5a_)
B_scenario['ymax'][5,] <- sum(red5b_)



B_plot_scenario <- ggplot(data=B_scenario)+
  geom_bar(aes(x=scenario, y=ymed),stat="identity")+
  geom_errorbar(aes(x=scenario,ymin=ymin,ymax=ymax),
                width=0.4, colour="orange", alpha=0.9)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=12),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12))+
  labs(y = expression(paste('Avoided GHG emission [kt CO '[2],'eq]')))
B_plot_scenario


Q_plot_scenario + B_plot_scenario












