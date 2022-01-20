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
  "eurostat","iotables",
  "matlib",
  "rstudioapi",
  "tidyverse"
)

have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
rm(have,want,junk)


#Folder where to save temporary data
temporary_directory <- file.path(getwd(),'data','temp')
if ( ! dir.exists(temporary_directory) ) dir.create (temporary_directory)


###############################################################################
###################IMPORT SECONDARY FILES WITH FUNCTIONS#######################
###############################################################################
source("IIM_functions.R")
source("IIM_label_functions.R")

###############################################################################
###################IMPORT IO TABLES FROM EUROSTAT##############################
###############################################################################

#GET TABLES at BASIC PRICE for ALL COUNTRIES AND YEARS and FILTER PER COUNTRY
# filter is dplyr::filter
country_id <- "IT"

if ( ! file.exists(file.path(temporary_directory,"naio_10_cp1700.rds")) ) {
  naio_10_cp1700 <- iotables_download("naio_10_cp1700", #SIOT PP
                                      data_directory = temporary_directory )
  naio_10_cp1700_it <- naio_10_cp1700 %>%
    filter ( geo %in% c(country_id)) %>%
    filter ( year %in% c(2010, 2019))
}
if ( ! file.exists(file.path(temporary_directory,"naio_10_cp1750.rds")) ) {
  naio_10_cp1750 <- iotables_download("naio_10_cp1750", #SIOT II
                                      data_directory = temporary_directory ) 
  naio_10_cp1750_it <- naio_10_cp1750 %>%
    filter ( geo %in% c(country_id)) %>%
    filter ( year %in% c(2010, 2019))
}
if ( ! file.exists(file.path(temporary_directory,"naio_10_cp1610.rds")) ) {
  naio_10_cp1610 <- iotables_download("naio_10_cp1610", #Use
                                      data_directory = temporary_directory ) 
  naio_10_cp1610_it <- naio_10_cp1610 %>%
    filter ( geo %in% c(country_id)) %>%
    filter ( year %in% c(2010, 2019))
}
if ( ! file.exists(file.path(temporary_directory,"naio_10_cp15.rds")) ) {
  naio_10_cp15 <- iotables_download("naio_10_cp15", #Supply
                                    data_directory = temporary_directory ) 
  naio_10_cp15_it <- naio_10_cp15 %>%
    filter ( geo %in% c(country_id)) %>%
    filter ( year %in% c(2010, 2019))
}



###############################################################################
######################## LOAD TEMPORARY DATA ##################################
###############################################################################

#load from pre-saved file to increase running speed
load (file = file.path(temporary_directory,'naio_10.rda') )

load (file = file.path(temporary_directory, 'naio_10_it.rda') )


###############################################################################
####################### SAVE TEMPORARY DATA ###################################
###############################################################################

#save all data
if ( ! file.exists(file.path(temporary_directory,"naio_10.rda")) )
  save (naio_10_cp1700, naio_10_cp1750, naio_10_cp1610,naio_10_cp15,
        file = file.path(temporary_directory, 'naio_10.rda'))

#save italian all data
if ( ! file.exists(file.path(temporary_directory,"naio_10_it.rda")) )
  save (naio_10_cp1700_it, naio_10_cp1750_it, naio_10_cp1610_it,naio_10_cp15_it,
        file = file.path(temporary_directory, 'naio_10_it.rda'))




###############################################################################
######GET SIOT (industryxindustry & productxproduct Symmetric IO Tables)#######
#####################and SUT (Supply and Use Tables)###########################
###############################################################################
year <- 2015


#SHORT LABELS (OPTION: labelling = "short")
#LONG LABELS  (OPTION: labelling = "iotables") 
Z0_pp <-  iotable_get ( labelled_io_data = naio_10_cp1700, 
                                 source = "naio_10_cp1700", geo = country_id, 
                                 year = year, unit = "MIO_EUR", 
                                 stk_flow = "TOTAL",
                                 labelling = "short")
Z0_ii <-  iotable_get ( labelled_io_data = naio_10_cp1750, 
                                 source = "naio_10_cp1750", geo = country_id, 
                                 year = year, unit = "MIO_EUR", 
                                 stk_flow = "TOTAL",
                                 labelling = "short")
supply <-  iotable_get ( labelled_io_data = naio_10_cp15, 
                                source = "naio_10_cp15", geo = country_id, 
                                year = year, unit = "MIO_EUR", 
                                stk_flow = "TOTAL",
                                labelling = "short")
use <-  iotable_get ( labelled_io_data = naio_10_cp1610, 
                             source = "naio_10_cp1610", geo = country_id, 
                             year = year, unit = "MIO_EUR", 
                             stk_flow = "TOTAL",
                             labelling = "short")



###############################################################################
#########################Input coefficients matrix#############################
###############################################################################
#Product x Product Matrix
A0_pp <- input_coefficient_matrix_create(
  data_table = Z0_pp
)

#Industry x Industry Matrix
A0_ii <- input_coefficient_matrix_create(
  data_table = Z0_ii
)


###############################################################################
#########################GET SECTORS LABELS####################################
###############################################################################

# Build the DB of sectors by name and code
all_sectors <- get_labels(Z0_ii)
pp_sectors <- get_labels(A0_pp)
ii_sectors <- get_labels(A0_ii)


#GET LABELS FROM MATRIX -> get_labels_matrix() works only on matrix (after matrix_setup())
Z <- matrix_setup(Z0_ii)
A <- matrix_setup(A0_ii)
Z_col_label <- get_labels_matrix(Z,"column")
Z_row_label <- get_labels_matrix(Z,"row")
tail(Z_row_label)

#Write NACE codes to Excel
#nace_file_name <- file.path(getwd(),'data','temp',"Z_codes.xlsx")
#xlsx::write.xlsx ( Z_col_label, file = nace_file_name, sheetName = "Column of Z",
#                  col.names=TRUE, row.names=TRUE, append=FALSE)
#xlsx::write.xlsx ( Z_row_label, file = nace_file_name, sheetName = "Row of Z",
#                   col.names=TRUE, row.names=TRUE, append=TRUE)

###############################################################################
##################################Leontieff matrix#############################
###############################################################################
#Product x Product Matrix
L0_pp <- leontieff_matrix_create( A0_pp )

#Industry x Industry Matrix
L0_ii <- leontieff_matrix_create( A0_ii )

###############################################################################
##########################Leontieff Inverse matrix#############################
###############################################################################
#Product x Product Matrix
I0_pp <- leontieff_inverse_create( A0_pp )

#Industry x Industry Matrix
I0_ii <- leontieff_inverse_create( A0_ii )






###############################################################################
###############################################################################
###############FORMALISM FROM Inoperability Input-Output Model#################
#######for Interdependent Infrastructure Sectors. I: Theory and Methodology####
#####################            METHOD 1               #######################
###############################################################################
###############################################################################



###############################################################################
######################### Generic Operators ###################################
###############################################################################
#Total Number of Sector
tot_sector <- nrow(A0_ii)

#UNITY VECTOR (SUM VECTOR)
s <- sum_vect(tot_sector)

#IDENTITY MATRIX
I <- I_matr(tot_sector)


###############################################################################
####################### EXOGENEOUS DEMAND e ###################################
###############################################################################
#WHICH COLUMN HAS TO BE SELECTED???? 
e1 <- use[1:tot_sector,c("TFU")]
#?????????????????????????????????????????



###############################################################################
############## SUPPLY TRANSPOSE - MAKE MATRIX #################################
###############################################################################
#V_t <- supply matrix (row = commodity  ; column = industry)
#V <- make matrix (row = industry ; column = commodity) is the supply matrix transpose
V_t <- matrix_setup(supply,tot_sector)
V <- t(V_t)


###############################################################################
###################### TOTAL INDUSTRY OUTPUT VECTOR ###########################
###############################################################################
#Equation 2 or 3 or 12 or 17
### Is this x0 ? it's the same as x1...
x0 <- rowSums(V)
x1 <- V %*% t(s) 
x1_i <- vect_inv(x1)
x1_i_d <- diag(as.vector(x1_i))



###############################################################################
########################## NORMALIZED USE TABLE ###############################
###############################################################################
#Equation 11
#U <- use matrix (row = commodity ; column = industry)
U <- matrix_setup(use,tot_sector)

#Equation 13 or 14
U_n <- U %*% x1_i_d



###############################################################################
########################## TOTAL COMMODITY OUTPUT VECTOR ######################
###############################################################################
#Equation 5 (1st method)
y1 <- t(V) %*% t(s)

#Equation 9 or 10 (2nd method)
y1a <- U %*% t(s) + e1

#Equation 20 (3rd method)
y1b <- U_n %*% x1 + e1


y1_i <- vect_inv(y1)
y1_i_d <- diag(as.vector(y1_i))


#TO DO -> Compare with y0
#CHECK ON EUROSTAT MANUAL CHAPTER 11
#I THINK WE HAVE TO SUBTRACT TRADE AND MARGIN


###############################################################################
########################## NORMALIZED MAKE MATRIX #############################
###############################################################################
#Equation 6
V_n <- V %*% y1_i_d



###############################################################################
################### TECHNICAL COEFFIECIENT MATRIX A ###########################
###############################################################################
#Equation 15
A1_ii <- V_n %*% U_n
#A = V_n*U_n (CHECK IT WITH A0_ii) - perhaps A = U_n*V_n should be A_pp


#TO DO -> Compare with A0
#CHECK ON EUROSTAT MANUAL CHAPTER 11
#I THINK WE HAVE TO SUBTRACT TRADE AND MARGIN



###############################################################################
###################### TRANSFORMATION MATRIX P ################################
###############################################################################
#P=(diag(x)^-1)
#Equation 35
P1 <- P_create (x1)
P1_i <- P_inv_create(x1)

#TO DO -> calculate P0


###############################################################################
################# DEMAND-BASED INTEDEPENDENCY MATRIX A* #######################
###############################################################################
#Equation 6 in "Managin the risk of terrorism...", 33 or 36 in "Inoperability...Theory and Methodology"
#A* = P * A * P^-1
#USARE A = V*U or A0_ii o A0_pp (directly from II - PP SIOT)

#TO DO -> change P1 with P0
A0_ii <- matrix_setup(A0_ii)
A0_ii_s <- A_star2(P1, A0_ii, P1_i)
# or A0_ii_s2 <- A_star(A0_ii, x1)

A1_ii_s <- A_star2(P1,A1_ii,P1_i)


###############################################################################
######################## Sector interdependency index #########################
############## From "Managing the risk of terrorism" paper ####################
###############################################################################
#interdependency index for Industry i
#Equation 31 
theta <- theta_create(A1_ii_s)


#interdependency ratio of Sectors i and j
#Equation 32
ro <- ro_create(theta) 
