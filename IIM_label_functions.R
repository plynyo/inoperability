########################################################################
########################################################################
#### Functions to:
#### - build a database of the sectors by code, description, and labels and 
#### - search the row/column number of a matrix by code/description/label 
#### TODO:
#### 1) DONE -- write a function to select the right row/column in a matrix based on the code/description of the sector
#### 2) use tolower(), so we don't need to be precise with lower/uppercase letters and codes
#### n) check if the metadata and the columns of our tables correspond to each other
####       (if we input a labels that are not in iotables_metadata, the function exits with an error)







# Returns the row/column number of a matrix corresponding to a text
# The text can be the code, description or label
# This function takes directly a matrix, and creates a temporary db (slower)
# -> it can be modified to filter directly iotables::metadata to select industries/products by either name, code or whatever
# Usage: sector_select_matrix(A0_pp, "chemicals")
sector_select_matrix <- function(data_table, text ) {
  sectors <- get_labels(data_table)
  return( sector_select(sectors, text) )
}

# Returns the row/column number corresponding to a text
# If not unique, returns 0
# If the sector is not found, returns NA
# It takes a pre-formed db (made with get_labels) as input -> much faster
# Usage: sector_select(ii_sectors, "chemicals")
sector_select <- function (sectors_db, text) {
  res <- lapply(sectors_db, function(ch) grep(text, ch))
  if (! is.na(res$code[1]) ) {
    if (! is.na(res$code[2]) )
      return(0)
    else
      return(res$code[1])
  } else if (! is.na(res$description[1]) ) {
    if (! is.na(res$description[2]) )
      return(0)
    else
      return(res$description[1])
  } else if (! is.na(res$iotables_label[1]) ) {
    if (! is.na(res$iotables_label[2]) )
      return(0)
    else
      return(res$iotables_label[1])
  } else return(NA)
}


# Returns a dataframe of the codes, descriptions, and readable labels of a matrix
# Usage:
# industry_labels <- get_labels(Z0_pp)

get_labels <- function(data_table) {
  
  # Create the dataframe (tibble)
  labels_db <- tribble (~code, ~label, ~iotables_label)
  
  # Get the names of the columns or, if not present, the rows
  if (! is.null(colnames(data_table)) )
    table_codes <- colnames(data_table)
  else table_codes <- rownames(data_table)
  
  # Build the dataframe
  for (i in table_codes )  {
    
    # avoid column "prod_na"
    if (i != "prod_na") 
      # Filters the rows containing ind_code and variable "induse" (since most industries have various rows)
      # and selects only the columns we want to keep
      labels_db <- labels_db %>% add_row (iotables::metadata %>% 
                                            filter( code == i, variable == "induse") %>% 
                                            select( c("code", "label", "iotables_label") )
                                          )
  }
  
  # rename "label" to "description"
  labels_db <- labels_db %>% rename(description = label)
  
  return(labels_db)
}


matrix_setup <- function(M_, cutoff = nrow(M_) ) {
  if(length(M_[,names(M_) == "prod_na"]) != 0){
    M2_ <- as.matrix(M_[,names(M_) != "prod_na"])
    row.names(M2_) <- as.matrix(M_[,names(M_) == "prod_na"])
  }else{
    M2_ <- M_
  }
  
  if(cutoff < nrow(M_)){
    M2_ <- M2_[1:cutoff,1:cutoff]
  }
  
  return(M2_)
} 



vector_setup <- function(v_,M_,cutoff = nrow(M_)){
  row.names(v_) <- as.matrix(M_[1:cutoff,names(M_) == "prod_na"])
  return(v_)
}


get_labels_matrix <- function(data_table,row_or_col_ = "column") {
  
  # Create the dataframe (tibble)
  labels_db <- tribble (~code, ~label, ~iotables_label)
  var_type_ <- ""
  # Get the names of the columns or, if not present, the rows
  if(row_or_col_ == "column"){
    table_codes <- colnames(Z)
    var_type_ <- "induse"
  }else if(row_or_col_ == "row"){
    table_codes <- as.vector(rownames(Z))
    var_type_ <- "prod_na"
  }else{
    return()
  }
  
  # Build the dataframe
  for (i in 1:length(table_codes) ){
    
    # Filters the rows containing ind_code and variable "induse" (since most industries have various rows)
    # and selects only the columns we want to keep
    labels_db <- labels_db %>% add_row (iotables::metadata %>% 
                                          filter( code == table_codes[i], variable == var_type_) %>% 
                                          select( c("code", "label", "iotables_label") )
    )
  }
  
  # rename "label" to "description"
  labels_db <- labels_db %>% rename(description = label)
  
  return(labels_db)
}
