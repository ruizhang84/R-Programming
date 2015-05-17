complete <- function(directory, id = 1:332) {
  file_list <- list.files(directory, full.names = TRUE)  
  nobs_total <- sapply( file_list, function(f) sum(complete.cases(read.csv(f))) )
  nobs <- nobs_total[id]
  data.frame( id = c(id), nobs, row.names = nrow(nobs) )
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}
