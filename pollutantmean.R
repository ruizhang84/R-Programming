pollutantmean <- function(directory, pollutant, id = 1:332) {
   file_list <- list.files(directory, full.names = TRUE)  
   data <- Reduce(function(x,y) {rbind(x,y)}, lapply(file_list, read.csv))
   data_filter <- lapply(id, function(id) {subset(data, data$ID == id)[pollutant]})
   mean(unlist(data_filter), na.rm =TRUE)
   ## 'directory' is a character vector of length 1 indicating
   ## the location of the CSV files
   
   ## 'pollutant' is a character vector of length 1 indicating
   ## the name of the pollutant for which we will calculate the
   ## mean; either "sulfate" or "nitrate".
   
   ## 'id' is an integer vector indicating the monitor ID numbers
   ## to be used
   
   ## Return the mean of the pollutant across all monitors list
   ## in the 'id' vector (ignoring NA values)
   ## NOTE: Do not round the result!
}