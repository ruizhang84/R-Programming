corr <- function(directory, threshold = 0) {
    file_list <- list.files(directory, full.names = TRUE)  
    data <- Reduce(function(x,y) {rbind(x,y)}, lapply(file_list, read.csv))
    nobs <- sapply( file_list, function(f) sum(complete.cases(read.csv(f))) )
    correlation <- function (id){
        cor(subset(data$sulfate, data$ID == id), subset(data$nitrate, data$ID == id), use="complete.obs")
    }
    
    case <- data.frame( id = c(1:length(nobs)), nobs)
    id <- case[nobs > threshold,]$id
    if (length(id) == 0){
        numeric(0)
    }else{
        sapply(id, correlation)
    }
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
}