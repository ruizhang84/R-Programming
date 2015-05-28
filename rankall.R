rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    state <- sort(c('AK','AL','AR','AZ','CA','CO','CT','DC','DE','FL','GA','GU','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MP','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','PR','RI','SC','SD','TN','TX','UT','VI','VT','VA','WA','WV','WI','WY'))
    category <- c('heart attack', 'heart failure', 'pneumonia')
    if (!( outcome %in% category)) stop("invalid outcome")
    ## For each state, find the hospital of the given rank
    case <- ifelse(outcome == 'heart attack', 11 , ifelse(outcome == 'heart failure', 17, 23))
    num <- ifelse( num == 'best', 1, ifelse( num == 'worst', 'worst', num))
    outcome_data[, case] <- suppressWarnings(as.numeric(outcome_data[, case]))
    hospital <- sapply(state, function (x) 
                  subset(outcome_data, outcome_data$State == x)
                    [order(subset(outcome_data, outcome_data$State == x)[, case], subset(outcome_data, outcome_data$State == x)$Hospital.Name), ]
                    $Hospital.Name[ifelse(num == 'worst', sum(!is.na(subset(outcome_data, outcome_data$State == x)[, case])), num)])
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    hospital_state <- data.frame(cbind(hospital,state))
}
