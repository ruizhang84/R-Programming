rankhospital <- function(state, outcome, num = "best") {
    #check if inputs as valid state and outcome
    state_50 <- c('AL','AK','AZ','AR','CA','CO','CT','DC','DE','FL','GA','GU','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MP','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','PR','RI','SC','SD','TN','TX','UT','VI','VT','VA','WA','WV','WI','WY')
    category <- c('heart attack', 'heart failure', 'pneumonia')
    if (!( state %in% state_50)) stop("invalid state")
    if (!( outcome %in% category)) stop("invalid outcome")
    #read in the outcome-of-care-measures data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    state_outcome <- subset(outcome_data, outcome_data$State == state)
    case <- ifelse(outcome == 'heart attack', 11 , ifelse(outcome == 'heart failure', 17, 23))
    state_outcome[, case] <- suppressWarnings(as.numeric(state_outcome[, case]))
    #find the hospital by rate
    num <- ifelse( num == 'best', 1, ifelse( num == 'worst', sum(!is.na(state_outcome[, case])), num))
    hospital <- state_outcome[order(state_outcome[, case], state_outcome$Hospital.Name), ]$Hospital.Name
    hospital[num]
}
