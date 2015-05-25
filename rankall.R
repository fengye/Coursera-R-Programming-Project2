rank_state_hospital <- function(outcome_data, state, outcome, num = "best")
{
	outcome_list <- c("heart attack", "heart failure", "pneumonia")
	outcome_index_list <- c(11, 17, 23)
	outcome_df <- data.frame(args=outcome_list, indices=outcome_index_list)

	state_hospital <- subset(outcome_data, State==state)
	outcome_index <- subset(outcome_df, args==outcome)[, 2]

	state_hospital[, outcome_index] <- as.numeric(state_hospital[, outcome_index])
	valid_state_hospital <- state_hospital[complete.cases(state_hospital[, outcome_index]), ]

	## Return hospital name in that state with the given rank
	## 30-day death rate
	if (num == "best")
	{
		row <- valid_state_hospital[which.min(valid_state_hospital[, outcome_index]), ]
		return (c(row$Hospital.Name, state))
	}
	else
	if (num == "worst")
	{
		row <- valid_state_hospital[which.max(valid_state_hospital[, outcome_index]), ]
		return (c(row$Hospital.Name, state))
	}
	else
	{
		df <- valid_state_hospital[order(valid_state_hospital[, outcome_index],  valid_state_hospital[, 2]), ]
		row <- df[num, ]
		#row <- valid_state_hospital[order(rank(valid_state_hospital[, outcome_index], ties.method="first"))[num], ]
		return (c(row$Hospital.Name, state))
	}
}

rankall <- function(outcome, num = "best") {
	## Read outcome data
	outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check that state and outcome are valid
	state_list <- unique(outcome_data[,7])
	state_list <- sort(state_list) # need sort

	outcome_list <- c("heart attack", "heart failure", "pneumonia")
	outcome_index_list <- c(11, 17, 23)
	outcome_df <- data.frame(args=outcome_list, indices=outcome_index_list)

	if (!is.element(outcome, outcome_list))
	{
		stop("invalid outcome")
	}

	if (num != "best" && num != "worst" && !is.numeric(num) )
	{
		stop("invalid num")
	}

	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	outcome_index <- subset(outcome_df, args==outcome)[, 2]
	valid_state_hospital <- outcome_data[complete.cases(outcome_data[, outcome_index]), ]
	
	state_rank_result <- data.frame()
	for (state in state_list)
	{
		df <- data.frame(rank_state_hospital(outcome_data, state, outcome, num))
		state_rank_result <- rbind(state_rank_result, data.frame(df[1,1], df[2, 1]))
	}
	colnames(state_rank_result) <- c("hospital", "state")

	return(state_rank_result)
	
}