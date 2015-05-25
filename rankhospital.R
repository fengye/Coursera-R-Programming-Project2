rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check that state and outcome are valid
	state_list <- unique(outcome_data[,7])
	outcome_list <- c("heart attack", "heart failure", "pneumonia")
	outcome_index_list <- c(11, 17, 23)
	outcome_df <- data.frame(args=outcome_list, indices=outcome_index_list)

	if (!is.element(state, state_list)) 
	{
		stop("invalid state")
	}

	if (!is.element(outcome, outcome_list))
	{
		stop("invalid outcome")
	}

	if (num != "best" && num != "worst" && !is.numeric(num) )
	{
		stop("invalid num")
	}

	# data massage
	state_hospital <- subset(outcome_data, State==state)
	outcome_index <- subset(outcome_df, args==outcome)[, 2]

	valid_state_hospital <- state_hospital[complete.cases(state_hospital[, outcome_index]), ]

	## Return hospital name in that state with the given rank
	## 30-day death rate
	if (num == "best")
	{
		row <- valid_state_hospital[which.min(valid_state_hospital[, outcome_index]), ]
		return(row$Hospital.Name)
	}
	else
	if (num == "worst")
	{
		row <- valid_state_hospital[which.max(valid_state_hospital[, outcome_index]), ]
		return(row$Hospital.Name)
	}
	else
	{
		df <- valid_state_hospital[order(valid_state_hospital[, outcome_index],  valid_state_hospital[, 2]), ]
		row <- df[num, ]
		#row <- valid_state_hospital[order(rank(valid_state_hospital[, outcome_index], ties.method="first"))[num], ]
		return(row$Hospital.Name)	
	}
}
