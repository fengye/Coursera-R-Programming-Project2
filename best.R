best <- function(state, outcome) {
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

	# data massage
	state_hospital <- subset(outcome_data, State==state)
	outcome_index <- subset(outcome_df, args==outcome)[, 2]

	valid_state_hospital <- state_hospital[complete.cases(state_hospital[, outcome_index]), ]

	## Return hospital name in that state with lowest 30-day death
	## rate
	#min(valid_state_hospital[, outcome_index])

	row <- valid_state_hospital[which.min(valid_state_hospital[, outcome_index]), ]
	row$Hospital.Name
}
