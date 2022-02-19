################################################################################ 

GetAlsfrsQuestions <- function() {
	
	# Creates a list of columns corresponding to the ALSFRS questions that go 
	# into the ALSFRS total score.
	#
	# Returns:
	#	A vector that contains the column names corresponding to the ALSFRS 
	#	questions used for the ALSFRS total score.
	
	alsfrs.questions <- c("speech", "salivation", "swallowing", "handwriting", 
		"cutting.wo.gastrostomy", "cutting.w.gastrostomy", "dressing", "turning", 
		"walking", "climbing.stairs", "respiratory", "r1.dyspnea")
	return(alsfrs.questions)
}
	
