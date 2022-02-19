################################################################################
# Dependencies
#source("Code/get_alsfrs_questions.r")

AlsfrsScore <- function(dataset) {
	# Calculates the 10 question ALSFRS/ALSFRS(R) contest score given a dataset 
	# generated by GenAlsfrsData
	#
	# Args:
	#	dataset: An ALSFRS dataset generated by GenAlsfrsData
	#
	# Returns: 
	#	Original dataset with ALSFRS score as an additional column
	#
	# Example usage:
	# 	dataset <- AlsfrsScore(dataset)
	
	alsfrs.questions<-GetAlsfrsQuestions()
	# set all values for 5b to NA in rows where values for 5a and 5b are both 
	# numbers
	dataset$cutting.w.gastrostomy[!is.na(dataset$cutting.wo.gastrostomy)&!is.na(
		dataset$cutting.wo.gastrostomy)]<-NA
	sum<-rowSums(dataset[,colnames(dataset) %in% alsfrs.questions],
	na.rm=TRUE)
	cols<-colnames(dataset)
	dataset<-cbind(dataset,sum)
	colnames(dataset)<-c(cols,"alsfrs.score")
	return(dataset)
}
