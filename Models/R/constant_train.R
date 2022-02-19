ConstantTrain <- function(train.name,test.name,save.preds=TRUE,args=list(slope=-.9)) {
	# Returns a constant slope model
	#
	# Args:
	#	train.name: (unused) base name of training dataset
	#	test.name: base name of test dataset
	#   save.preds: save predictions to file? TRUE by default
	#	args: list of additional arguments including "slope", the constant 
	#		prediction
	#
	# Returns:
	#	Two column matrix, ordered by the first column, with first column 
	#   "subject.id" containing test set subject ids and the second column 
	#   "prediction" containing predicted slopes.
	#
	# Example usage:
	#	source("Code/Models/constant_predict.R")
	#	ConstantPredict("","train",list(slope=-.9))
	
	# Extract additional arguments
	slope = args[["slope"]]
		
	ConstantPredict <- function(test.name="test",save.preds=TRUE) {
		# Returns constant slope prediction for each subject
		#
		# Args:
		#	test.name: base name of test dataset
		#   save.preds: save predictions to file? TRUE by default
		#
		# Returns:
		#	Two column matrix, ordered by the first column, with first column 
		#   "subject.id" containing test set subject ids and the second column 
		#   "prediction" containing predicted slopes.
		
		# Get test data
		dataset <- GetRawData(test.name)
	
		# Form constant predictions
		subject.id<-unique(dataset$subject.id)

		predictions <- cbind(subject.id,rep(slope,length(subject.id)))
		colnames(predictions) <- c("subject.id","prediction")
	
		if(save.preds) {
			# Write predictions to file
			out.file = sprintf("Predictions/Constant_%g-%s.txt",slope,test.name)
			WritePredictions(predictions, out.file)
		}
		return(predictions)
	}
	return(ConstantPredict)
}