# Dependencies
# source("Code/get_alsfrs_data.R")
# source("Code/slopes.R")
# source("Code/write_predictions.R")

GroundTruthTrain <- function(train.name="",target=NULL,args=list()) {
	# Returns a function that predicts the ground-truth (that is, future) slope for 
	# each subject in the dataset.
	#
	# Args:
	#	train.name: (unused) base name of training dataset
	#	target: (unused) target vector; if NULL, future slopes are used as target
	#	args: (unused) list of additional arguments
	#
	# Returns:
	#	A function that predicts the ground-truth given a test name
	#
	# Example usage:
	#	model <- GroundTruthTrain()
	#	predictions<-model("test")
	
	GroundTruthPredict<-function(test.name="test",save.preds=TRUE) {
	# Returns the true future slopes of a test set
	#
	# Args:
	#	test.name: name of dataset
	#	save.preds: save predictions to file? TRUE by default
	#
	# Returns:
	#	Two column matrix, ordered by the first column, with first column 
	#   "subject.id" containing test set subject ids and the second column 
	#   "prediction" containing predicted slopes.
	#
	# Side effects:
	#	Saves predictions to file
		
		# Get alsfrs test data
		data <- GetAlsfrsData(test.name)

		# Get future slopes for each subject
		predictions <- cbind(sort(unique(data$subject.id)), Slopes(data,3,12,"f"))
		colnames(predictions) <- c("subject.id","prediction")
	
		if(save.preds) {
			# Write predictions to file
			out.file = paste("Predictions/GroundTruth-",test.name,".txt",sep="")
			WritePredictions(predictions, out.file)
		}
	
		return(predictions)
	}
	
	return(GroundTruthPredict)
}