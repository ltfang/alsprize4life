ChainingTrain <- function(train.name="train",args=list()) {
	# Predicts on successive residuals given a training set
	#
	# Args:
	#	train.name: base name of training dataset
	#	method.args: list of methods to use in order of application on residuals
	#
	#
	# Example usage:
	#	model<-ChainingTrain("train",args=list(RandomForestTrain,GlmnetTrain))
	# 	predictions<-model("test")
	
	# Initialize training targets
	target<-Slopes(GetAlsfrsData(train.name),3,12,"f")	
	
	# Initialize list to hold models
	model.list<-list()
		
	for (i in 1:length(args)) {
		# Get ith method
		method<-args[[i]]
		# Train a model on the training data and current targets
		model<-method(train.name,target=target)
		# Store the model in a list
		model.list[[i]]<-model
		# Predict targets on training data
		train.pred<-model(train.name)
		# Update targets to next level of residuals
		target<-target-train.pred[,2]
	}

	ChainingPredict <- function(test.name) {
	# Makes predictions based on a chaining model for residuals
	#	
	# Args:
	#	test.name: base name of test data
	#
	# Returns:
	#	Two column matrix, ordered by the first column, with first column 
	#   "subject.id" containing test set subject ids and the second column 
	#   "prediction" containing predicted slopes.
	#
	# Side effects:
	#	Saves predictions to file

		predictions<-lapply(model.list,function(x){x(test.name)})
		predictions<-Reduce(function(x,y) {merge(x,y,all.x=TRUE)},predictions)
		predictions<-cbind(predictions[,1],rowSums(predictions[,2:dim(predictions)[2]]))
		colnames(predictions)<-c("subject.id","predictions")

		# Write predictions to file
		out.file = sprintf("Predictions/Chaining-%s-%s.txt",train.name,test.name)
		WritePredictions(predictions, out.file)
	
		return(predictions)
	}
	
	return(ChainingPredict)
}	

