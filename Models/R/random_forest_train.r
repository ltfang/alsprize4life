################################################################################
# Dependencies
#library("randomForest")

RandomForestTrain <- function(train.name="train",target=NULL,
	args=list(test.name=NULL,R=1,corr.bias=FALSE,pls=FALSE,prune=FALSE,std=FALSE,
	features=c("static","alsfrs","fvc","svc","vital"), slope.features=NULL)) {
	
	# Trains a random forest model given training data
	#
	# Args:
	#	train.name: base name of training dataset
	#	target: replacement target vector; if NULL, future slopes are used as target
	#	args: list of additional arguments
	#   - R: number of random forest models to train; final predictions formed 
	#        by averaging across RF model predictions
	#	- pls: TRUE means predicting the last score rather than the slope
	#	- prune: TRUE means keeping only features with data for at least 918/2 
	#		patients
	#	- std: TRUE means standardize columns by subtracting away column means 
	#	- features: a character vector of feature sets to get for the training 
	#		data.  Choose from "static","alsfrs","fvc","svc","vital","lab"
	#	- slope.features: a character vector of datasets to get slope features 
	#		for.  Choose from NULL if no slopes are desired, or among "static",
	#		"alsfrs","fvc","svc","vital","lab"
	#
	# Returns:
	#	a function that given a test set, returns predictions
	#
	# Example usage:
	if(FALSE) {
		features = c("static","alsfrs","fvc","svc","vital")
	   	slope.features = c("alsfrs","fvc","svc","vital")
	   	args=list(test.name="test",R=1,corr.bias=FALSE,pls=FALSE,prune=FALSE,
	   		std=FALSE,features=features,slope.features=slope.features)
		model <- RandomForestTrain(train.name="train",target=NULL,args=args)
		predictions<-model("test")
	}
			
	# Preprocess training data according to arguments
	train.preprocess<-PreprocessFeatures(train.name=train.name,test.name=
		args[["test.name"]],args=args)
		
	# Get training data and target
	train.data<-train.preprocess$feature.data
	target<-train.preprocess$target

	# Drop constant columns		
	train.data<-train.data[,apply(train.data,2,function(c)length(unique(c))>1)]

	# Fit multiple models to training data
	models <- lapply(1:args[["R"]], function(i) 
		randomForest(train.data[,-1],target,corr.bias=args[["corr.bias"]],ntree=1500))

	# Display model out of bag error
	print(lapply(models, function(model) sqrt(model$mse[length(model$mse)])))

	RandomForestPredict <- function(test.name="test",save.preds=TRUE) {
		# Returns predictions based on a random forest model given a test set
		#
		# Args:
		#	test.name: base name of test dataset
		#	save.preds: save predictions to file? TRUE by default
		#
		# Returns:
		#	Two column matrix, ordered by the first column, with first column 
		#   "subject.id" containing test set subject ids and the second column 
		#   "prediction" containing predicted slopes.
		#
		# Side effects:
		#	Saves predictions to file
	
		# Preprocess test data according to arguments
		test.preprocess<-PreprocessFeatures(train.name=NULL,test.name=test.name,
			args=args,train.cols=train.preprocess$train.cols,train.data.std=
			train.preprocess$train.data.std,train.data.na=
			train.preprocess$train.data.na)

		test.data<-test.preprocess$feature.data
		
		# Match train columns
		test.data<-test.data[,colnames(train.data)]

		# Form test predictions by averaging the predictions of each model
		predictions <- rowMeans(sapply(models, function(model) predict(model,test.data[,-1])))
		
		# If predicting last score, transform predictions back to slopes
		if (args[["pls"]]) {
			scaling<-PlsScale(test.name)
			predictions<-predictions-
				scaling$score.first*scaling$time.scale
		}

		predictions <- cbind(test.data[,1],predictions)
	
		if(save.preds) {
			# Write predictions to file
			out.file = GetPredictionFilename("RandomForestTrain",train.name,test.name,args)
			WritePredictions(predictions, out.file)
		}
	
		return(predictions)
	}
	return(RandomForestPredict)
}