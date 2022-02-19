################################################################################

GbmTrain <- function(train.name="train",target=NULL,args=list(test.name=NULL,
	R=1,pls=FALSE,prune=FALSE,std=FALSE,features=c("static","alsfrs","fvc",
	"svc","vital"), slope.features=NULL)) {
	# Trains a GBM model given training data.
	# Args:
	#	train.name: base name of training dataset
	#	target: target vector; if NULL, future slopes are used as target
	#	args: list of additional arguments
	#	-test.name: base name of test set, if you want training set to be 
	#	 processed using both test and train data
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
	# Side effects:
	#	Saves predictions to file
	#
	# Example usage:
	if(FALSE) {
		features = c("static","alsfrs","fvc","svc","vital")
	   	slope.features = NULL
	   	args=list(test.name="test",R=1,pls=FALSE,prune=FALSE,std=FALSE,
	   		features=features,slope.features=slope.features)
		model <- GbmTrain(train.name="train",target=NULL,args=args)
		predictions<-model("test")
	}

	# Preprocess training data according to arguments
	train.preprocess<-PreprocessFeatures(train.name=train.name,
		test.name=args[["test.name"]],args=args)
		
	# Get training data and target
	train.data<-as.data.frame(train.preprocess$feature.data)
	
	# Need to throw out if not imputing NAs
	#train.data<-train.data[,colnames(train.data)!="slope.height.slope"]
	
	target<-train.preprocess$target
	
	# Fit multiple models to training data	
	GetModel <- function(i) {
		# Fit to training data
		object <- gbm(target~.,data=train.data[,-1],distribution="gaussian",n.trees=20000,
			cv.folds=10,verbose=FALSE)
	
		# Show features used
		print(summary(object,plot=FALSE))
	
		# Find optimal number of iterations
		#best.iter<-gbm.perf(object,method="OOB") # Check with OOB
		#best.iter<-gbm.perf(object,method="test") # Check with 50% heldout test
		best.iter<-gbm.perf(object,method="cv",plot.it=FALSE) # Check with CV
	
		# Report summary results
		print(sprintf("Model: %d, CV error: %g, Best iter: %g",i,min(object$cv.error),best.iter))
		return(list(object = object, param = best.iter))
	}
	models <- lapply(1:args[["R"]], GetModel)
	
	GbmPredict <- function(test.name="test",save.preds=TRUE) {
		# Returns predictions based on the trained model given a test set
		#
		# Args:
		#	test.name: base name of test dataset
		#   save.preds: save predictions to file? TRUE by default
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
			train.preprocess$train.data.std,
			train.data.na=train.preprocess$train.data.na)

		test.data<-as.data.frame(test.preprocess$feature.data)
		
		# Need to throw out if not imputing NAs
		#test.data<-test.data[,colnames(test.data)!="slope.height.slope"]
		
		predictions <- rowMeans(sapply(models, function(model) 
			predict(model$object,test.data[,-1],model$param)))
		
		# If predicting last score, transform predictions back to slopes
		if (args[["pls"]]) {
			scaling<-PlsScale(test.name)
			predictions<-predictions-
				scaling$score.first*scaling$time.scale
		}
		predictions <- cbind(test.data[,1],predictions)
		
		if(save.preds) {
			# Write predictions to file
			out.file = GetPredictionFilename("GbmTrain",train.name,test.name,args)
			WritePredictions(predictions, out.file)
		}
		return(predictions)
	}
	return(GbmPredict)
}
