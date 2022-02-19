################################################################################
# Dependencies
# library("glmnet")
# .5097359 with RF Impute
GlmnetTrain <- function(train.name="train",target=NULL,args=list(test.name=NULL,
		R=1,pls=FALSE,prune=FALSE,std=FALSE,features=c("static","alsfrs","fvc",
		"svc","vital"),slope.features=NULL)) {
	# Trains a LASSO regression model to given training data and returns a function
	# that returns predictions given test data
	#
	# Args:
	#	train.name: base name of training dataset
	#	target: target vector; if NULL, future slopes are used as target
	#	args: list of additional arguments
	#	-test.name: base name of test set, if you want training set to be 
	#	 processed using both test and train data
	#   - R: number of models to train; final predictions formed 
	#        by averaging across model predictions
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
	#
	# Returns:
	#	A function that returns predictions given test data
	#
	# Example usage:
	if (FALSE) {
		features = c("static","alsfrs","fvc","svc","vital")
	   	slope.features = c("alsfrs","fvc","svc","vital")
	   	args=list(test.name="test",R=1,pls=FALSE,prune=TRUE,std=FALSE,
	   		features=features,slope.features=slope.features)
		model <- GlmnetTrain(train.name="train",target=NULL,args=args)
		predictions<-model("test")
	}
	############################################################################
		
	# Preprocess training data according to arguments
	train.preprocess<-PreprocessFeatures(train.name=train.name,test.name=
		args[["test.name"]],args=args)
		
	# Get training data 
	train.data<-train.preprocess$feature.data

	# Add onset interactions
	# onset.delta.x<-train.data[,-1]*train.data[,colnames(train.data)=="Onset.Delta"]
	# colnames(onset.delta.x)<-paste(colnames(train.data)[-1],"interact")
	# train.data<-cbind(train.data,onset.delta.x)
	
	target<-train.preprocess$target
	
	# Drop constant columns		
	train.data<-train.data[,apply(train.data,2,function(c)length(unique(c))>1)]
	
	# Fit multiple models to training data	
	GetModel <- function(i) {
		# Use cross-validation to choose Lasso parameter
		cv.res <- cv.glmnet(as.matrix(train.data[,-1]), target)
		print(sprintf("Model: %d, CV error: %g, lambda: %g",i,sqrt(min(cv.res$cvm)),
			cv.res$lambda.min))
		return(list(object = cv.res$glmnet.fit, param = cv.res$lambda.min))
	}
	models <- lapply(1:args[["R"]], GetModel)
	
	GlmnetPredict <- function(test.name="test",save.preds=TRUE) {
		# Predicts slopes using a LASSO regression fit to static training data.
		#
		# Args:
		#	test.name: base name of test set
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
			train.preprocess$train.data.std,train.data.na=
			train.preprocess$train.data.na)

		test.data<-test.preprocess$feature.data
		
		# Match train columns
		test.data<-test.data[,colnames(train.data)]
	
		# Add onset interactions
		# onset.delta.x<-test.data[,-1]*test.data[,colnames(test.data)=="Onset.Delta"]
		# colnames(onset.delta.x)<-paste(colnames(test.data)[-1],"interact")
		# test.data<-cbind(test.data,onset.delta.x)

		# Form test predictions
		predictions <- rowMeans(sapply(models, function(model) predict(
			model$object,as.matrix(test.data[,-1]),s=model$param)))
		
		# If predicting last score, transform predictions back to slopes
		if (args[["pls"]]) {
			scaling<-PlsScale(test.name)
			predictions<-predictions-
				scaling$score.first*scaling$time.scale
		}
		predictions <- cbind(test.data[,1],predictions)
	
		if(save.preds) {
			# Write predictions to file
			out.file = GetPredictionFilename("GlmnetTrain",train.name,test.name,args)
			WritePredictions(predictions, out.file)
		}
	
		return(predictions)
	}
	
	return(GlmnetPredict)
}


