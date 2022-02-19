################################################################################
# Dependencies
# library("mboost")

MboostBlackboostTrain <- function(train.name="train",target=NULL,args=list(test.name=NULL,R=1,pls=FALSE,
		prune=FALSE,std=FALSE,features=c("static","alsfrs","fvc","svc","vital"), 
		slope.features=NULL)) {
	# Trains a MboostBlackboost model given training data.
	# Args:
	#	train.name: base name of training dataset
	#	target: target vector; if NULL, future slopes are used as target
	#	args: list of additional arguments
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
	   	slope.features = c("alsfrs","fvc","svc","vital")
	   	args=list(test.name="test",R=1,pls=FALSE,prune=TRUE,std=FALSE,
	   		features=features,slope.features=slope.features)
		model <- MboostBlackboostTrain(train.name="train",target=NULL,args=args)
		predictions<-model("test")
	}

	# Preprocess training data according to arguments
	train.preprocess<-PreprocessFeatures(train.name=train.name,test.name=
		args[["test.name"]],args=args)
		
	# Get training data and target
	train.data<-as.data.frame(train.preprocess$feature.data)
	target<-train.preprocess$target
	
	# Drop constant columns		
	train.data<-train.data[,apply(train.data,2,function(c)length(unique(c))>1)]
	
	# Fit multiple models to training data	
	GetModel <- function(i) {		
		# Boosting with P-splines with a B-spline basis
		# object<-mboost(target~.,data.frame(target,train.data[,-1]),baselearner="bbs")
		# gamboost(target~.,data.frame(target,train.data[,-1]),
				# control = boost_control(mstop = ...)),
				# baselearner = c("bbs", "bols", "btree", "bss", "bns"), dfbase = 4)
		# Boosting with linear models
		# object<-mboost(target~.,data.frame(target,train.data[,-1]),baselearner="bols")
		# glmboost(formula, data = list(),
				# na.action = na.fail, contrasts.arg = NULL,
				# center = TRUE, control = boost_control(), ...)
		# Boosting with conditional trees
		#object<-mboost(target~.,data.frame(target,train.data[,-1]),baselearner="btree")
		object<-blackboost(target~.,data.frame(target,train.data[,-1]),
						   control = boost_control(mstop = 200),
						   # Default tree control
						    # tree_controls = ctree_control(
							 # teststat = "max",
							 # testtype = "Teststatistic",
							 # mincriterion = 0,
							 # maxdepth = 2, savesplitstats = FALSE))
							# Stump tree control
							tree_controls = ctree_control(stump = TRUE,
							mincriterion = 0,
							 savesplitstats = FALSE))
							
		# Select optimal number of boosting iterations using CV
		cv.err<-cvrisk(object, folds = cv(model.weights(object),type="kfold"))
		object[mstop(cv.err)]
	
		# Report summary results
		print(sprintf("Model: %d, CV error: %g, Best iter: %g",i,
			sqrt(min(colMeans(cv.err[,]))),mstop(cv.err)))
		
		# predictions <- predict(object,data.frame(test.data[,-1]))
		# Rmsd(predictions, test.target)	
		#test.target <- GroundTruthTrain()()
		#Rmsd(predictions, test.target[,2])		
	
		return(list(object = object))
	}
	models <- lapply(1:args[["R"]], GetModel)
	
	MboostBlackboostPredict <- function(test.name="test",save.preds=TRUE) {
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
			train.preprocess$train.data.std,train.data.na=
			train.preprocess$train.data.na)
	
		test.data<-as.data.frame(test.preprocess$feature.data)
			
		# Match train columns
		test.data<-test.data[,colnames(train.data)]

		# Form test predictions
		predictions <- rowMeans(sapply(models, function(model) predict(model$object,
			test.data[,-1])))
		
		# If predicting last score, transform predictions back to slopes
		if (args[["pls"]]) {
			predictions<-predictions-
				scaling$score.first*scaling$time.scale
		}
		predictions <- cbind(test.data[,1],predictions)
		
		if(save.preds) {
			# Write predictions to file
			out.file = GetPredictionFilename("MboostBlackboostTrain",train.name,test.name,args)
			WritePredictions(predictions, out.file)
		}
		return(predictions)
	}
	return(MboostBlackboostPredict)
}