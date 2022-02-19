################################################################################
# Dependencies
# library("mboost")

MboostGamboostTrain <- function(train.name="train",target=NULL,args=list(test.name=NULL,R=1,pls=FALSE,
	prune=FALSE,std=FALSE,features=c("static","alsfrs","fvc","svc","vital"), 
	slope.features=NULL)) {
	# Trains a MboostGamboost model given training data.
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
	   	args=list(test.name="test",R=1,pls=FALSE,prune=FALSE,std=TRUE,
	   		features=features,slope.features=slope.features)
		model <- MboostGlmboostTrain(train.name="train",target=NULL,args=args)
		predictions<-model("test")
	}

	# Preprocess training and test data according to arguments
	train.preprocess<-PreprocessFeatures(train.name=train.name,test.name=
		args[["test.name"]],args=args)

	train.data<-as.data.frame(train.preprocess$feature.data)
	
	test.preprocess<-PreprocessFeatures(train.name=NULL,
		test.name=args[["test.name"]],args=args,
		train.cols=train.preprocess$train.cols,
		train.data.std=train.preprocess$train.data.std,
		train.data.na=train.preprocess$train.data.na)

	test.data<-test.preprocess$feature.data
	
	# Remove constant columns
	train.data<-train.data[,apply(train.data,2,function(c)length(unique(c))>1)]
	test.data<-test.data[,colnames(train.data)]
	
	# Get target
	target<-train.preprocess$target
	
	# Fit multiple models to training data	
	GetModel <- function(i) {
		# Figure out how to specify the knots for each variable in an easy, programmatic
		# Look in bbs/gamboost code
		# Specify knots based on train.data and test.data (so that the entire range is captured)
		
		object<-gamboost(target~.,data.frame(target,train.data[,-1]),
			dfbase = 4,control = boost_control(mstop = 200))
		# Note: this command works if you standardize with means and sds, but it yields
		# an outlier in prediction 88
			
		# Get formula to pass into gamboost
		formula<-GetGamboostFormula(train.data,test.data,nknots=5)
		object<-gamboost(as.formula(formula),data.frame(target,train.data[,-1]),
			dfbase = 4,control = boost_control(mstop = 200))
											
		# Select optimal number of boosting iterations using CV
		cv.err<-cvrisk(object, folds = cv(model.weights(object),type="kfold"))
		# Set optimal boosting iteration (this looks strange, but it is what it does)
		object[mstop(cv.err)]
		
		pred<-predict(object,newdata=data.frame(test.data[,-1]))

		# Report summary results
		print(sprintf("Model: %d, CV error: %g, Best iter: %g",i,sqrt(min(colMeans(cv.err[,]))),mstop(cv.err)))
		
		# predictions <- predict(object,data.frame(test.data[,-1]))	
		# Rmsd(pred,test.target)	
	
		return(list(object = object))
	}
	models <- lapply(1:args[["R"]], GetModel)
	
	MboostGamboostPredict <- function(test.name="test",save.preds=TRUE) {
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

		# Form test predictions
		predictions <- rowMeans(sapply(models, function(model) 
			predict(model$object,test.data[,-1])))
		
		# If predicting last score, transform predictions back to slopes
		if (args[["pls"]]) {
			predictions<-predictions-
				scaling$score.first*scaling$time.scale
		}
		predictions <- cbind(test.data[,1],predictions)
		
		if(save.preds) {
			# Write predictions to file
			out.file = GetPredictionFilename("MboostGamboostTrain",train.name,test.name,args)
			WritePredictions(predictions, out.file)
		}
		return(predictions)
	}
	return(MboostGamboostPredict)
}

GetGamboostFormula<-function(train.data,test.data,nknots) {
	# Drop constant columns and stack data
	all.data<-rbind(train.data,test.data)

	# Get ranges for each variable
	var.max<-apply(all.data,2,max)
	var.min<-apply(all.data,2,min)

	# Create list of bbs formula components
	forms<-lapply(2:ncol(train.data),BbsFormula,train.data,var.max,var.min,nknots) 
	formula<-paste(forms,collapse="+")
	formula<-paste("target~",formula,sep="")
	return(formula)
}

BbsFormula<-function(i,train.data,var.max,var.min,nknots) {
	# Construct bbs formula for one variable as a string
	form<-sprintf("bbs(train.data[,%d],knots=%d,boundary.knots=c(%g,%g))",
		i,nknots,var.min[i],var.max[i])	
	return(form)
}
