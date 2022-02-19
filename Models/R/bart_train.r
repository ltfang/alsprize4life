################################################################################
# Dependencies
# library("BayesTree")

BartTrain <- function(train.name="train",target=NULL,args=list(test.name=NULL,R=1,
	pls=FALSE,prune=FALSE,std=FALSE,features=c("static","alsfrs","fvc","svc",
	"vital"), slope.features=NULL)) {
	# Trains a Bayes Tree regression model to given training data and returns a
	# function that returns predictions given test data
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
	   	train.name="traintest"
	   	test.name="validation"
		features = c("static","alsfrs","fvc","svc","vital")
	   	slope.features = c("alsfrs","fvc","svc","vital")
	   	args=list(test.name=test.name, R=10,pls=FALSE,prune=FALSE,std=FALSE,
	   		features=features,slope.features=slope.features)
		model <- BartTrain(train.name=train.name,target=NULL,args=args)
		predictions<-model(test.name=test.name)
	}

	# Preprocess training data according to arguments
	train.preprocess<-PreprocessFeatures(train.name=train.name,
		test.name=args[["test.name"]],args=args)
		
	# Get training data and target
	train.data<-train.preprocess$feature.data
	
	target<-train.preprocess$target
	
	# Drop constant columns		
	train.data<-train.data[,apply(train.data,2,function(c)length(unique(c))>1)]

	BartPredict <- function(test.name="test",save.preds=TRUE) {
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
			train.preprocess$train.data.std,
			train.data.na=train.preprocess$train.data.na)

		test.data<-test.preprocess$feature.data
		
		# Match train columns
		test.data<-test.data[,colnames(train.data)]
		
		# Obtain test predictions from R BART models
		predictions<- matrix(sapply(1:args[["R"]], 
			function(i) { object<-bart(x.train=train.data[,-1],y.train=target,
			x.test=test.data[,-1],ntree=100,keeptrainfits=FALSE,printevery=1000,
			ndpost=10000,sigquant=0.5); return(object$yhat.test.mean) }),ncol=args[["R"]])
			
		# Compute average model prediction
		predictions <- rowMeans(predictions)
		# If predicting last score, transform predictions back to slopes
		if (args[["pls"]]) {
			scaling<-PlsScale(test.name)
			predictions<-predictions-
				scaling$score.first*scaling$time.scale
		}
		predictions <- cbind(test.data[,1],predictions)
	
		if(save.preds) {
			# Write predictions to file
			out.file = GetPredictionFilename("BartTrain",train.name,test.name,args)
			WritePredictions(predictions, out.file)
		}
	
		return(predictions)
	}
	
	return(BartPredict)
}

