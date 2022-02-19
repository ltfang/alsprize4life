################################################################################

PreprocessFeatures <- function(train.name="train",test.name=NULL,args,
	train.cols=NULL,train.data.std=NULL,train.data.na=NULL,
	target=NULL) {
	# Preprocesses feature data according to list of arguments in args 
	#
	# Args:
	#	train.name: base name of training data
	#	test.name: base name of test data
	# 	- Possible combinations:
	#		train.name, test.name: processes train data with all data
	#		train.name, test.name=NULL: processes train data with just train data
	#		train.name=NULL,test.name: processes test data with train.cols,
	#		train.data.std, and train.data.na, which may have been created with
	#		just train data or all data 
	#	args: list of arguments 
	#	- pls: TRUE means predicting the last score rather than the slope
	#	- prune: TRUE means keeping only features with data for at least 918/2 
	#		patients
	#	- std: TRUE means standardize columns by subtracting away column means 
	#	- features: a character vector of feature sets to get for the training 
	#		data.  Choose from "static","alsfrs","fvc","svc","vital","lab"
	#	- slope.features: a character vector of datasets to get slope features 
	#		for.  Choose from NULL if no slopes are desired, or among "static",
	#		"alsfrs","fvc","svc","vital","lab"
	#	train.cols: the names of the training data columns after pruning.  Used to 
	#		make sure the training and test data columns match
	#	train.data.std: the training data prior to standardization.  Used to 
	#		standardize test data with train data column means 
	#	train.data.na: the training data prior to imputation of NAs.  Used to impute
	#		NAs in the test data with the training data. 
	#
	# Returns: 
	#	Feature dataset with features processed according to the arguments
	#	train.cols
	#	train.data.std
	#	train.data.na
	#	target 
	#
	# Example usage:
	if (FALSE) {
		features <- c("static","alsfrs","fvc","svc","vital")
	   	slope.features <- NULL
	   	args <- list(R=1,pls=TRUE,prune=TRUE,std=TRUE,features=features,
	   	slope.features=slope.features)
	   	
	   	# Process training set on both training and test set
		trainf<-PreprocessFeatures(train.name="train",test.name="test",args)
		
		# Process test set based on processing of training set
		testf<-PreprocessFeatures(train.name=NULL,test.name="test",args,
			train.cols=trainf$train.cols,train.data.std=trainf$train.data.std,
			train.data.na=trainf$train.data.na)
	}
	
	# Extract arguments
	pls <- args[["pls"]]
	prune<-args[["prune"]]
	std<-args[["std"]]
	features<-args[["features"]]
	slope.features<-args[["slope.features"]]
	
	# Get targets for training data
	if (!is.null(train.name)) {
		if (is.null(target)) {target<-Slopes(GetAlsfrsData(train.name),3,12,"f")}
	}		

	# Get feature data
	if (!is.null(train.name)) {
		# Get training data if train name exists 
		feature.data<-GetFeatureSet(train.name,features,slope.features)		
		if (!is.null(test.name)) {
			# Stack with test data if test data is provided
			num.train.rows<-nrow(feature.data)
			test.data<-GetFeatureSet(test.name,features,slope.features)
			feature.data<-rbind(feature.data,test.data)
		}
	}
	else {
		# Get test data if train name is null
		feature.data<-GetFeatureSet(test.name,features,slope.features)
	}
	
	# Prune data
	if (prune) {
		if (!is.null(train.name)) {
			# Prune training data using training data	
			feature.data <- PruneFeatures(feature.data)
			# Keep a copy of columns at this stage
			train.cols<-colnames(feature.data)
		}
		else {
			# Prune test data based on earlier pruning of training data
			feature.data<-feature.data[,train.cols]
		}
	}
		
	# Predict on last score

	if (pls) {
		if (!is.null(train.name)) {
			scaling.train<-PlsScale(train.name)
		}
		else {
			scaling.train<-NULL
		}
		if (!is.null(test.name)) {
			scaling.test<-PlsScale(test.name)
		}
		else {
			scaling.test<-NULL
		}
		scaling<-rbind(scaling.train,scaling.test)
		feature.data[,-1]<-sweep(feature.data[,-1],1,scaling$time.scale,"*")	
		if (!is.null(train.name)) {
			target<-target+scaling.train$score.first*scaling.train$time.scale
		}
	}
		
	# Impute NAs 
	if (!is.null(train.name)) { 
		# Keep a copy of train data with NAs for imputing purposes
		train.data.na<-feature.data
	}
	feature.data<-ImputeNas(train.data.na,feature.data)

	# Standardize features
	if (std) {
		if (!is.null(train.name)) {
			# Keep copy of old train data to standardize test data features
			train.data.std<-feature.data
		}
		feature.data[,-1]<-StandardizeFeatures(train.data.std[,-1],feature.data[,-1])
	}

	# Separate out train data if both train and test data stats were used for processing
	if (!is.null(train.name)&!is.null(test.name)) {
		feature.data<-feature.data[1:num.train.rows,]
	}
	
	# Create a list with return values
	return.values<-list(feature.data=feature.data,target=target,train.cols=train.cols,
		train.data.std=train.data.std,train.data.na=train.data.na)
		
	return(return.values)
}

PlsScale <- function(train.name) {
# Gets scaling factors when predicting the last score

	# Get slope components for past and future
	slope.past<-SlopeComponents(GetAlsfrsData(train.name),0,3,"p")
	slope.future<-SlopeComponents(GetAlsfrsData(train.name),3,12,"f")
	
	# Get the first date, estimated as the last past date
	time.first<-slope.past$last.date
	# Fill in any blanks with the average 
	time.first[is.na(time.first)]<-mean(time.first,na.rm=TRUE)
	
	# Get the first score, estimated as the last past score
	score.first<-slope.past$last.score
	# Fill in any blanks with the average 
	score.first[is.na(score.first)]<-mean(score.first,na.rm=TRUE)
	
	# Get the last date, estimated as the average of all last future dates
	time.last<-rep(mean(slope.future$last.date),nrow(slope.future))
	
	# Get the real last score
	score.last<-slope.future$last.score
	
	# Get time scaling
	time.scale<-1/(time.last-time.first)
	
	# Return score.first and time.scale 
	return(data.frame(score.first,time.scale))
}
