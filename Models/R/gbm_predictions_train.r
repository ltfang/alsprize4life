################################################################################

GbmPredictionsTrain<- function(train.name="train",method.list,num.cv=10) {
	
	# Calculates GBM combinations based on the cv prediction files made 
	#	by other methods, ie, regresses cv test predictions on the truth
	#	(Note: GetPredictionData function is in the average_predictions file)
	#
	# Args: 
	#	train.name: base name of training data from which splits are created 
	# 	method.list: a list, where each element is a list of method args used for 
	#   	each prediction to be averaged and the name of each element is the  method
	#
	# Returns:
	#	  A GBM model
	#
	# Example usage:
	if(FALSE) {
		features <- c("static","alsfrs","fvc","svc","vital")
		slope.features <- NULL
		glm.args <- list(R=1,pls=FALSE,prune=FALSE,std=FALSE,features=features,
			slope.features=slope.features)
		rf.args <- c(corr.bias=FALSE, glm.args)
		gbm.args <- glm.args
		bag.args <- glm.args
		# glm.prune.args <- glm.args; glm.prune.args$prune = TRUE
		# rf.prune.args <- rf.args; rf.prune.args$prune = TRUE
		# gbm.prune.args <- glm.prune.args
		# bag.prune.args <- glm.prune.args

		method.list<-list(
			# "RandomForestTrain"=rf.prune.args,
			# "GlmnetTrain"=glm.prune.args,
			# "GbmTrain"=gbm.prune.args,
			# "BaggingTrain"=bag.prune.args,
			"RandomForestTrain"=rf.args,
			"GlmnetTrain"=glm.args,
			"GbmTrain"=gbm.args,
			"BaggingTrain"=bag.args
		)
 
		model<-GbmPredictionsTrain("train", method.list)
		predictions<-model("test")
	}
	############################################################################
	
	# Get vector of train names corresponding to cv splits
	train.names<-paste(train.name,"-cvtrain_",1:num.cv,"of",num.cv,sep="")
	
	# Get vector of test names corresponding to cv splits
	test.names<-paste(train.name,"-cvtest_",1:num.cv,"of",num.cv,sep="")

	# Function that gets prediction data from each set of arguments for one cv split
	GetCvPredictions<-function(train.name.cv,test.name.cv,method.list) {
		return(sapply(names(method.list),GetPredictionData,train.name.cv,
			test.name.cv,method.list))
	}
    # Get prediction data for all cv splits
    train.preds<-mapply(GetCvPredictions,train.names,test.names,list(method.list))
    
    # Stack prediction data
    train.preds<-do.call(rbind,train.preds)
    
    # Get subject ids in same order
    subject.ids<-do.call(rbind,lapply(test.names,GetSubjectIds))
    
    # Put together prediction data with subject ids
    train.preds<-cbind(subject.ids,train.preds)
    
    # Sort by subject id
    train.preds<-train.preds[order(train.preds$subject.id),]
    
    # Get targets
    target<-Slopes(GetAlsfrsData(train.name),3,12,"f")
    
    # Use Gbm on predictions     	
   	object <- gbm(target~.,data=train.preds[,-1],distribution="gaussian",
   		n.trees=20000,cv.folds=10,verbose=FALSE)
    
    # Find optimal number of iterations
    best.iter<-gbm.perf(object,method="cv",plot.it=FALSE)
    
    # Report relative influence
    print(summary(object,plot=FALSE))
    # Report CV internal error
    print(paste("Internal CV error:",sqrt(min(object$cv.error))))
    
    # Report error on our CV splits
    train.err <- Rmsd(target, predict(object,train.preds[,-1],best.iter))
    
    print(paste("Error on our CV splits:",train.err))
    
    return(list(object = object, param = best.iter))
   
	GbmPredictionsPredict<- function(test.name="test",save.preds=TRUE) {
		#	Makes predictions based on a ridge regression on predictions from
		#	other methods
		#
		# Args: 
		#	test.name: base name of test data 
		# 	save.preds: saves to file if TRUE
		#
		# Returns:
		#	 A matrix of subject ids and corresponding predictions
		#
		# Side effects:
		# 	Writes to a file in the predictions folder
	
		# Get test prediction data
		test.preds<-sapply(names(method.list),GetPredictionData,train.name,
			test.name,method.list)
	       
       	# Make test predictions
       	predictions<-predict(object,test.preds,param)
		
		# Get subject ids for test set
		subject.ids<-GetSubjectIds(test.name)
		
		predictions <- cbind(subject.ids,predictions)
	
		if(save.preds) {
			# Write predictions to file
			out.file = GetPredictionFilename("GbmPredictionsTrain",train.name,
				test.name,method.list)
			WritePredictions(predictions, out.file)
		}
	
		return(predictions)
	}

	return(GbmPredictionsPredict)
}





