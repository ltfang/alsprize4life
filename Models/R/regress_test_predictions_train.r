################################################################################
# library(glmnet)

RegressTestPredictionsTrain<- function(method.list,train.name="train",test.name="test") {
	
	# Calculates ridge regression weights based on the test prediction files made 
	#	by other methods, ie, regresses test predictions on the truth
	#	(Note: GetPredictionData function is in the avg_predictions file)
	#
	# Args: 
	#	train.name: base name of training data from which splits are created 
	# 	method.list: a list, where each element is a list of method args used for 
	#   	each prediction to be averaged and the name of each element is the  method
	#
	# Returns:
	#	  A ridge regression model 
	#
	# Example usage:
	if(FALSE) {
		features <- c("static","alsfrs","fvc","svc","vital")
		slope.features <- c("alsfrs","fvc","svc","vital")
		
		glm.args <- list(test.name="test",R=1,pls=FALSE,prune=FALSE,std=FALSE,
						features=features,slope.features=slope.features)
		rf.args <- c(corr.bias=FALSE, glm.args)
		gbm.args <- glm.args
		bag.args <- glm.args
		bart.args <- glm.args
		mblack.args <- glm.args
		mglm.args <- glm.args

		bart10.args <- glm.args; bart10.args$R = 10
		
		glm.prune.args <- glm.args; glm.prune.args$prune = TRUE
		rf.prune.args <- rf.args; rf.prune.args$prune = TRUE
		gbm.prune.args <- glm.prune.args
		bag.prune.args <- glm.prune.args
		mblack.prune.args <- glm.prune.args
		mglm.prune.args <- glm.prune.args

		method.list<-list(
			"BartTrain"=bart10.args,
			#"BaggingTrain"=bag.args,
			"BartTrain"=bart.args,
			#"GbmTrain"=gbm.args,
			"GlmnetTrain"=glm.args,
			"MboostBlackBoostTrain"=mblack.args,
			"MboostGlmBoostTrain"=mglm.args,
			"RandomForestTrain"=rf.args,
			"GlmnetTrain"=glm.prune.args,
			"RandomForestTrain"=rf.prune.args,
			"MboostBlackBoostTrain"=mblack.prune.args,
			"MboostGlmBoostTrain"=mglm.prune.args
			#"GbmTrain"=gbm.prune.args,
			#"BaggingTrain"=bag.prune.args,
		)
 
		model<-RegressTestPredictionsTrain(method.list=method.list)
		predictions<-model("test")
	}
	#######################################################################

    # Get prediction data for each method
    num.methods <- length(method.list)
	train.preds<-sapply(1:num.methods,function(i,train.name,test.name,method.list) 
		GetPredictionData(names(method.list)[i],train.name,test.name,method.list[i:num.methods]),
		train.name,test.name,method.list)
	colnames(train.preds) <- names(method.list)
				
    # Get subject ids in same order
    subject.ids<-do.call(rbind,lapply(test.name,GetSubjectIds))
    
    # Put together prediction data with subject ids
    train.preds<-cbind(subject.ids,train.preds)
    
    # Add interactions
    # for (i in 2:num.methods) {
    	# for (j in (i+1):(num.methods+1)) {
    		# inter<-train.preds[,i]*train.preds[,j]
	    	# train.preds<-cbind(train.preds,inter)
		# }
    # }

    # Sort by subject id
    train.preds<-train.preds[order(train.preds$subject.id),]
    
    # Convert to numeric matrix
	train.preds <- as.matrix(train.preds)
	mode(train.preds) <- "numeric"

    # Get TEST targets
    target<-Slopes(GetAlsfrsData(test.name),3,12,"f")
    
    # Ridge regression 
    cv.res <- cv.glmnet(as.matrix(train.preds[,-1]), target, lambda=seq(0,1,.001), alpha=0, nfolds=nrow(train.preds))
    #cv.res <- cv.glmnet(as.matrix(train.preds[,-1]), target, alpha=0, nfolds=nrow(train.preds))
       
    print(paste("Internal CV error:",sqrt(min(cv.res$cvm))))
    err <- Rmsd(target, predict(cv.res$glmnet.fit,as.matrix(train.preds[,-1]),s=cv.res$lambda.min))
    print(paste("Regression Error on test data:",err))
    print(coef(cv.res))
    print(paste("lambda:",cv.res$lambda.min))
   
	RegressTestPredictionsPredict<- function(test.name="test",save.preds=TRUE) {
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
		test.preds<-sapply(1:num.methods,function(i,train.name,test.name,method.list) 
			GetPredictionData(names(method.list)[i],train.name,test.name,method.list[i:num.methods]),
			train.name,test.name,method.list)
		
		# Add interactions
	    for (i in 2:num.methods) {
    		for (j in (i+1):(num.methods+1)) {
    			inter<-test.preds[,i]*test.preds[,j]
	    		test.preds<-cbind(test.preds,inter)
			}
    	}
    
    	# Convert to numeric matrix
    	test.preds <- as.matrix(test.preds)
		mode(test.preds) <- "numeric"
   
       	# Make test predictions
       	predictions<-predict(cv.res$glmnet.fit,test.preds,s=cv.res$lambda.min)
		
		# Get subject ids for test set
		subject.ids<-GetSubjectIds(test.name)
		
		predictions <- cbind(subject.ids,predictions)
	
		if(save.preds) {
			# Write predictions to file
			out.file = GetPredictionFilename("RegressTestPredictionTrain",train.name,test.name,method.list)
			WritePredictions(predictions, out.file)
		}
	
		return(predictions)
	}

	return(RegressTestPredictionsPredict)
}





