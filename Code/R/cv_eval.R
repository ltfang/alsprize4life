################################################################################

CvEval<-function(method,raw.data.name,args=list(),num.cv=10) {
	# Computes cv errors for a given prediction method
	#
	# Args:
	#	method: 
	#	raw.data.name:
	#	method.args
	#	num.cv
	#
	# Returns:
	# 	Cross validation error
	#
	# Example usage:
	#	error<-CvEval(RandomForestTrain,"train")
	
	# Generate cv splits
	GenCvData(raw.data.name,num.cv)

	# Get the ground truth prediction model	
	model.truth<-GroundTruthTrain()

	TrainTestRmsd <- function(fold) {
		# For a given CV fold, trains method on training set and computes the RMSD between method's 
		# predictions on the test set and the ground truth on the test set
		
		# Construct train and test set names
		train.name <- sprintf("%s-cv%s_%sof%d", raw.data.name, "train",fold,num.cv)
		test.name <- sprintf("%s-cv%s_%sof%d", raw.data.name, "test",fold,num.cv)
		# Train method and compute error between ground truth and method predictions
		return(Rmsd(model.truth(test.name)[,2], method(train.name,args=args)(test.name)[,2]))
	}
	
	# Get the test errors of method on each train-test split
	split.rmse <- sapply(1:num.cv,TrainTestRmsd)
	print(split.rmse)
	
	# Average MSEs and take square root
	cv.rmse<-sqrt(mean(split.rmse^2))
	print(cv.rmse)
	return(cv.rmse)
}

FileCvEval <- function(method,train.name,method.args,num.cv) {
	# Calculates CV error across a number of cv splits
	#
	# Args:
	#	method: name of method file used to create predictions
	#	train.name: base name of training data used to make splits
	#	method.args: arguments passed into method to make prediction 
	#	num.cv: number of splits 
	#
	# Returns:
	# 	Average RMSD over all cv splits 
	#
	#################################################################
	# Example usage:
	if(FALSE) {
		# Evaluate randomforest
		args <- list(R=1,corr.bias=FALSE,pls=FALSE,prune=FALSE,std=FALSE,features=c("static","alsfrs","fvc","svc","vital"), slope.features=NULL)
		cv.rmsd<-FileCvEval("RandomForestTrain","train",method.args=args,num.cv=10)
		
		# Evaluate glmnet
		args <- list(R=1,pls=FALSE,prune=FALSE,std=FALSE,features=c("static","alsfrs","fvc","svc","vital"), slope.features=NULL)
		cv.rmsd<-FileCvEval("GlmnetTrain","train",method.args=args,num.cv=10)
		
		# Evaluate gbm
		args <- list(R=1,pls=FALSE,prune=FALSE,std=FALSE,features=c("static","alsfrs","fvc","svc","vital"), slope.features=NULL)
		cv.rmsd<-FileCvEval("GbmTrain","train",method.args=args,num.cv=10)

		# Evaluate bagging
		args <- list(R=1,pls=FALSE,prune=FALSE,std=FALSE,features=c("static","alsfrs","fvc","svc","vital"), slope.features=NULL)
		cv.rmsd<-FileCvEval("BaggingTrain","train",method.args=args,num.cv=10)
	

		# Evaluate neural net
		args <- list(R=1,pls=FALSE,prune=TRUE,std=FALSE,features=c("static","alsfrs","fvc","svc","vital"), slope.features=NULL)
		cv.rmsd<-FileCvEval("NnetTrain","train",method.args=args,num.cv=10)
	}
	#################################################################
	# Example usage with residual train:
	if(FALSE) {
		old.method <- "GlmnetTrain"
		old.args <- list(R=1,pls=FALSE,prune=TRUE,std=FALSE,features=c("static","alsfrs","fvc","svc","vital"), slope.features=NULL)
		new.method <- "GbmTrain"
		new.args <- list(R=1,pls=FALSE,prune=TRUE,std=FALSE,features=c("static","alsfrs","fvc","svc","vital"), slope.features=NULL)
	 	cv.rmsd<-FileCvEval("Residual","train",method.args=c(old.method,old.args,new.method,new.args),num.cv=10)
	}
	#################################################################
	
	# Get vector of train names corresponding to cv splits
	train.names<-paste(train.name,"-cvtrain_",1:num.cv,"of",num.cv,sep="")
	
	# Get vector of ttest names corresponding to cv splits
	test.names<-paste(train.name,"-cvtest_",1:num.cv,"of",num.cv,sep="")
	
	# Calculate rmses of each train/test split
	rmses<-mapply(FileArgsRmsd,method,train.names,test.names,list(method.args))
	
	# Average MSEs and take square root
	cv.rmse<-sqrt(mean(rmses^2))

	print(paste("CV RMSE:",cv.rmse))
	return(cv.rmse)

}
