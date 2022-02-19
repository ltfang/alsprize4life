################################################################################
# library("glmnet")
# library("randomForest")
# library("gbm")

ResidualTrain <- function(train.name,old.method,old.args,new.method,new.args) {
	# Creates a model for predicting residuals
	#
	# Args:
	#	train.name: base name of training dataset
	#	old.method: first method used to make predictions (string)
	#	old.args: list of arguments passed to first method to make predictions 
	#	new.method: method to be used for training on residuals (string)
	#	new.args: list of arguments passed to new method to predict on residuals
	#
	# Returns:
	#	A model for predicting residuals on test data
	#
	# Example usage:
	#	old.args<-list(test.name="test",R=1,corr.bias=FALSE,pls=FALSE,prune=FALSE,
	#		std=FALSE,features=c("static","alsfrs","fvc","svc","vital"), 
	#		slope.features=NULL)
	#	new.args=list(test.name="test",R=1,pls=FALSE,prune=FALSE,std=FALSE,
	#		features=c("static","alsfrs","fvc","svc","vital","lab"), 
	#		slope.features=NULL)
	#	model<-ResidualTrain("train",old.method="RandomForestTrain",
	#		old.args=old.args,new.method="GbmTrain",new.args=new.args)
	# 	predictions<-model("test")
	
	# Get test name
	test.name<-old.args[["test.name"]]
	
	# Get filename of old prediction
	old.train.file<-GetPredictionFilename(old.method,train.name,train.name,old.args)
	
	# If it exists, load it
	old.preds.exist<-file.exists(old.train.file)
	if (old.preds.exist) {
		print(paste("loading first method prediction from",old.train.file))
		old.train.pred<-read.csv(old.train.file,sep=",",header=FALSE)
	}
	# Otherwise get the model and predictions
	else {
		print("Running first method")
		method.old<-get(old.method)
		old.model<-method.old(train.name,args=old.args)
		old.train.pred<-old.model(train.name)
	}
			
	# Initialize training targets
	target<-Slopes(GetAlsfrsData(train.name),3,12,"f")		
	
	# Subtract predictions from target to get residuals
	new.target<-target-old.train.pred[,2]
	
	# Train on residuals with method
	method.new<-get(new.method)
	new.model<-method.new(train.name,target=new.target,args=new.args)
	
	ResidualPredict <- function(test.name="test",save.preds=TRUE) {
	# Makes predictions for residuals
	#	
	# Args:
	#	test.name: base name of test data
	#	save.preds: save predictions to file? TRUE by default
	#
	# Returns:
	#	Two column matrix, ordered by the first column, with first column 
	#   "subject.id" containing test set subject ids and the second column 
	#   "prediction" containing predicted slopes.
	#
	# Side effects:
	#	Saves predictions to file
		
		# Get old test file name
		old.test.file<-GetPredictionFilename(old.method,train.name,test.name,old.args)
		
		# If there was no old training prediction file or no test prediction file,
		# create the old predictions for the test data
		if (!old.preds.exist|!file.exists(old.test.file)) {
			old.test.pred<-old.model(test.name,save.preds=TRUE)
		}
		
		# If old test file exists, read in
		else {
			old.test.pred<-read.csv(old.test.file,sep=",",header=FALSE)
		}
		
		# Form predictions by adding old test predictions to test residual predictions
		predictions<-old.test.pred[,2]+new.model(test.name,save.preds=FALSE)[,2]
		
		predictions<-cbind(old.test.pred[,1],predictions)
		colnames(predictions)<-c("subject.id","predictions")

		if(save.preds) {
			# Write predictions to file
			out.file = GetPredictionFilename("Residual",train.name,test.name,
				args=c(old.method,old.args,new.method,new.args))
			WritePredictions(predictions, out.file)
		}
	
		return(predictions)
	}
	
	return(ResidualPredict)
}	

