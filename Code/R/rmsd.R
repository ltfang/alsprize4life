
Rmsd<-function(x,y) {
	# Calculates RMSD between two matrices or vectors of equal size
	#
	# Args:
	#	true: ground truth file
	#	predicted: prediction file
	#
	# Returns:
	# 	RMSD of predictions
	#
	RMSD<-sqrt(mean((x-y)^2))
	return(RMSD)
}


FileRmsd<-function(true,predicted) {
	# Calculates RMSD given a ground truth file and prediction file
	#
	# Args:
	#	true: ground truth file
	#	predicted: prediction file
	#
	# Returns:
	# 	RMSD of predictions
	#
	# Example usage:
	#	source("Code/file_rmsd.R")
	#	error<-Rmsd("GroundTruth-train-cvtest_1of10","Avg--Lasso-train-cvtrain_1of10-train-cvtest_1of10-randomForest-train-cvtrain_1of10-train-cvtest_1of10")
	true_data<-read.csv(paste("Predictions/",true,".txt",sep=""),sep=",",header=FALSE)
	colnames(true_data)<-c("subject_id","true_slope")
	predicted_data<-read.csv(paste("Predictions/",predicted,".txt",sep=""),sep=",",header=FALSE)
	colnames(predicted_data)<-c("subject_id","predicted_slope")
	all_data<-merge(true_data,predicted_data,all=TRUE)
	RMSD<-Rmsd(all_data$true_slope,all_data$predicted_slope)
	print(paste("RMSD:",RMSD))
	return(RMSD)
}

FileArgsRmsd <- function(method,train.name,test.name,method.args) {
	# Calculates RMSD given a set of arguments used to create the prediction 
	#
	# Args:
	#	method: name of method file used to create predictions
	#	train.name: base name of training set
	#	test.name: base name of test set
	#	method.args: arguments passed into method to make prediction 
	#
	# Returns:
	# 	RMSD of predictions
	#
	# Example usage:
	#	rmsd<-FileArgsRmsd("RandomForestTrain","train-cvtrain_1of10","train-cvtest_1of10",method.args=list(R=1,corr.bias=FALSE,pls=FALSE,prune=FALSE,std=FALSE,features=c("static","alsfrs","fvc","svc","vital"), slope.features=NULL))

	# Get name of ground truth file
	truth.file <- paste("Predictions/GroundTruth-",test.name,".txt",sep="")
	
	# Get name of prediction file
	pred.file<-GetPredictionFilename(method,train.name,test.name,method.args)

	# Get ground truth data
	truth<-read.csv(truth.file,sep=",",header=FALSE)
	
	# Get prediction data
	pred<-read.csv(pred.file,sep=",",header=FALSE)
	
	# Calculate RMSD
	RMSD<-Rmsd(truth[,2],pred[,2])
	
	print(paste("RMSD:",RMSD))
	
	return(RMSD)
}

