################################################################################

# Dependencies
AvgPredsCv <- function(train.name,file.list,num.cv) {
	
	# Calculates the average predictions for a set of cv splits
	#
	# Args: 
	#	train.name: base name of training data from which splits are made
	# 	file.list: a list, where each element is a list of method args used for 
	#   	each prediction to be averaged and the name of each element is the  method
	#	num.cv: number of cv splits
	#
	# Returns:
	#	 A list of averaged predictions for each cv split
	#
	# Side effects:
	# 	Writes average prediction to each cv split to a separate file 
	# 
	# Example usage:
	# method.args1<-list(R=1,corr.bias=FALSE,pls=FALSE,prune=FALSE,std=FALSE,
	# features=c("static","alsfrs","fvc","svc","vital"),slope.features=NULL)
	# 
	# method.args2<-list(R=1,pls=FALSE, prune=FALSE,std=FALSE,
	# features=c("static","alsfrs","fvc","svc","vital"), slope.features=NULL)
	#
	# file.list<-list("RandomForestTrain"=method.args1,"GlmnetTrain"=method.args2)
	#
	# avg.preds<-AvgPredsCv("train",file.list,10)

	# Get vector of train names corresponding to cv splits
	train.names<-paste(train.name,"-cvtrain_",1:num.cv,"of",num.cv,sep="")
	
	# Get vector of test names corresponding to cv splits
	test.names<-paste(train.name,"-cvtest_",1:num.cv,"of",num.cv,sep="")
	
	# Average predictions for each cv split
	avg.preds<-mapply(AvgPredictions,train.names,test.names,list(file.list),SIMPLIFY=FALSE)
	
	return(avg.preds)
	
}
AvgPredictions<- function(train.name,test.name,file.list=list()) {
	
	# Calculates the average prediction based on a list of prediction files
	#
	# Args: 
	#	train.name: base name of training data
	#	test.name: base name of test data
	# 	file.list: a list, where each element is a list of method args used for 
	#   	each prediction to be averaged and the name of each element is the  method
	#
	# Returns:
	#	  A matrix consisting of the subject ids and the corresponding averaged 
  	#   predictions 
	#
	# Side effects:
	# 	Writes to a file in the predictions folder
	#
	# Example usage:   
		 # method.args1<-list(R=1,corr.bias=FALSE,pls=FALSE,prune=FALSE,std=FALSE,
		# features=c("static","alsfrs","fvc","svc","vital"),slope.features=NULL)
	  
		# method.args2<-list(R=1,pls=FALSE, prune=FALSE,std=FALSE,
		# features=c("static","alsfrs","fvc","svc","vital"), slope.features=NULL)
	
		# file.list<-list("RandomForestTrain"=method.args1,"GlmnetTrain"=method.args2)
	
 	  #avg.pred<-AvgPredictions("train-cvtrain_1of10","train-cvtest_1of10",file.list)		

	# Get subject.id
	subject.id<-GetSubjectIds(test.name)
		
  # Get prediction data from each set of arguments
	predictions<-sapply(names(file.list),GetPredictionData,train.name,
    test.name,file.list)
  
  # Take average
	avg.pred<-rowMeans(predictions)
	
  # Add subject ids
	avg.pred<-cbind(subject.id,avg.pred)
	
  # Write to file 
	out.file <- GetPredictionFilename("Avg",train.name,test.name,file.list)
	WritePredictions(avg.pred, out.file)
	
	return(avg.pred)
}

GetPredictionData <- function(method.name,train.name,test.name,file.list) {

	element<-file.list[[method.name]]
	# Get the prediction filename given a list element with all arguments 
	filename<-GetPredictionFilename(method.name,train.name,test.name,
		element)
	
	# Read in the predictions and keep prediction column
	predictions<-read.csv(filename,header=FALSE,sep=",")
	
	return(predictions[,2])
}




