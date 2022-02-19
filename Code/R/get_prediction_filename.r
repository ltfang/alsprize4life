################################################################################
GetPredictionFilename <- function(method,train.name,test.name,args=list()) {
	# Creates a hash value for a set of arguments passed into a model function
	#
	# Args:
	#	method: name of the method used
	#		E.g., "RandomForest", "Glmnet" 
	#	train.name: base name of training data
	#	test.name: base name of test data
	#	args: a list of arguments passed into a model function 
	#
	# Returns: 
	#	Hash value for appending to filename
	#
	# Example usage:
	# 	hashed.args <- HashArgNames(args=list(R=1,corr.bias=FALSE,pls=FALSE,
	#		prune=FALSE,std=FALSE,features=c("static","alsfrs","fvc","svc",
	#		"vital","lab"), slope.features=NULL)))
	
	args<-SuperSort(args)
	hashed.args<-digest(args)
	file.name = sprintf("Predictions/%s-%s-%s-%s.txt",method,train.name,test.name,
		hashed.args)	
	return(file.name)
}

SuperSort <- function(args.list) {
	SubSort <- function(element) {
		if(is.list(element)) {
			SuperSort(element)
		}
		else {
			element<-sort(element)
		} 
		return(element)
	}

	args.list<-args.list[order(names(args.list))]
	args.list<-lapply(args.list,SubSort)
	return(args.list)
}

