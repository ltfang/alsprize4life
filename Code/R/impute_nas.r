################################################################################

ImputeNas <- function(training.set,test.set=NULL) {
	# Imputes NAs in a training set or test set of features
	#
	# Args:
	#	training.set: dataset with training features
	#	test set: dataset with test features; if NULL, will impute NAs for the 
	#	training set
	#
	# Returns: 
	#	A dataset with NAs filled in 
	#
	# Example usage:
	# 	train.data[,1]<-ImputeNas(train.data[,-1])
		
	#Compute column medians
	col.names<-colnames(training.set)
	medians<-apply(training.set,2,median,na.rm=TRUE)
	
	# Replace NAs with column medians in training set if test set is NULL
	if (is.null(test.set)) {
		training.set<-sapply(1:ncol(training.set),ColReplace,training.set,medians)
		colnames(training.set)<-col.names
		# Keep only columns that have no NAs		
		na.col<-apply(training.set,2,function(x) {any(is.na(x))})
		training.set<-training.set[,!na.col]
		return(training.set)
	}
	
	# Otherwise replace NAs with training set column medians in test set
	else {
		nrows <- nrow(test.set)
		ncols <- ncol(test.set)
		# Reshape test set into data frame of the correct dimension after replacing NAs
		test.set<-data.frame(matrix(sapply(1:ncol(test.set),ColReplace,test.set,medians),nrow=nrows,ncol=ncols))
		colnames(test.set)<-col.names		
		# Keep only columns that have no NAs		
		na.col<-apply(test.set,2,function(x) {any(is.na(x))})
		test.set<-test.set[,!na.col]
		return(test.set)
	}
	
}

# Replace NA function to be used in apply functions
ColReplace <- function(i,data,medians) {
	data[,i][is.na(data[,i])]<-medians[i]
	return(data[,i])
}
