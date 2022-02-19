StandardizeFeatures<-function(training.set,test.set=NULL,stddev=FALSE) {
	# Subtracts away mean feature vector in each column, given a dataset
	# (assumes no subject.id column); can also divide by std deviation
	#
	# Args:
	#	training.set: dataset of training variables
	#	test.set: dataset of test variables
	#	- if NULL, training set is standardized with training variables 
	#	- if given, test set is standardized with training variables 
	#	sd: switch for whether standard deviations are divided
	#
	# Returns:
	#	  a dataset standardized columns
  
  	# Calculate mean and sd vector
	mean<-apply(training.set,2,mean,na.rm=TRUE)
	sd<-apply(training.set,2,sd,na.rm=TRUE)
  
 	if (is.null(test.set)) {
  		training.set<-sweep(training.set,2,mean,"-")
  		if (!stddev) {
  			return(training.set)
  		}
  		else {
  			return(sweep(training.set,2,sd,"/"))
  		}
	}
	else {
		test.set<-sweep(test.set,2,mean,"-")
		if (!stddev) {
			return(test.set)
		}
		else {
			 return(sweep(test.set,2,sd,"/"))
		}
	}
}

#mean slope: -5.325687e-01
#sd slope: 1.076365e+00 
