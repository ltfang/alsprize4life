################################################################################

GetFeatureSet <- function(raw.data.name,features,slope.features) {
	# Gets training data given a name of the raw train data and the features 
	# to add
	#
	# Args:
	#	raw.data.name: base name of dataset
	#	features: a character vector of datasets to get features from
	#		Choose from "static","alsfrs","fvc","svc","vital","lab"
	#	slope.features: a character vector of datasets to get slopes for 
	#		Choose from NULL, "alsfrs","fvc","svc","vital","lab"
	# Returns: 
	#	A matrix containing the features
	#
	# Example usage:
	# 	train.data <- GetFeatureSet("train",c("static","alsfrs"),c("static","alsfrs"))
	
	# Initialize empty list
	feature.list<-list()
	
	# Add static features
	if ("static" %in% features) {
		feature.list<-c(feature.list,list(GetStaticData(raw.data.name)))
	}
	
	# Add ALSFRS features
	if ("alsfrs" %in% features) {
		feature.list<-c(feature.list,list(GetAlsfrsFeatures(raw.data.name)))
	}

	# Add FVC features
	if ("fvc" %in% features) {
		feature.list<-c(feature.list,list(GetFvcFeatures(raw.data.name)))
	}

	# Add SVC features
	if ("svc" %in% features) {
		feature.list<-c(feature.list,list(GetSvcFeatures(raw.data.name)))
	}

	# Add vital features
	if ("vital" %in% features) {
		feature.list<-c(feature.list,list(GetVitalFeatures(raw.data.name)))
	}

	# Add lab features
	if ("lab" %in% features) {
		feature.list<-c(feature.list,list(GetLabFeatures(raw.data.name)))
	}

	# Add uric features
	if ("uric" %in% features) {
		feature.list<-c(feature.list,list(GetUricFeatures(raw.data.name)))
	}	
	# Add slope features
	
	if (!is.null(slope.features)) {
		feature.list<-c(feature.list,list(GetSlopeFeatures(raw.data.name,slope.features)))
	}
	

	feature.data<-Reduce(function(x,y) {merge(x,y,by="subject.id",all.x=TRUE)}, feature.list)
	
	# Change column names to be compatible with data frames
	colnames(feature.data)<-colnames(data.frame(feature.data))
	
	return(feature.data)
}
