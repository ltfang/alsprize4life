################################################################################################
# Dependencies
#source("Code/get_svc_data.r")
#source("Code/get_raw_data.r")
#source("Code/slopes.r")
#source("Code/past_stat.r")

GetDefaultFeatures <- function(raw.data.name,data,measure) {
	
	# For a specified dataset, returns a dataset containing default static features derived from 
	# a given measure for each subject id.  These features are the max, min, mean, number, sum, 
	# and time of first and last visits. 
	#
	# Args: 
	# 	raw.data.name: name of raw dataset	
	#	data: a dataset derived from a "get" function (e.g., GetAlsfrsData) on raw.data.name that has 	#	the columns subject.id, the relevant delta, the measure of interest  
	#	measure: name of the measure.  Should be one of: "alsfrs.score", "fvc.liters", 
	#	"svc.liters", "weight", "height", "resp.rate", "bp.systolic", "bp.diastolic"
	#
	# Returns:
	#	A dataset in which for each row corresponds to a unique subject.id.  
	#	The columns consist of subject.id and features. 
	#
	# Example usages:
	#	#source("Code/get_features.R") 
	#	svc.features<-GetDefaultFeatures("train",GetSvcData("train"),"svc.liters")
	#	or
	#	data<-GetVitalData("train")
	#	weight<-as.data.frame(data$weight.data)
	# 	weight.features<-GetDefaultFeatures("train",weight,"weight")

	# Add unique subject ids for this raw dataset to features
	features<-GetSubjectIds(raw.data.name)
	
	# Get the name of the delta variable
	delta<-names(data)[grep("delta",names(data))]
	
	# Get column index of the delta field
	delta.col<-grep("delta",names(data))
  
	# Delete all rows where delta>91
	data<-data[data[,delta.col]<=91,]
	
    # Create a list with one element per subject id containing all data
	split.data<-split(data,data$subject.id)

	# Add maximum past value to features
	features <- merge(features, PastStat(split.data,measure,max,paste("max",measure,sep=".")), by="subject.id", all.x=TRUE)
	# Add minimum past value to features
	features <- merge(features, PastStat(split.data,measure,min,paste("min",measure,sep=".")), by="subject.id", all.x=TRUE)
	# Add latest past value to features 
	last.measure<-paste("last",measure,sep=".")
	features <- merge(features, PastStat(split.data,measure,last,last.measure), by="subject.id", all.x=TRUE)
	# Add mean past value to features
	features <- merge(features, PastStat(split.data,measure,mean,paste("mean",measure,sep=".")), by="subject.id", all.x=TRUE)
	# Add number of past values to features
	num.visits<-paste("num",measure,"visits",sep=".")
	features <- merge(features, PastStat(split.data,measure,length,num.visits), by="subject.id", all.x=TRUE)
	# Add sum of past values to features
	features <- merge(features, PastStat(split.data,measure,sum,paste("sum",measure,sep=".")), by="subject.id", all.x=TRUE)
	# Add time of first and last past visits
	first.date<-paste("first",measure,"date",sep=".")
	features <- merge(features, PastStat(split.data,delta,min,first.date), by="subject.id", all.x=TRUE)
	last.date<-paste("last",measure,"date",sep=".")
	features <- merge(features, PastStat(split.data,delta,max,last.date), by="subject.id", all.x=TRUE)
	# Add mean of squared values
	features <- merge(features, PastStat(split.data,measure,meansquares,paste("meansquares",measure,sep=".")), by="subject.id", all.x=TRUE)
	# Add standard deviation of value
	features <- merge(features, PastStat(split.data,measure,stddev,paste("sd",measure,sep=".")), by="subject.id", all.x=TRUE)
	# Get first value (for slope calculation only)
	first.measure<-paste("first",measure,sep=".")
	features <- merge(features, PastStat(split.data,measure,first,first.measure), by="subject.id", all.x=TRUE)
	# Calculate slope
	slope<-(features[,colnames(features)==last.measure]-features[,colnames(features)==first.measure])/
		DaysToMonths(features[,colnames(features)==last.date]-features[,colnames(features)==first.date])
		
	# Add slope feature 
	features<-cbind(features,slope)
	# Drop first measure
	features<-features[,colnames(features)!=first.measure]

	# Rename slope
	names(features)[names(features)=="slope"]<-paste(measure,"slope",sep=".")

			
	# Add indicator for when there are fewer than 2 observations 
#	if (any(is.na(features[,colnames(features)==paste("num",measure,"visits",sep=".")])|features[,colnames(features)==paste("num",measure,"visits",sep=".")]<2)) {
		features<-
cbind(features,lessthan2=as.numeric(is.na(features[,colnames(features)==num.visits])|features[,colnames(features)==num.visits]<2))
		# Rename flag
		names(features)[names(features)=="lessthan2"]<-paste("lessthan2",measure,sep=".")
		
#	}
	
	# Add indicator for ids with no data - features$subject.id is the set of all subject.ids and data$subject.id
	# is the set of subject.ids that have measure data
#	if (any(!(features$subject.id %in% data$subject.id))) {
		features <- cbind(features, no.data=as.integer(!(features$subject.id %in% data$subject.id)))
		# Rename indicator
		names(features)[names(features)=="no.data"]<-paste("no",measure,"data",sep=".")
#	}

	return(features)
}

# Mean of squares function
meansquares<-function(data) {
	return(mean(data^2))
}

# Standard deviation function
stddev<-function(data) {
	return(sqrt(mean(data^2)-mean(data)^2))
}

# Last measure function
last<-function(data) {
	return(data[length(data)])
}

# First measure function
first<-function(data) {
	return(data[1])
}