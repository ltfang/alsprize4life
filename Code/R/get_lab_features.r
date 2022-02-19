################################################################################################
# Dependencies
#source("Code/get_vital_data.r")
#source("Code/get_raw_data.r")
#source("Code/slopes.r")
#source("Code/past_stat.r")

GetLabFeatures <- function(raw.data.name="train") {
	
	# For a specified dataset, returns a dataset containing static features derived
	# Vital Signs data for each subject id.
	#
	# Args: 
	# 	raw.data.name: name of dataset
	#
	# Returns:
	#	A dataset in which for each row corresponds to a unique subject.id.  
	#	The columns consist of subject.id and features derived from the vital 
	# 	data.
	#
	# Example usage:
	#	#source("Code/get_vital_features.R") 
	#	vital.features<-GetVitalFeatures("train")
	
	# Check to see if vital features datafile has been created, if so, load
	rda.filename = paste("Data/lab_",raw.data.name,"_features.rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved lab features from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(lab.features)
	}

	# Get list of datasets and column names in lab data
	lab.data<-GetLabData(raw.data.name)
	columns<-names(lab.data)
	
	# Get a list of datasets containing feature data for each test
	feature.list<-mapply(GetDefaultFeatures,raw.data.name,lab.data,columns,SIMPLIFY=FALSE)

	lab.features<-Reduce(function(x,y) {merge(x,y,by="subject.id",all.x=TRUE)}, feature.list)
	
	# Save and return
	save(lab.features, file = rda.filename)
	return(lab.features)
}






