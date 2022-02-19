################################################################################################
# Dependencies
#source("Code/get_svc_data.r")
#source("Code/get_raw_data.r")
#source("Code/slopes.r")
#source("Code/past_stat.r")

GetSvcFeatures <- function(raw.data.name="train") {
	
	# For a specified dataset, returns a dataset containing static features derived
	# SVC data for each subject id.
	#
	# Args: 
	# 	raw.data.name: name of dataset
	#
	# Returns:
	#	A dataset in which for each row corresponds to a unique subject.id.  
	#	The columns consist of subject.id and features derived from the svc 
	# 	data.
	#
	# Example usage:
	#	#source("Code/get_svc_features.R") 
	#	svc.features<-GetSvcFeatures("train")
	
	# Check to see if svc features datafile has been created, if so, load
	rda.filename = paste("Data/svc_",raw.data.name,"_features.rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved svc features from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(svc.features)
	}
	
	svc.features<-GetDefaultFeatures(raw.data.name,GetSvcData(raw.data.name),"svc.liters")

	# Save features data to RDA file
	save(svc.features, file = rda.filename)

	return(svc.features)

}







