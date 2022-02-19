################################################################################################
# Dependencies
#source("Code/get_fvc_data.r")
#source("Code/get_raw_data.r")
#source("Code/slopes.r")
#source("Code/past_stat.r")

GetFvcFeatures <- function(raw.data.name="train") {
	
	# For a specified dataset, returns a dataset containing static features derived
	# FVC data for each subject id.
	#
	# Args: 
	# 	raw.data.name: name of dataset
	#
	# Returns:
	#	A dataset in which for each row corresponds to a unique subject.id.  
	#	The columns consist of subject.id and features derived from the fvc 
	# 	data.
	#
	# Example usage:
	#	#source("Code/get_fvc_features.R") 
	#	fvc.features<-GetFvcFeatures("train")
	
	# Check to see if fvc features datafile has been created, if so, load
	rda.filename = paste("Data/fvc_",raw.data.name,"_features.rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved fvc features from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(fvc.features)
	}

	fvc.features<-GetDefaultFeatures(raw.data.name,GetFvcData(raw.data.name),"fvc.liters")
	
	# Save features data to RDA file
	save(fvc.features, file = rda.filename)

	return(fvc.features)
	
}







