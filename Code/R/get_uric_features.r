################################################################################################

GetUricFeatures <- function(raw.data.name="train") {
	
	# For a specified dataset, returns a dataset containing static features derived
	# uric acid test data for each subject id.
	#
	# Args: 
	# 	raw.data.name: name of dataset
	#
	# Returns:
	#	A dataset in which for each row corresponds to a unique subject.id.  
	#	The columns consist of subject.id and features derived from the uric acid data. 
	#
	# Example usage:
	#	uric.features<-GetUricFeatures("train")
	
	# Check to see if uric features datafile has been created, if so, load
	rda.filename = paste("Data/uric_",raw.data.name,"_features.rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved uric features from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(uric.features)
	}
	
	uric.features<-GetDefaultFeatures(raw.data.name,GetUricData(raw.data.name),"Uric.Acid")

	# Save features data to RDA file
	save(uric.features, file = rda.filename)

	return(uric.features)

}







