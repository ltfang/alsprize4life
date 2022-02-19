GetSubjectIds<-function(raw.data.name="train") {

	# Returns a vector containing all unique subject ids for a given raw data set
	#
	# Args: 
	# 	raw.data.name: base name of a TXT file containing data in the contest format
	#
	# Returns:
	#	A vector of unique subject ids
	#
	# Example usage: 
	#	GetSubjectIds("train")

	# Check if RDA version of data has been stored
	rda.filename = paste("Data/subjects_",raw.data.name,".rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved data from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(data)
	}
	# Otherwise, get subject ids 
	data<-data.frame(subject.id=sort(unique(GetRawData(raw.data.name)$subject.id)))
	
	# Save data to RDA file
	print(paste("saving raw data to",rda.filename,sep=" "))
	save(data, file=rda.filename)
	print("finished reading and saving")
	return(data)
}