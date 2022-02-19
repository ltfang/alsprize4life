GetRawData<-function(raw.data.name="train") {

	# Returns an R dataframe corresponding to the data in
	# paste(raw.data.name,".txt",sep="")
	#
	# Args: 
	# 	raw.data.name: base name of a TXT file containing data in the contest format
	#
	# Returns:
	#	An R dataframe containing the data.
	#
	# Example usage: 
	#	GetRawData("train")

	# Check if RDA version of data has been stored
	rda.filename = paste("Data/",raw.data.name,".rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved data from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(data)
	}
	# Otherwise, read in data from TXT file
	txt.filename = paste(raw.data.name,".txt",sep="")
	print(paste("reading raw data from",txt.filename,sep=" "))
	data <- read.csv(txt.filename, header=FALSE, sep='|',colClasses=c("numeric","numeric","character","character","numeric","character","character")) 
	names(data)<-c("subject.id","form.id","form","record.id","field.id","field","value")
	# Fix naming scheme for Site of Onset
	data$value[data$field=="Site of Onset"&data$value==3] = "Onset: Limb"
	data$value[data$field=="Site of Onset"&data$value==1] = "Onset: Bulbar"
	# Fix naming scheme for Sex
	data$value[data$field=="Sex"&data$value==1] = "Male"
	data$value[data$field=="Sex"&data$value==2] = "Female"
	# Delete meaningless character values from fvc.liters
	data<-data[!(data$field=="Subject Liters (Trial 1)"&is.na(as.numeric(data$value))),]
	# Save data to RDA file
	print(paste("saving raw data to",rda.filename,sep=" "))
	save(data, file=rda.filename)
	print("finished reading and saving")
	return(data)
}