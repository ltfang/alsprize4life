################################################################################################
# Dependencies
#source("Code/get_dynamic_data.r")

GetFvcData <- function(raw.data.name="train") {
	
	# For a specified dataset in the original contest format, returns
	# a dataset containing only FVC information.  
	#
	# Args: 
	# 	raw.data.name: name of dataset
	#
	# Returns:
	#	A dataset in which each row corresponds to a unique 
	#	record.id.  The columns consist of subject.id, fvc.delta, fvc.liters, and fvc.normal.
	#
	# Example usage: 
	#	#source("Code/get_fvc_data.R")
	#	fvc.data<-GetFvcData("train")
	
	# Check if RDA version of FVC data has been stored
	rda.filename = paste("Data/fvc_",raw.data.name,".rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved fvc data from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(fvc.data)
	}
	
	# Get raw data
	data<-GetRawData(raw.data.name)	

	# Choose form from which to draw data
	form <- "Forced Vital Capacity"

	# Choose field names to include
	fields <- c("Forced Vital Capacity Delta","Subject Liters (Trial 1)","Subject Normal")

	# Choose column names for dataset
	columns<-c("fvc.delta", "fvc.liters", "fvc.normal")
	
	# Only consider desired form and fields to pass into GetDynamicData
	data <- data[data$form == form & data$field %in% fields, ]

	# Create dataset
	fvc.data<-GetDynamicData(data,fields,columns)

	# Sort rows by subject.id and then by fvc.delta
	fvc.data<-fvc.data[order(fvc.data$subject.id,fvc.data$fvc.delta),]
	
	# Remove records with missing fvc.delta or fvc.liters
	fvc.data <- fvc.data[!is.na(fvc.data$fvc.delta)&!is.na(fvc.data$fvc.liters),]
	
	# Check for duplicate deltas and delete those rows   
	# Create combination of subject.ids and deltas	
	id<-paste(as.integer(fvc.data$subject.id),as.integer(fvc.data$fvc.delta),
	   sep=".")
  
	# Check for duplicates in id
	dup<-duplicated(id)
  
	# Keep only nonduplicates - throws away second record with same delta
	fvc.data<-fvc.data[!dup,]
  
	# Save FVC data to RDA file
	save(fvc.data, file = rda.filename)
	return(fvc.data)
}






