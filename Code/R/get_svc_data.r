################################################################################################
# Dependencies
#source("Code/get_dynamic_data.r")

GetSvcData <- function(raw.data.name="train") {
	
	# For a specified dataset in the original contest format, returns
	# a dataset containing only SVC information.  
	#
	# Args: 
	# 	raw.data.name: name of dataset
	#
	# Returns:
	#	A dataset in which each row corresponds to a unique 
	#	record.id.  The columns consist of subject.id, svc.delta, svc.liters, and svc.normal.
	#
	# Example usage: 
	#	#source("Code/get_svc_data.R")
	#	svc.data<-GetSvcData("train")
	
	# Check if RDA version of SVC data has been stored
	rda.filename = paste("Data/svc_",raw.data.name,".rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved svc data from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(svc.data)
	}
	
	# Get raw data
	data<-GetRawData(raw.data.name)	

	# Choose form from which to draw data
	form <- "Slow Vital Capacity"

	# Choose field names to include
	fields <- c("Slow vital Capacity Delta","Subject Liters (Trial 1)")

	# Choose column names for dataset
	columns<-c("svc.delta", "svc.liters")

	# Only consider desired form and fields to pass into GetDynamicData
	data <- data[data$form == form & data$field %in% fields, ]

	# Create dataset
	svc.data<-GetDynamicData(data,fields,columns)

	# Sort rows by subject.id and then by svc.delta
	svc.data<-svc.data[order(svc.data$subject.id,svc.data$svc.delta),]
	
	# Remove records with missing svc.delta or svc.liters
	svc.data <- svc.data[!is.na(svc.data$svc.delta)&!is.na(svc.data$svc.liters),]
	
	# Check for duplicate deltas and delete those rows   
	# Create combination of subject.ids and deltas	
	id<-paste(as.integer(svc.data$subject.id),
	          as.integer(svc.data$svc.delta),sep=".")
	
	# Check for duplicates in id
	dup<-duplicated(id)
	
	# Keep only nonduplicates - throws away second record with same delta
	svc.data<-svc.data[!dup,]
  
	# Save SVC data to RDA file
	save(svc.data, file = rda.filename)
	return(svc.data)
}






