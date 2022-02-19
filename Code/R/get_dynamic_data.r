################################################################################################
# Dependencies
#source("Code/get_raw_data.r")

GetDynamicData <- function(data, fields, columns) {
	# Returns a dataset with dynamic data from the given fields and form.
	#
	# Args:
	# 	data: the name of a raw dataset that includes only the form and fields that are desired 
	#	fields: the name of the fields from which to draw data (do not include "subject.id")
	#	columns: the column names in the returned dataset, one per field in fields 
	#		(NOT including "subject.id")
	#
	# Returns:
	#	A dataset in which each row corresponds to a unique 
	#	record.id.  The columns are given by 'columns'.
	
	
	# Get subsets of the data that correspond to each field name 
	subsets<-lapply(fields,subset.field,data=data)

	# Merge subsets in list on subject.id and record.id
	dyn.data<-Reduce(function(x,y)
    {merge(x,y,by=c("subject.id","record.id"),all=TRUE)}, subsets)
	
	# Rename with column names
	colnames(dyn.data)<-c("subject.id","record.id",columns)
	
	# Find columns that should be numeric: "test.name" and any containing the substring "unit"
	#numeric.cols <- sapply(dyn.data, function(c) !any(!is.na(c) & c != "" & is.na(as.numeric(c))))
	numeric.cols <- !(grepl("unit",colnames(dyn.data)) | grepl("test.name",colnames(dyn.data)))
  
	# Change type of those columns from character to numeric
	dyn.data[,numeric.cols] <- lapply(dyn.data[,numeric.cols],as.numeric)
		
	# Drop record.id
	dyn.data<-dyn.data[,colnames(dyn.data)!="record.id"]
  
	return(dyn.data)
	
}

subset.field<-function(data,field) {
	# Given a field name and a dataset in the form of a raw dataset, returns a dataset that 
	# contains only rows with that fieldname. Value column is renamed with name of field
		
	# Get subset
	subset<-data[data$field==field,]
	# Rename value column
	colnames(subset)[colnames(subset)=="value"]<-field
	# Keep only subject.id, record.id, and 'field' columns
	subset<-subset[,colnames(subset) %in% c("subject.id","record.id",field)]
	return(subset)	
}

