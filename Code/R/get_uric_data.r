################################################################################################
# Dependencies
#source("Code/get_dynamic_data.r")

GetUricData <- function(raw.data.name="train") {
	
	# For a specified dataset in the original contest format, returns
	# a dataset containing only lab information.  
	#
	# Args: 
	# 	raw.data.name: name of dataset
	#
	# Returns:
	#	A list.  The first element is a list of datasets that correspond to lab tests. The columns 
	#   in each dataset consist of subject.id, test delta and test result. 
	#	The second element is a vector of column names corresponding to the test result column of 
	#	each dataset. 
	#
	# Example usage: 
	#	#source("Code/get_lab_data.R")
	#	lab.data<-GetLabData("train")
	
	# Check if RDA version of lab data has been stored
	rda.filename = paste("Data/uric_",raw.data.name,".rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved uric data from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(uric.data)
	}

	# Get raw data
	 data<-GetRawData(raw.data.name)	

	# Keep only lab data 
	 data <- data[data$form == "Laboratory Data", ]	
	
	uric.data<-TestToData("Uric Acid",data)
	
	uric.data$test.result[uric.data$test.unit=="mg/dL"]<-
		uric.data$test.result[uric.data$test.unit=="mg/dL"]*59.48
	  
  	# Drop test name and test units
	uric.data<-uric.data[,!(colnames(uric.data) %in% c("test.name","test.unit"))]

	# Sort by subject id and delta
	uric.data<-uric.data[order(uric.data$subject.id,uric.data$lab.delta),]
  
	# Remove records with missing lab.delta, test.result, or subject.id
	uric.data <- uric.data[!is.na(uric.data$lab.delta)& !is.na(uric.data$test.result)&
    	!is.na(uric.data$subject.id),]
	
	# Check for duplicate deltas and delete those rows   
	# Create combination of subject.ids and deltas	
	id<-paste(as.integer(uric.data$subject.id), as.integer(uric.data$lab.delta),sep=".")
	
	# Check for duplicates in id
	dup<-duplicated(id)
	
	# Keep only nonduplicates - throws away second record with same delta
	uric.data<-uric.data[!dup,]

	# Rename test.result column
	colnames(uric.data)[colnames(uric.data)=="test.result"]<-"Uric.Acid"
	
	# Rename delta column
	colnames(uric.data)[colnames(uric.data)=="lab.delta"]<-"Uric.Acid.delta"

  # Create outlier filter if filter does not exist
  num.sd<-4 
  filter.filename<-paste("Data/uric_filter",num.sd,"sd.rda",sep="_")
	if (file.exists(filter.filename)) {
	  print(paste("loading saved uric filter from",filter.filename,sep=" "))
	  load(filter.filename)
	}
  else {
    uric.filter<-LabOutlierFilter(Uric.Acid,num.sd)
    save(uric.filter,file=filter.filename)
  }

  # Discard outliers
  uric.data<-LabDiscardOutlier(uric.data,uric.filter)
	
	# Save dataset to file
	save(uric.data, file = rda.filename)
	return(uric.data)
}


