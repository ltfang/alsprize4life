################################################################################################
# Dependencies
#source("Code/get_dynamic_data.r")

GetAlsfrsData <- function(raw.data.name="train") {
	
	# For a specified dataset in the original contest format, returns
	# a dataset containing only ALSFRS information.  
	#
	# Args: 
	# 	raw.data.name: name of dataset
	#
	# Returns:
	#	A dataset in which each row corresponds to a unique 
	#	record.id.  The columns consist of subject.id, alsfrs.delta, the ALSFRS 
	# 	question fields.
	#
	# Example usage: 
	#	#source("Code/get_alsfrs_data.r")
	#	alsfrs.data<-GetAlsfrsData("train")
	
	# Check if RDA version of ALSFRS data has been stored
	rda.filename = paste("Data/alsfrs_",raw.data.name,".rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved alsfrs data from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(alsfrs.data)
	}
	# Get raw data
	data<-GetRawData(raw.data.name)	
	
	# Choose form from which to draw data
	form <- "ALSFRS(R)"

	# Choose field names to include
	fields <- c("ALSFRS Delta", "1. Speech", "2. Salivation", 
		"3. Swallowing", "4. Handwriting", "5a. Cutting without Gastrostomy", 
		"5b. Cutting with Gastrostomy", "6. Dressing and Hygiene", 
		"7. Turning in Bed", "8. Walking", "9. Climbing Stairs", 
		"10. Respiratory", "R-1. Dyspnea", "R-2. Orthopnea", 
		"R-3. Respiratory Insufficiency")

	# Choose column names for dataset
	columns<-c("alsfrs.delta", "speech", "salivation", 
		"swallowing", "handwriting", "cutting.wo.gastrostomy", "cutting.w.gastrostomy", 
		"dressing", "turning", "walking", "climbing.stairs", "respiratory", "r1.dyspnea", 		
		"r2.orthopnea", "r3.resp.insufficiency")

	# Only consider desired form and fields to pass into GetDynamicData
	data <- data[data$form == form & data$field %in% fields, ]

	# Create dataset
	alsfrs.data<-GetDynamicData(data,fields,columns)

	# Sort rows by subject.id and then by alsfrs.delta
	alsfrs.data<-alsfrs.data[order(alsfrs.data$subject.id,
    alsfrs.data$alsfrs.delta),]
  
	# Check for duplicate deltas and delete those rows   
	# Create combination of subject.ids and deltas	
	id<-paste(as.integer(alsfrs.data$subject.id),
    as.integer(alsfrs.data$alsfrs.delta),sep=".")

	# Check for duplicates in id
	dup<-duplicated(id)
	
	# Keep only nonduplicates - throws away second record with same delta
	alsfrs.data<-alsfrs.data[!dup,]
	
	# Compute ALSFRS score
	alsfrs.data<-AlsfrsScore(alsfrs.data)
	
	# Create column that is contains cutting with and without gastrostomy together
	cutting<-apply(cbind(alsfrs.data$cutting.wo.gastrostomy,alsfrs.data$cutting.w.gastrostomy),1,function(x)
		max(x,na.rm=TRUE))
	
	# If both cutting fields = NA, then result of max will be -Inf.  Replace all -Inf with NA
	cutting[is.infinite(cutting)]<-NA	
	
	# Add to data frame
	alsfrs.data<-cbind(alsfrs.data,cutting)
	  	
	# Save ALSFRS data to RDA file
	save(alsfrs.data, file = rda.filename)
	return(alsfrs.data)
}



