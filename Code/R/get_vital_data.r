################################################################################################
# Dependencies
#source("Code/get_vital_data.r")

GetVitalData <- function(raw.data.name="train") {
	
	# For a specified dataset in the original contest format, returns
	# a dataset containing only Vital Signs information.  
	#
	# Args: 
	# 	raw.data.name: name of dataset
	#
	# Returns:
	#	A list.  The first element is a list of datasets that correspond to lab tests. The columns 
	#   in each dataset consist of subject.id, vital delta, and vital result.
	#	The second element is a vector of column names corresponding to the test result column of 
	#	each dataset. 
	#
	# Example usage: 
	#	#source("Code/get_vital_data.R")
	#	vital.data<-GetVitalData("train")
	
	# Check if RDA version of Vital Signs data has been stored UPDATE
	rda.filename = paste("Data/vital_",raw.data.name,".rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved vital data from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(data.list)
	}
	
	# Get raw data
	data<-GetRawData(raw.data.name)	

	# Choose form from which to draw data
	form <- "Vital Signs"

	# Choose field names to include
	#fields <- c("Blood Pressure (Diastolic)", "Blood Pressure (Systolic)", "Vital Signs Delta", "Pulse", "Respiratory Rate", "Weight", "Weight Units", "Blood Pressure (Diastolic) Units", "Pulse Units", "Respiratory Rate Units", "Blood Pressure (Systolic) Units", "Height", "Height Units", "Temperature Units", "Baseline Standing Blood Pressure (Diastolic)", "Baseline Standing Blood Pressure (Systolic)", "Baseline Supine Blood Pressure (Diastolic)", "Baseline Supine Blood Pressure (Systolic)", "Baseline Weight", "Supine Pulse", "Standing Pulse", "Endpoint Supine Pulse", "Endpoint Standing Pulse", "Baseline Supine Pulse", "Baseline Standing Pulse", "Endpoint Weight", "Endpoint Standing Blood Pressure (Diastolic)", "Endpoint Standing Blood Pressure (Systolic)", "Endpoint Supine Blood Pressure (Diastolic)", "Endpoint Supine Blood Pressure (Systolic)", "Supine Blood Pressure (Diastolic)", "Supine Blood Pressure (Systolic)", "Standing Blood Pressure (Diastolic)", "Standing Blood Pressure (Systolic)")
	
	# Create list where each element is vector of fields
	field.list<-list(weight=c("Vital Signs Delta", "Weight", "Weight Units"),
		height=c("Vital Signs Delta", "Height", "Height Units"),
		resp.rate=c("Vital Signs Delta", "Respiratory Rate"),
		bp.diastolic=c("Vital Signs Delta", "Blood Pressure (Diastolic)"),
		bp.systolic=c("Vital Signs Delta", "Blood Pressure (Systolic)"))
	
	# Create list where each element is a vector of column names for each dataset
	colname.list<-list(weight=c( "vital.delta", "weight", "weight.units"),
		height=c("vital.delta","height", "height.units"),
		resp=c("vital.delta","resp.rate"),
		bp.diastolic=c("vital.delta","bp.diastolic"),
		bp.diastolic=c("vital.delta","bp.systolic"))
	
	# Create vector of value column names to pass out of function for use with GetDefaultFeatures
	columns<-c("weight","height","resp.rate","bp.diastolic","bp.systolic")
	
	# Only consider desired form to pass into GetDynamicData
	data <- data[data$form == form, ]

	# Create datasets 
	n<-length(field.list)
	data.list<-list(rep(0,n))
	for (i in 1:n) { 

		data.temp<-data[data$form == form & data$field %in% field.list[[i]],] 
		vital.data<-GetDynamicData(data.temp,field.list[[i]],colname.list[[i]])

		# Sort rows by subject.id and then by vital.delta
		vital.data<-vital.data[order(vital.data$subject.id,
      vital.data$vital.delta),]
	
		# Remove records with missing vital.delta or primary field of interest
		# Note: diastolic and systolic data sometimes have missing units, 
    # but height, weight, and respiratory rate do not
    
    	 if (i %in% c(1,2)) {
			 vital.data <- vital.data[!is.na(vital.data[,2])&
			 !is.na(vital.data[,3])&!is.na(vital.data[,4]),]
		 }
		 else {
			vital.data <- vital.data[!is.na(vital.data[,2])&
			!is.na(vital.data[,3]),]
		}
	
		# Check for duplicate deltas and delete those rows   
		# Create combination of subject.ids and deltas	
		id<-paste(as.integer(vital.data$subject.id),
		          as.integer(vital.data$vital.delta),sep=".")
		
		# Check for duplicates in id
		dup<-duplicated(id)
		
		# Keep only nonduplicates - throws away second record with same delta
		vital.data<-vital.data[!dup,]
		
		# Write dataset to list 
		data.list[[i]]<-as.data.frame(vital.data)
		
	}
	
	# Get weight data from list
	weight.data<-data.list[[1]]
	# Convert weight units (Kilograms, 2, Pounds) so that everything is in kg; treat 2 as kg
	weight.data$weight[weight.data$weight.units=="Pounds"]<-
		weight.data$weight[weight.data$weight.units=="Pounds"]*0.453592
	# Drop units
	weight.data<-weight.data[,colnames(weight.data)!="weight.units"]
	# Rewrite new dataset to list
	data.list[[1]]<-weight.data
	
	# Get height data from list 
	height.data<-data.list[[2]]
	# Convert height units (CM, Centimeters, Inches) so everything is in cm
	height.data$height[height.data$height.units=="Inches"]<-
		height.data$height[height.data$height.units=="Inches"]*2.54
	# Drop units
	height.data<-height.data[,colnames(height.data)!="height.units"]
	# Rewrite new dataset to list
	data.list[[2]]<-height.data
	
	# No need to convert respiratory rate units or blood pressure units 

	# Rename list elements to reflect the column names of the values
	names(data.list)<-names(field.list)
	
	# Save vital data to RDA file
	save(data.list, file = rda.filename)
	return(data.list)
}

