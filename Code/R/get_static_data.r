################################################################################################
# Dependencies
#source("Code/get_raw_data.r")
#source("Code/slopes.r")

GetStaticData <- function(raw.data.name="train",ref.name="traintest") {
	
	# For a specified raw dataset, returns a dataset containing the static features
	# for each subject id.
	#
	# Args: 
	# 	raw.data.name: name of dataset
	#
	# Returns:
	#	A dataset in which each row corresponds to a unique subject.id.  
	#	The columns consist of subject.id, age ...
	#	
	# Side effects:
	#	Saves static.data to file
	#
	# Example usage:
	#	#source("Code/get_static_data.R") 
	#	static.data<-GetStaticData("train")
	
	# Check if RDA version of data has been stored
	rda.filename = paste("Data/static_",raw.data.name,".rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved data from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(static.data)
	}
	
	# Otherwise, get the raw data for this dataset
	data<-GetRawData(raw.data.name)
	# Get reference data, used to determine which features to include
	ref.data<-GetRawData(ref.name)
	
	print(sprintf("Creating static dataset for %s",raw.data.name))
	# Add subject ids to static dataset
	static.data <- data.frame(subject.id=as.numeric(sort(unique(data$subject.id))))
	num.ids <- dim(static.data)[1]
	
	# Add Subject ALS History features
	print("Adding Subject ALS History features")
	val.fields.alshistory <- c(
		#"Subject ALS History Delta", # always zero
		"Onset Delta"
		#"Age at onset", # incorrectly recorded
    	#"Diagnosis Delta" # ADD THIS LATER?
	)
	static.data <- AddFieldValues(static.data, data, val.fields.alshistory)
	cat.fields.alshistory <- c(
		"Symptom",
		# "Symptom - Other (Specify)", # ADD THIS LATER?
		# "Location", # ADD THIS LATER? There are many values to this field, which creates a huge expansion of categorical variables when added 
	    "Site of Onset"
	)
	static.data <- AddFieldCategories(static.data, data, cat.fields.alshistory, ref.data)

	# Add Demographics features
	print("Adding Demographics features")
	one.fields.demographics <- c(
		#"Ethnicity", # what does this mean?
		"Race - Asian", 
		"Race - Black/African American", 
		"Race - Caucasian",
		"Race - Other"
		#"Race - Unknown" # always empty
	)	
	static.data <- AddFieldOnes(static.data, data, one.fields.demographics)
	val.fields.demographics <- c(
		#"Demographics Delta", # always zero
		"Age"
	)
	static.data <- AddFieldValues(static.data, data, val.fields.demographics)
	cat.fields.demographics <- c(
	    "Sex"
		#"Race Other Specify" # too seldom used
	)
	static.data <- AddFieldCategories(static.data, data, cat.fields.demographics, ref.data)

	# Add Family History features
	print("Adding Family History features")
	one.fields.fam <- c(
		# Family History Delta, # always zero
		"Aunt",
 		"Aunt (Maternal)",
		#"Aunt (Paternal)", # always empty
		"Cousin",
		#"Cousin (Paternal)", # always empty
		#"Cousin (Maternal)", # always empty
		"Father",
		#"Grandfather", # always empty               
		"Grandfather (Maternal)",
		#"Grandfather (Paternal)", # always empty
		"Grandmother",                       
		"Grandmother (Maternal)",            
		#"Grandmother (Paternal)", # always empty          
		"Mother",                            
		#"Nephew", # always empty                           
		#"Niece", # always empty                          
		#"Sibling", # always empty                          
		"Uncle",                             
		"Uncle (Maternal)",
		"Uncle (Paternal)",
		"Son",
		"Daughter",
		"Sister",
		"Brother"
	)
	static.data <- AddFieldOnes(static.data, data, one.fields.fam)
	# Create a feature that combines all family features into one
	static.data<-cbind(static.data,Family=as.numeric(Reduce(function(x,y){x|y},
		static.data[,colnames(static.data) %in% one.fields.fam])))
	
	cat.fields.fam <- c(
		"Neurological Disease"
		# "Neurological Disease Other Specify" # ADD THIS LATER?
	)
	static.data <- AddFieldCategories(static.data, data, cat.fields.fam, ref.data)	
	
	# Add Treatment Group features
	print("Adding Treatment Group features")
	fields.treatment <- c(
		#"Treatment Group Delta", # ADD THIS LATER?
		"Study Arm"
	)
	# Only store past data (data recorded before day 92)
	past.ids <- data$subject.id[(data$field == "Treatment Group Delta") & (as.integer(data$value) < 92)]
	static.data <- AddFieldCategories(static.data, data, fields.treatment, ref.data, past.ids)

	# Check dimensions of produced data frame
	if (dim(static.data)[1] != num.ids) {
		warning(sprintf("Incorrect matrix dimensions: expected %d rows, have %d rows", 
			num.ids, dim(static.data)[1]))
	}
	
	# Make numeric Onset Delta and Age
 	mode(static.data$"Onset Delta")<-"numeric"
	mode(static.data$Age)<-"numeric"
	
	# Save data to RDA file
	print(paste("saving raw data to",rda.filename,sep=" "))
	save(static.data, file=rda.filename)
	return(static.data)
}
# # # Find all unique forms
# unique(data$form)
# # Check unique values of each field in a form
# fields <- unique(data$field[data$form == "Family History"])
# for (field in fields) {
	# print(field)
	# print(unique(data$value[data$field == field]))
# }
# # Check if fields are duplicated for a single subject.id
# fields <- unique(data$field[data$form == "Family History"])
# for (field in fields) {
	# print(field)
	# print(data$subject.id[data$field == field & data$value != ""][duplicated(data$subject.id[data$field == field & data$value != ""])])
# }
# # Count the occurrences of each value in a field
# field <- "Location"
# vals <- unique(data$value[data$field == field])
# for (val in vals) {
	# print(val)
	# print(length(data$subject.id[data$field == field & data$value == val]))
# }
# # Check if duplicate subject ids occur with this field
# field = "Diagnosis Delta"
# which(duplicated(data$subject.id[data$field == field]))
# for (id in data$subject.id[data$field == field]) {
	# if (length(unique(data$value[data$field == field & data$subject.id == id])) > 1){
		# print(id)
	# }
# }

AddFieldOnes <- function(static.data, data, fields){
	# Adds a new binary column to static.data for each field in fields; populates an entry with  
	# a 1 if and only if the subject ID is associated with the value 1 in that field for some
	# row in data; uses the names in fields as column names
	#
	# Args:
	#	static.data: static dataset
	#	data: raw dataset
	#	fields: array of string field names
	#
	# Returns:
	#	static.data with an additional binary column representing each field in fields
	
	num.rows <- dim(static.data)[1]
	rownames(static.data) <- static.data$subject.id
	
	# For each field, add an all zero column to static data
	names <- colnames(static.data)
	static.data <- cbind(static.data, matrix(0,num.rows,length(fields)))
	colnames(static.data) <- c(names, fields)
	
	for (field in fields) {
		# If a subject ID has a value of 1 for this field in data, assign a 1 in static.data
		ids <- unique(data$subject.id[(data$field==field) & (data$value==1)])
		static.data[as.character(ids),field] <- 1
	}
	return(static.data)
}

AddFieldValues <- function(static.data, data, fields){
	# Adds a new column to static.data for each field in fields; populates the columns with the 
	# field values in data and uses the names in fields as column names; the value associated
	# with each subject.id and field combination must be unique
	#
	# Args:
	#	static.data: static dataset
	#	data: raw dataset
	#	fields: array of string field names
	#
	# Returns:
	#	static.data with an additional column representing each field in fields
	
	names <- colnames(static.data)
	for (field in fields) {
		static.data <- merge(static.data, 
			unique(data[data$field==field,c("subject.id","value")]), 
			by="subject.id", all.x=TRUE)
	}
	colnames(static.data) <- c(names, fields)
	return(static.data)
}

AddFieldCategories <- function(static.data, data, fields, ref.data, filter.ids=NULL){
	# Adds a new binary column to static.data for each ref.data value of each field in fields; 
	# populates the columns with the field values in data and constructs column names from
	# field name and value name
	#
	# Args:
	#	static.data: static dataset
	#	data: raw dataset
	#	ref.data: reference raw dataset used to determine which values to include
	#	fields: array of string field names representing categorical fields
	#	filter.ids: if not NULL, will only add value of 1 to subject.ids appearing in this vector
	#
	# Returns:
	#	static.data with an additional binary columns for each value of each field; a value
	#	of 1 appears if a subject id is present in data along with a particular field-value 
	#	combination (see the additional filter.ids constraint above)
	num.rows = dim(static.data)[1]
	rownames(static.data) <- static.data$subject.id
	for (field in fields) {
		# Find the unique values for this field in reference data
		vals <- unique(ref.data$value[ref.data$field == field])
		for (val in vals) {
			# Add an all zero column to static.data for this field-value pair
			names <- colnames(static.data)
			static.data <- cbind(static.data, matrix(0,num.rows,1))
			name <- paste(field,"-",val,sep="")
			colnames(static.data) <- c(names, name)
			ids <- unique(data$subject.id[(data$field==field) & (data$value==val)])
			if(!is.null(filter.ids)) {
				# Intersect with filter.ids if desired
				ids <- intersect(filter.ids, ids)
			}
			# Assign a value of 1 to each selected id
			static.data[as.character(ids),name] <- 1
		}
 	}
	
	return(static.data)
}