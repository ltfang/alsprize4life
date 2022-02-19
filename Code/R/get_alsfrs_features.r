################################################################################################
# Dependencies
#source("Code/get_alsfrs_data.r")
#source("Code/get_raw_data.r")
#source("Code/slopes.r")
#source("Code/alsfrs_score.r")
#source("Code/past_stat.r")

GetAlsfrsFeatures <- function(raw.data.name="train") {
	
	# For a specified dataset, returns a dataset containing static features derived
	# alsfrs data for each subject id.
	#
	# Args: 
	# 	raw.data.name: name of dataset
	#
	# Returns:
	#	A dataset in which for each row corresponds to a unique subject.id.  
	#	The columns consist of subject.id and features derived from the alsfrs 
	# 	data.
	#
	# Example usage:
	#	#source("Code/get_alsfrs_features.R") 
	#	alsfrs.features<-GetAlsfrsFeatures("train")
	
	# Check to see if alsfrs features data has been created, if so, load
	rda.filename = paste("Data/alsfrs_",raw.data.name,"_features.rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved alsfrs features from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(alsfrs.features)
	}

	# Get alsfrs data 
	data<-GetAlsfrsData(raw.data.name)
		

	# Get default features for alsfrs score
	alsfrs.features<-GetDefaultFeatures(raw.data.name,data,"alsfrs.score")

	# Create list of individual column names to get features for; include only those shared by all alsfrs and alsfrs-r
	list<-c("speech","salivation","swallowing","handwriting","cutting","dressing","turning","walking","climbing.stairs")

	# Add on features for all individual scores in the list
	for (i in 1:length(list)) {
		alsfrs.features <- merge(alsfrs.features,
			GetDefaultFeatures(raw.data.name,data,list[i]), by="subject.id",all.x=TRUE)
	}
	
	# Save features data to RDA file
	save(alsfrs.features, file = rda.filename)

	return(alsfrs.features)
	# ADD FIRST SLOPE, LAST SLOPE, MEAN SLOPE between pairs of visits
	
	# Add some type of slope acceleration to features?
	# names <- c(names,"num.visits")
	# # Max slope between pair of past visits
	# alsfrs.features <- cbind(alsfrs.features, ???)
	# names <- c(names,"max.slope")
	# # Min slope between pair of past visits
	# alsfrs.features <- cbind(alsfrs.features, ???)
	# names <- c(names,"min.slope")
}

# Notes on subjects
# 87664 - only one visit before 91 days
# 681771 - only one visit before 91 days
# 757875 - no visits before 91 days



