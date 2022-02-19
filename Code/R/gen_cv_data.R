GenCvData<-function(data.name,num.cv,random.seed="123") {

	# Generates and saves cross validation datasets for an input dataset in
	# paste("Data/",data.name,".rda",sep="")
	#
	# Args: 
	# 	data.name: base name of RDA file in Data folder; file must contain a
	#   	single variable; the variable must be a dataframe with the column 'subject.id'
	#	num.cv: number of cross-validation folds
	#	random.seed: seed to set before generating random numbers
	#
	# Example usage: 
	#	source("Code/CV/gen_cv_data.R")
	#	GenCvData("train",10)
		
	#Check to see if cv split datasets already exist
	cv.train.list<-list()
	cv.test.list<-list()
	for (fold in 1:num.cv) {
		cv.train.list[[fold]]<-sprintf("Data/%s-cv%s_%sof%d.rda",data.name,
			"train",fold,num.cv)
		cv.test.list[[fold]]<-sprintf("Data/%s-cv%s_%sof%d.rda",data.name,
			"test",fold,num.cv)
	}
	filename.list<-c(cv.train.list,cv.test.list)
	check<-sapply(filename.list,file.exists)
	if (all(check)) {
		print("CV splits already generated")
		return()
	}

	# Load original dataset
	rda.filename <- paste("Data/",data.name,".rda",sep="")
	# Keep track of name of variable loaded
	variable.name <- load(rda.filename)

	# Assign loaded variable a standard name
	all.data <- get(variable.name)

	# Find the unique subject ids and sort them
	ids <- sort(unique(all.data$subject.id))
	
	# Set random seed to guarantee reproducibility of results
	set.seed(random.seed)
	
	# Randomly permute subject ids
	ids <- sample(ids)
	
	# Randomly partition subject ids into num.cv segments of roughly equal size
	cv.ids <- split(ids,1:num.cv)
	
	# Create cv test and train datasets for each CV fold
	for (fold in names(cv.ids)) {
		# Create test dataset for this fold
		SaveCvData(data.name, "test", fold, num.cv, variable.name,
			all.data[all.data$subject.id %in% cv.ids[[fold]],])
		# Create training dataset for this fold
		SaveCvData(data.name, "train", fold, num.cv, variable.name,
			all.data[!(all.data$subject.id %in% cv.ids[[fold]]),])
	}
}

SaveCvData <- function(data.name, data.type, fold, num.cv, variable.name, cv.data) {
	# Saves cross-validation dataset to file
	#
	# Args:
	# 	data.name: base name of RDA file in Data folder
	#	data.type: "train" or "test"
	#	fold: string representation of cross-validation fold number
	#	num.cv: number of cross-validation folds
	#	variable.name: name of variable to which cv.data should be assigned
	#	cv.data: cross-validation dataset to save
	
	filename <- sprintf("Data/%s-cv%s_%sof%d.rda",data.name,data.type,fold,num.cv)
	print(paste("Saving dataset to",filename))
	# Assign dataset in correct variable name
	assign(variable.name, cv.data)
	# Save dataset
	save(list = variable.name, file = filename)
}

