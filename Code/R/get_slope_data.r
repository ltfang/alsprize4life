################################################################################

GetSlopeData <- function(raw.data.name="train",datasets=c("alsfrs","fvc","svc","vital","lab")) {
	# Calculates the slope data for alsfrs, fvc, svc, vital, and lab data 
	#
	# Args:
	#	raw.data.name: name of the raw dataset 
	#	datasets: a vector with the names of datasets to get slopes for 
	#
	# Returns: 
	#	a list of slope datasets 
	#
	# Example usage:
	# 	slope.list <- GetSlopeData("train")
  
  # Initialize slope data list 
  slope.data.list<-list()
  
 	if ("alsfrs" %in% datasets) {
 		
 		# Check to see if slope data has been created, if so, load
		rda.filename = paste("Data/slope_alsfrs_",raw.data.name,".rda",sep="")
  		
  		if (file.exists(rda.filename)) {
    		print(paste("loading saved alsfrs slope from",rda.filename,sep=" "))
    		load(rda.filename)
    		print("finished loading")
  		}

		else {
  			# Get ALSFRS data
	  		alsfrs.data<-GetAlsfrsData(raw.data.name)
   
	  		# Create vector of alsfrs measures to get slopes for      
	  		als.measures<-c("alsfrs.score","speech","salivation","swallowing",
    			"handwriting","cutting","dressing","turning","walking",
    			"climbing.stairs")
  
	  		alsfrs.slopes<-lapply(als.measures,CalcSlopeData,dataset=alsfrs.data)
  
	  		# Name datasets in list
	  		names(alsfrs.slopes)<-paste("slope",als.measures,sep=".")
	  		
	  		# Save to file
	  		save(alsfrs.slopes,file=rda.filename)
	  }
	  # Add to list of slope datasets
	  slope.data.list<-c(slope.data.list,alsfrs.slopes)
  }
  
  if ("fvc" %in% datasets) {
  	
	# Check to see if slope data has been created, if so, load
	rda.filename = paste("Data/slope_fvc_",raw.data.name,".rda",sep="")
  		
  	if (file.exists(rda.filename)) {
    		print(paste("loading saved fvc slope from",rda.filename,sep=" "))
    		load(rda.filename)
    		print("finished loading")
  	}
  	else {
		# Get FVC slopes
	  	fvc.slopes<-CalcSlopeData(GetFvcData(raw.data.name),"fvc.liters")
	  	
	  	# Save to file
	  	save(fvc.slopes,file=rda.filename)
	 } 
	 # Add to list of slope datasets
	 slope.data.list<-c(slope.data.list,list(slope.fvc.liters=fvc.slopes))
  }
  
  if ("svc" %in% datasets) { 	
  	
  	# Check to see if slope data has been created, if so, load
	rda.filename = paste("Data/slope_svc_",raw.data.name,".rda",sep="")

  	 if (file.exists(rda.filename)) {
    		print(paste("loading saved svc slope from",rda.filename,sep=" "))
    		load(rda.filename)
    		print("finished loading")
  	}
	else {
  		# Get SVC slopes
	  	svc.slopes<-CalcSlopeData(GetSvcData(raw.data.name),"svc.liters")
	  	
	  	# Save to file
	  	save(svc.slopes,file=rda.filename)
	}
	
	# Add to list of slope datasets
	slope.data.list<-c(slope.data.list,list(slope.svc.liters=svc.slopes))
  }
  
  if ("vital" %in% datasets) {
  	
  	# Check to see if slope data has been created, if so, load
	rda.filename = paste("Data/slope_vital_",raw.data.name,".rda",sep="")

  	if (file.exists(rda.filename)) {
    		print(paste("loading saved vital slope from",rda.filename,sep=" "))
    		load(rda.filename)
    		print("finished loading")
  	}
	else {
  		# Get vital slopes
	  	vital.data<-GetVitalData(raw.data.name)
	  	vital.names<-names(vital.data)
		vital.slopes<-mapply(CalcSlopeData,vital.data,vital.names,SIMPLIFY=FALSE)
  
	  	# Rename datasets in vital list
	  	names(vital.slopes)<-paste("slope",vital.names,sep=".")
	  	
	  	# Save to file
	  	save(vital.slopes,file=rda.filename)
	 }
	 
	 # Add to list of slope datasets
	 slope.data.list<-c(slope.data.list,vital.slopes)
  }
  
  if ("lab" %in% datasets) {
  	
  	# Check to see if slope data has been created, if so, load
	rda.filename = paste("Data/slope_lab_",raw.data.name,".rda",sep="")

  	if (file.exists(rda.filename)) {
    		print(paste("loading saved lab slope from",rda.filename,sep=" "))
    		load(rda.filename)
    		print("finished loading")
  	}
  	else {
		# Get lab slopes
	  	lab.data<-GetLabData(raw.data.name)
		lab.names<-names(lab.data)
	  	lab.slopes<-mapply(CalcSlopeData,lab.data,lab.names,SIMPLIFY=FALSE)
	
	  	# Rename datasets in lab list
	  	names(lab.slopes)<-paste("slope",lab.names,sep=".")
	  	
	  	# Save to file
	  	save(lab.slopes,file=rda.filename)
	 }
	 # Add to list of slope datasets
	slope.data.list<-c(slope.data.list,lab.slopes)
	}
	
  return(slope.data.list)
}

CalcSlopeData<-function(dataset, measure) {
# Given a dataset, returns a new dataset where the new measure is the slope of 
# the old measure.  If dataset is empty, returns an empty dataset with the 
# correct slope column names
#
# Args:
#   dataset: data frame generated from Get*Dataa
#   measure: the name of the measurement column
#
# Returns: 
#   a data frame with columns consisting of subject ids, mid point deltas, and
#   slopes of the measure in the dataset
#
# Example usage:
#   slope.data<-CalcSlopeData(GetFvcData("train"),"fvc.liters")

  # Get length of dataset
  nrow<-dim(dataset)[1]
    
  # Get delta column number
  delta.col<-grep("delta",colnames(dataset))
  
  # Get value column number
  value.col<-match(measure,colnames(dataset))
  
  # Calculate slopes
  slope<-diff(dataset[,value.col])/
    DaysToMonths(diff(dataset[,delta.col]))
  
  # Calculate midpoint deltas
  delta.mid<-dataset[,delta.col][2:nrow]-diff(dataset[,delta.col]/2)
    
  # Get lagged differences in subjects to indicate change in subject.id
  sub.diff<-diff(dataset$subject.id)
  
  # Form dataset containing subject ids, midpoint deltas, slopes, and sub diff
  slope.data<-data.frame(subject.id=dataset$subject.id[-1],delta.mid,slope,
    sub.diff)
  
  # Drop rows where subject changes and drop sub diff column 
  slope.data<-slope.data[slope.data$sub.diff==0,1:3]
  
  # Rename columns
  colnames(slope.data)<-c("subject.id",paste("slope",measure,"delta",sep="."),
    paste("slope",measure,sep="."))
  
  return(slope.data)
  
}