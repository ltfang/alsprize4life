################################################################################################
# Dependencies
#source("Code/get_dynamic_data.r")

GetLabData <- function(raw.data.name="train") {
	
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
	rda.filename = paste("Data/lab_",raw.data.name,".rda",sep="")
	if (file.exists(rda.filename)) {
		print(paste("loading saved lab data from",rda.filename,sep=" "))
		load(rda.filename)
		print("finished loading")
		return(lab.data)
	}

	# Get raw data
	 data<-GetRawData(raw.data.name)	

	# Keep only lab data 
	 data <- data[data$form == "Laboratory Data", ]	

	# Get list of all tests	
	# test.names<-unique(data$value[data$field=="Test Name"])
	
	# Count the number of subjects there is data for for each test
	# NumTestSubs(test.names)
	
	# List of all tests with at least 709 subject ids (30)
	# test.names<-c("Bicarbonate", " HbA1c (Glycated Hemoglobin)", "Red Blood Cells (RBC)","White Blood Cells (WBC)","Creatine Kinase","Chloride","Triglycerides","Glucose", "Total Cholesterol", "Albumin", "Sodium", "Hemoglobin", "Calcium", "Monocytes", "Lymphocytes", "Hematocrit","Neutrophils","Alkaline Phosphatase","Gamma-glutamyltransferase","Protein","Eosinophils","AST(SGOT)","Basophils","Phosphorus","Creatinine","Potassium","Bilirubin (total)", "Platelets","ALT(SGPT)", "Urine Ph")
	
	# List of all tests with at least 709 subject ids for which there are conversions (21)
	test.names<-c("Bicarbonate", "HbA1c (Glycated Hemoglobin)","Red Blood Cells (RBC)","Creatine Kinase","Chloride","Triglycerides","Glucose", "Total Cholesterol","Sodium", "Hemoglobin", "Calcium","Hematocrit","Alkaline Phosphatase","Gamma-glutamyltransferase","AST(SGOT)","Phosphorus","Creatinine","Potassium","Bilirubin (total)","ALT(SGPT)","Urine Ph")
	
	# List of corresponding test names 
	col.names<-c("Bicarbonate","HbA","RBC","Creatine.Kinase","Chloride","Triglycerides","Glucose","Cholesterol","Sodium","Hemoglobin","Calcium","Hematocrit","Alk.Phos","Gamma.Glutamyl","AST(SGOT)","Phosphorus","Creatinine","Potassium","Bilirubin","ALT(SGPT)","Urine.Ph")
	
	# Get data corresponding to each test name and put it in a list
	data.list<-lapply(test.names,TestToData,data=data)
			
	# Rename list elements
	names(data.list)<-test.names
	
	#lapply(data.list,TestUnitsUsed) # what units does each test show?
	
	# Get a list of conversion factors for each test in test.names 
	convert.list<-GetConvertList(test.names)
	
	# Get a list of new datasets with converted units and only key columns
	lab.data<-lapply(1:length(data.list),ConvertTestUnits,test.data=data.list,
    conversions=convert.list,col.names=col.names)

	# Name each dataset
	names(lab.data)<-col.names[1:length(data.list)]
  
  # Create outlier filter if filter does not exist
  num.sd<-4 
  filter.filename<-paste("Data/lab_filter",num.sd,"sd.rda",sep="_")
	if (file.exists(filter.filename)) {
	  print(paste("loading saved lab filter from",filter.filename,sep=" "))
	  load(filter.filename)
	}
  else {
    lab.filter<-lapply(lab.data,LabOutlierFilter,num.sd=num.sd)
    save(lab.filter,file=filter.filename)
  }
  
  # Discard outliers
  lab.data<-mapply(LabDiscardOutlier,test.data=lab.data,filter=lab.filter,
    SIMPLIFY=FALSE)
	
	# Save dataset to file
	save(lab.data, file = rda.filename)
	return(lab.data)
}

LabOutlierFilter<-function(test.data,num.sd) {
# Creates an outlier filter for a given dataset based on standard deviations
  test.mean<-mean(test.data[,3])
  test.sd<-sd(test.data[,3])
  filter.max<-test.mean+num.sd*test.sd
  filter.min<-test.mean-num.sd*test.sd
  return(c(filter.max,filter.min))
}

LabDiscardOutlier<-function(test.data,filter) {
# Discards outliers in dataset 
  test.data<-test.data[(test.data[,3]<filter[1]&test.data[,3]>filter[2]),]
  return(test.data)
}
  
TestToData<-function(test.name,data) {
	# Given full data, returns a dataset containing only observations corresponding to a 
	# particular test
	#
	# Args:
	#	test.name: name of test of interest
	#	data: dataset containing only the form of interest
	#
	# Returns:
	#	A dataset that contains only observations for the test specified in test.name, containing
	#	the columns subject.id, lab.delta, test.name, test.result, and test.unit
	
	# Fields
	fields<-c("Laboratory Delta","Test Name","Test Result","Test Unit")
	
	# Columns
	columns<-c("lab.delta","test.name","test.result","test.unit")

	# Get record ids for one test
	test.record<-data$record.id[data$value==test.name]	
	
	# Get all data that matches those record ids
	test.data<-data[data$record.id %in% test.record,]
			
	# Get dynamic data
	test.data<-GetDynamicData(test.data,fields,columns)
		
	# Coerce test result to numeric and drop NAs
	# Sometimes test result includes strange entries: eg, one entry of "A" in glucose
	test.data$test.result<-as.numeric(test.data$test.result)
	test.data<-test.data[!is.na(test.data$test.result),]
	
	return(test.data)
}

ConvertTestUnits<-function(i,test.data,conversions,col.names) {
	# Given a test dataset, returns that dataset with test.results converted to
  # consistent units
	#
	# Args:
	#	i: the index of the dataset in the dataset list 
	#	test.data: a list of test datasets 
	#	conversions: a list of conversion tables 
	#
	# Returns:
	#	A test dataset where test.results are converted to consistent units.
  # Columns are: subject.id, a delta column named with the test name, 
  # and test.results.  In addition, missing values and duplicate deltas are
  # dropped, and the dataset is sorted by subject.id and then delta.

	# Get name of test 
	name<-col.names[i]
	print(name)
  
	if (length(conversions[[i]])>1) {
		 # Drop all test results that do not have units found in the 
		 # conversion list
		test.data[[i]]<-test.data[[i]][test.data[[i]]$test.unit %in% 
		    names(conversions[[i]]),]
    
		# Replace old result by multiplying by conversion factor from table
		test.data[[i]]$test.result<-
			unlist(conversions[[i]][test.data[[i]]$test.unit])*
      test.data[[i]]$test.result
	}
	
	# Drop test name and test units
	test.data[[i]]<-test.data[[i]][,!(colnames(test.data[[i]]) %in%
    c("test.name","test.unit"))]

	# Sort by subject id and delta
	test.data[[i]]<-test.data[[i]][order(test.data[[i]]$subject.id,
    test.data[[i]]$lab.delta),]
  
	# Remove records with missing lab.delta, test.result, or subject.id
	test.data[[i]] <- test.data[[i]][!is.na(test.data[[i]]$lab.delta)&
    !is.na(test.data[[i]]$test.result)&
    !is.na(test.data[[i]]$subject.id),]
	
	# Check for duplicate deltas and delete those rows   
	# Create combination of subject.ids and deltas	
	id<-paste(as.integer(test.data[[i]]$subject.id),
	          as.integer(test.data[[i]]$lab.delta),sep=".")
	
	# Check for duplicates in id
	dup<-duplicated(id)
	
	# Keep only nonduplicates - throws away second record with same delta
	test.data[[i]]<-test.data[[i]][!dup,]

	# Rename test.result column
	colnames(test.data[[i]])[colnames(test.data[[i]])=="test.result"]<-name
	
	# Rename delta column
	colnames(test.data[[i]])[colnames(test.data[[i]])=="lab.delta"]<-
		paste(name,"delta",sep=".")

	return(test.data[[i]])
}

GetConvertList<-function(test.names) {
	# Given a vector of test names, returns a list of conversion tables for only
  # those tests
	#
	# Args:
	#	test.names: a vector of test names of interest
	#
	# Returns:
	#	A list of conversion tables 

	# Create full list of conversions
	convert.list<-list(
		Bicarbonate=list("mmol/l"=1),
		`HbA1c (Glycated Hemoglobin)` = list("%"=1, "V/V"=100),
		'Red Blood Cells (RBC)' = list("10E12/L"=1,"per mm3"=1E-6,"10E6/mm3"=1,"10E9/L"=1E-3),
		#'White Blood Cells (WBC)' = list("10E9/L"=1,"per mm3"=1E-4,"g/L"=,"10E3/mm3"=100), g/L?
		'Creatine Kinase' = list("IU/L"=1,"U/L"=1,"mIU/L"=1), # the mIU/L seem like already on same order of magnitude
		Chloride=list("mmol/l"=1),
		Triglycerides=list("mmol/l"=1,"g/L"=1.13),
		Glucose=list("mmol/l"=1,"g/L"=5.55),
		'Total Cholesterol'=list("mmol/l"=1,"g/L"=2.58),
		#Albumin=list("g/l"=1,"%"=?), #% of what? blood serum protein?
		Sodium=list("mmol/l"=1),
		Hemoglobin=list("g/l"=1,"mmol/L"=1/.06206,"mg/L"=.001),
		Calcium=list("mmol/l"=1,"mg/L"=.025,"mEq/L"=0.5),
		# Monocytes=list("%"=,"10E9/L"=,"per mm3"=,"g/L"=), need WBC count to convert %
		# Lymphocytes=list("%"=,"10E9/L"=,"per mm3"=,"g/L"=), need WBC count to convert %
		Hematocrit=list("%"=1,"V/V"=100,"1"=100),
		# Neutrophils=list("%"=,"10E9/L"=,"per mm3"=,"g/L"=), need WBC count to convert %
		'Alkaline Phosphatase'=list("U/L"=1,"IU/L"=1),
		'Gamma-glutamyltransferase'=list("U/L"=1,"IU/L"=1),
		# Protein=list("g/L"=1,"g/dL"=.1,"G/L"=1,"mEq/L"=), how to convert mEq/L?
		# Eosinophils=list("%"=,"10E9/L"=,"per mm3"=,"g/L"=,"10E3/mm3"), need count to convert 
		'AST(SGOT)'=list("u/l"=1,"iu/l"=1),
		# Basophils=list("%"=,"10E9/L"=,"per mm3"=,"g/L"=,"10E3/mm3"), need count to convert %
		Phosphorus=list("mmol/l"=1,"mg/L"=.0323),
		Creatinine=list("umol/l"=1,"mg/L"=8.84),
		Potassium=list("mmol/l"=1),
		'Bilirubin (total)'=list("umol/l"=1,"mg/L"=1.71),
		#Platelets=list("10E9/L"=,"per mm3"=,"g/L"=,"10E3/mm3"=), g/L?
		'ALT(SGPT)'=list("u/l"=1,"iu/l"=1),
		'Urine Ph'=list("pH"=1) 
	)
	# Keep only those elements that are in test.names
	return(convert.list[test.names])
}

TestUnitsUsed<-function(data) {
	data<-as.data.frame(data)
	return(unique(data$test.unit))
}

NumTestSubs<-function(test.names) {
	n.test<-length(test.names)
	# create matrix to hold test names and how many subjects they have data for
	 num.test.subjects<-	data.frame(test.names=test.names,subjects=rep(-99,length(test.names)),stringsAsFactors=FALSE)
	# Count the number of subjects per test
	 for (i in 1:n.test) {
		num.test.subjects[i,2]<-length(unique(data$subject.id[data$value==test.names[i]]))
	}
	num.test.subjects<-num.test.subjects[order(num.test.subjects$subjects),]		
	plot(num.test.subjects$subjects)	
	return(num.test.subjects)
}