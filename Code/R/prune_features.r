################################################################################

PruneFeatures <- function(dataset) {
	# Prunes features that have less than 918/2 nonzeros or non NAs
	#
	# Args:
	#	dataset: dataset to be pruned
	#
	# Returns: 
	#	dataset with features that have at least 918/2 nonzeros or non NAs
	#
	# Example usage:
	# 	train.data<-PruneFeatures(train.data)
	
	dataset<-dataset[,colSums(is.na(dataset)|dataset==0)<=nrow(dataset)/2]
	
	# # drop.features<-c("Symptom-Speech","Symptom-WEAKNESS","Symptom-OTHER","Symptom-Swallowing",
	   # "Symptom-GAIT_CHANGES","Symptom-Atrophy","Symptom-Cramps","Symptom-Fasciculations",                  
	   # "Symptom-SENSORY_CHANGES","Symptom-Stiffness","Symptom-.","Race - Asian",
	   # "Race - Black/African American","Race - Caucasian","Race - Other","Age","Sex-Female",                              
 		# "Sex-Male","Aunt","Aunt (Maternal)","Cousin","Father","Grandfather (Maternal)",
 		# "Grandmother", "Grandmother (Maternal)","Mother","Uncle","Uncle (Maternal)",
 		# "Uncle (Paternal)","Son","Daughter","Sister","Brother","Neurological Disease-OTHER",
 		# "Neurological Disease-STROKE-NOS","Neurological Disease-DEMENTIA-NOS",         
 		# "Neurological Disease-PARKINSON'S DISEASE","Neurological Disease-DAT",
 		# "Neurological Disease-ALS", "Neurological Disease-BRAIN TUMOR",
 		# "Neurological Disease-STROKE-ISCHEMIC","Neurological Disease-STROKE-HEMORRHAGIC",
 		# "Study Arm-PLACEBO", "Study Arm-ACTIVE","min.height","last.height","mean.height",                             
		# "num.height.visits","sum.height","first.height.date","last.height.date",
		# "meansquares.height","sd.height","height.slope")            
	
	# dataset<-dataset[,!(colnames(dataset) %in% drop.features)]
	
	return(dataset)
	
}
