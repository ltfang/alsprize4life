################################################################################################
# Dependencies

PastStat<-function(data,field,fun,name) {

	# For a specified dataset, field, and statistic, returns a dataset with a column of unique 	
	# subject ids and the corresponding statistic over the first 91 days for those subject ids 
	#
	# Args: 
	# 	data: name of dataset, a list where each element corresponds to a unique subject id
	#	field: name of variable of interest
	#	fun: function to be applied to variable; e.g: max, min, mean, number
	#	name: name to be used for column with statistic of interest
	#
	# Returns:
	#	A dataset in which for each row corresponds to a unique subject.id.  
	#	The columns consist of subject.id and features derived from the fvc 
	# 	data.
	#
	# Example usage:
	#	#source("Code/past_stat.R") 
	#	past.stat<-PastStat(data,"fvc.delta",max,"last.fvc.date")
	
	# Apply the desired function to each element in the list
	data<-lapply(data,function(data){
		stat<-fun(data[,colnames(data)==field])})
	# Put it in matrix form with one column for subject id and one column for score
	names<-as.numeric(names(data))
	stat<-as.numeric(unlist(data))
	data<-cbind(names,stat)
	colnames(data)<-c("subject.id",name)
	return(data)
}


