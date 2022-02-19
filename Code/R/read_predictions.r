ReadPredictions <- function(in.file="predicted.out") {
	# Read slope predictions from file in contest format 
	#
	# Returns:
	# 	two column matrix with subject ids as first column
	#   	and predictions as second column
	
	return(read.csv(in.file, header=FALSE, sep=","))
}