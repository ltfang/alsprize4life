WritePredictions <- function(predicted, out.file="predicted.out") {
	# Write slope predictions to file in contest format and in ascending order 
	# of subject id
	#
	# Args:
	#	predicted: two column matrix with subject ids as first column
	#   	and predictions as second column
	
	# Write predictions in ascending order of subject id
	write.table(predicted[order(predicted[,1]),], file=out.file, 
		append=FALSE, quote=TRUE, sep=",",
		col.names=FALSE, row.names=FALSE, qmethod="escape")
	# Write a copy of predictions to "predicted.out"
	# write.table(predicted[order(predicted[,1]),], file="predicted.out", 
		# append=FALSE, quote=TRUE, sep=",",
		# col.names=FALSE, row.names=FALSE, qmethod="escape")
}