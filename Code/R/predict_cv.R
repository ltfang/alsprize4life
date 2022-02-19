PredictCv <- function(predictor, args=list(), data.name, num.cv) {
	# Run a given predictor on each cross-validation train-test split.
	#
	# Args:
	#	predictor: the predictor function to run; will be called as
	#		predictor(train.name, test.name, args)
	#	args: list of additional arguments used by predictor
	# 	data.name: base name of RDA file used to generate cv data
	#	num.cv: number of cross-validation folds
	#
	# Example Usage:
	#	PredictCv(GroundTruthPredict, list(), "train", 10)

	# Run predictor on each CV train-test split
	file.pattern <- "%s-cv%s_%sof%d"
	for (fold in 1:num.cv) {
		train.name <- sprintf(file.pattern,data.name,"train",fold,num.cv)
		test.name <- sprintf(file.pattern,data.name,"test",fold,num.cv)
		predictor(train.name, test.name, args)
	}
}