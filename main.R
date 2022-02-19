install.packages("digest")
#install.packages("nnet", repos=NULL,type="source")
install.packages("BayesTree")
install.packages("Code",repos=NULL,type="source")
install.packages("Models",repos=NULL,type="source")

library("digest")
library("BayesTree")
library("Code")
library("Models")
#library("glmnet")

# Load modified version of bart.R
# source("Models/R/bart.R")

#model<-RandomForestTrain("train",args=list(R=10))
#model<-GlmnetTrain("train",args=list(R=10,pls=TRUE))
#model<-GbmTrain("train")
#predictions<-model("test")

# Save any prior random number generator seed
#old.seed <- .Random.seed
# Set random number generator seed for reproducibility
new.seed <- 9438
set.seed(new.seed)
# test.name<-"val"
test.name<-"test"
# Train model
features = c("static","alsfrs","fvc","svc","vital")
slope.features = c("alsfrs","fvc","svc","vital")
# args=list(test.name=test.name,R=10,pls=FALSE,prune=FALSE,std=FALSE,
# Setting test.name=NULL so that NAs are not imputed with the test set
args=list(test.name=NULL,R=10,pls=FALSE,prune=FALSE,std=FALSE,
	features=features,slope.features=slope.features)
model <- BartTrain(train.name="traintest",target=NULL,args=args)
# model <- GlmnetTrain(train.name="traintest",target=NULL,args=args)

# Form predictions on test set
predictions<-model(test.name)

# Write predictions to file
write.table(predictions[order(predictions[,1]),], file="predicted.out", 
	append=FALSE, quote=TRUE, sep=",",
	col.names=FALSE, row.names=FALSE, qmethod="escape")
	
# Restore prior RNG seed
#.Random.seed <- old.seed
