Reload <- function(package.name) {
# Removes, installs, and loads library from source
#
# Args:
#	package.name = name of package to be reloaded
#
# Example usage:
#	Reload("Code")
# 
install.packages(package.name,repos=NULL, type="source") 
library(package.name,character.only=TRUE)
remove.packages(package.name)
install.packages(package.name,repos=NULL, type="source") 
if (package.name=="Code") {
	detach("package:Code")
}
else if (package.name=="Models") {
	detach("package:Models")
}
library(package.name,character.only=TRUE)	
}

# Reload standard packages
Reload("Code")
Reload("Models")

# Call standard libraries
library("digest")
library("BayesTree")