x <- read.csv('wines.csv')
nomData <- 'wines'

quanti=names(which(sapply(x,is.numeric)))
quali=names(which(!(sapply(x,is.numeric))))
VariableChoices=quanti
nom=rownames(x)
num=c(1:length(nom))
QualiChoice=quali
IdChoices=c(1:length(VariableChoices))
Idqualisup=c(1:length(QualiChoice))

gsets<-list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
gnumset=list()
for (i in 1:length(gsets)) {
  gnumset[i] <- length(gsets[[i]])
}
glabels <- c("NZ1","NZ2","NZ3","NZ4","FR1","FR2","FR3","FR4","CA1","CA2","CA3","CA4")
gplaces <- substr(glabels,1,2)