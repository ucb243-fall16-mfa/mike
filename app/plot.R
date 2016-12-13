# auxiliar function to check the length of the labels
check_label<-function(MFA,labels){
  if(nrow(MFA$MatrixZ)!=length(labels)){
    stop("\n'labels' must have the same length as the number of the rows in the data")
  }
  TRUE
}

# auxiliar function to check the dimensions
check_dim<-function(MFA,dim){
  if(dim>ncol(MFA$MatrixF)){
    stop(paste0("\n'",dim,"' is out of the range, please check!"))
  }
  if(!is.numeric(dim)){
    stop(paste("\n",dim,"must be a number"))
  }
  if(dim%%1!=0){
    stop(paste("\n",dim,"must be an integer"))
  }
  if(dim<1){
    stop(paste("\n",dim,"must be a positive integer equal or bigger to 1"))
  }
  TRUE
}

#' @title Plot of compromised factor scores
#' @description This function generates plot of compromised  factor scores across two selected axes
#' @param data - data set
#' @param sets - list of vectors indicating the sets of variables
#' @param center - logical argument, whether to center
#' @param scale - logical arugment, whether to scale
#' @param dim1 - integer, selected dimension to plot on x-axis
#' @param dim2 - integer, selected dimension to plot on y-axis
#' @param labels - label for each row
#' @return plots of compromised factor scores for the two selected axes
#' @export
#' @examples
#' plot_factorscore(mtcars[c(1:2,4:5,8:9,20:21),],sets=list(1:3,4:6,7:8,9:11),dim1=1,dim2=2,labels=c("Maz1","Maz2","Hor1","Hor2","Merc1","Merc2","Toyota1","Toyota2"))



plot_factorscore<-function(data,sets,center=TRUE,scale=TRUE,dim1,dim2,labels){

  MFA<-mfa(data,sets=sets,ncomps=NULL,center=center,scale=scale)
  check_label(MFA,labels)
  check_dim(MFA,dim1)
  check_dim(MFA,dim2)


  library(stringr)
  number<-str_extract(labels,"[[:digit:]]+")
  category<-str_extract(labels,"[a-zA-Z]+")

  library(ggplot2)
  p=ggplot(as.data.frame(MFA$MatrixF),aes(x=MFA$MatrixF[,dim1],y=MFA$MatrixF[,dim2],color=category))+labs(x=paste("Axis",dim1),y=paste("Axis",dim2))
  p=p+geom_point(size=3.8)
  p=p+geom_text(aes(label = number, vjust = 1.1, hjust = -0.5))
  print(p)

}


#' @title Plot of partial factor scores and variable loadings for two selected dimensions
#' @description This function generates plots of partial factor scores and variable loadings for two selected dimensions
#' @param data - data set
#' @param sets - list of vectors indicating the sets of variables
#' @param center - logical argument, whether to center
#' @param scale - logical arugment, whether to scale
#' @param dim1 - integer, selected dimension to plot on x-axis
#' @param dim2 - integer, selected dimension to plot on y-axis
#' @param labels - label for each row
#' @return plots of partial factor scores and variable loadings for the two selected axes
#' @export
#' @examples
#' plot_partialfs(mtcars[c(1:2,4:5,8:9,20:21),],sets=list(1:3,4:6,7:8,9:11),dim1=1,dim2=2,labels=c("Maz1","Maz2","Hor1","Hor2","Merc1","Merc2","Toyota1","Toyota2"))


plot_partialfs<-function(data,sets,center=TRUE,scale=TRUE,dim1,dim2,labels){


  MFA<-mfa(data,sets=sets,ncomps=NULL,center=center,scale=scale)
  check_label(MFA,labels)
  check_dim(MFA,dim1)
  check_dim(MFA,dim2)

  library(stringr)
  category<-str_extract(labels,"[a-zA-Z]+")


  library(ggplot2)
  plot<-list()
  for(i in 1:MFA$Sets){
      plot[[i]]<-ggplot(as.data.frame(MFA$MatrixEFG[[i]][,c(dim1,dim2)]),aes(x=MFA$MatrixEFG[[i]][,dim1],y=MFA$MatrixEFG[[i]][,dim2],color=category))+ggtitle(paste("Observation",i))+
        geom_point(size=3.8)+geom_text(aes(label=labels,vjust = 2, hjust = 0.5))+labs(x=paste("Axis",dim1),y=paste("Axis",dim2))
      print(plot[[i]])
  }
}

#' @title Plot method for 'mfa' object
#' @description This function generates i)plot of compromised  factor scores ii)plot of partial factor scores & loadings across two selected axes
#' @param MFA - a 'mfa' object
#' @param labels - label for each row
#' @param dim1 - integer, selected dimension to plot on x-axis
#' @param dim2 - integer, selected dimension to plot on y-axis
#' @return
#' i) Plot of compromised  factor scores across two selected axes
#'
#' ii) Plots of partial factor scores & loadings across two selected axes
#' @export
#' @examples
#' plot(MFA=mfa(mtcars[c(1:2,4:5,8:9,20:21),],sets=list(1:3,4:6,7:8,9:11)),labels=c("Maz1","Maz2","Hor1","Hor2","Merc1","Merc2","Toyota1","Toyota2"),dim1=1,dim2=2)


plot.mfa <- function(MFA,labels,dim1,dim2){
  if(class(MFA)!= 'mfa'){
    stop("\n 'MFA' must be a 'mfa' object")
  }
  ### Checking input arguments
  check_dim(MFA,dim1)
  check_dim(MFA,dim2)

  ### Creating label parts
  library(stringr)
  number<-str_extract(labels,"[[:digit:]]+")
  category<-str_extract(labels,"[a-zA-Z]+")

  library(ggplot2)
  ### Compromised factor score plot
  p=ggplot(as.data.frame(MFA$MatrixF),aes(x=MFA$MatrixF[,dim1],y=MFA$MatrixF[,dim2],color=category))+labs(x=paste("Axis",dim1),y=paste("Axis",dim2))+ggtitle("Compromised Factor Scores")
  p=p+geom_point(size=3.8)
  p=p+geom_text(aes(label = number, vjust = 1.1, hjust = -0.5))
  print(p)

  ### Partial factore score plot
  plot<-list()
  for(i in 1:MFA$Sets){
    plot[[i]]<-ggplot(as.data.frame(MFA$MatrixEFG[[i]][,c(dim1,dim2)]),aes(x=MFA$MatrixEFG[[i]][,dim1],y=MFA$MatrixEFG[[i]][,dim2],color=category))+ggtitle(paste("Partial Factor Scores & Variable Loadings - Observation",i))+
      geom_point(size=3.8)+geom_text(aes(label=labels,vjust = 2, hjust = 0.5))+labs(x=paste("Axis",dim1),y=paste("Axis",dim2))
    print(plot[[i]])

  }}
