# auxiliar function to check k
check_pos_integer<-function(k){
  if(!is.numeric(k)){
    stop(paste("\n",k,"must be a number"))
  }
  if(k%%1!=0){
    stop(paste("\n",k,"must be an integer"))
  }
  if(k<1){
    stop(paste("\n",k,"must be a positive integer equal or bigger to 1"))
  }
  TRUE
}



#' @title Simulate bootstrap sampling
#' @description This function simulates bootstrap sampling of partial factor scores
#' @param dataset - data set
#' @param sets - list of vectors indicating the sets of variables
#' @param ncomps - integer, how many number of components
#' @param center - logical argument, whether to center
#' @param scale - logical arugment, whether to scale
#' @param k - integer, size of sampling for each bootstrap variable
#' @param B - integer, size of bootstrap sampling
#' @return List containing:
#'
#' Mean  - List of bootstrap means for each axis
#'
#' Variance  - List of bootstrap variance for each axis
#'
#' Ratio  - List of bootstrap ratio for each axis
#'
#' Size  - Size of bootstrap samples
#'
#' Dimension - Dimension of bootstrap samples (number of axis)
#' @export
#' @examples
#' b.sample <- bootstrap(dataset=mtcars,sets=list(1:3,4:6,7:8,9:11), ncomps=NULL, center=TRUE, scale=TRUE, k=10,B=100)



bootstrap<- function(dataset,sets, ncomps=NULL, center=TRUE, scale=TRUE, k,B){
  check_pos_integer(k)
  check_pos_integer(B)


  MFA <-mfa(dataset,sets=sets,ncomps=ncomps,center=center,scale=scale)

  if(is.null(ncomps)){
    ncomps=ncol(MFA$MatrixEFG[[1]]) #since last few component might be too trivial to include
  }


  F_star<-list() #dummy list

  for(j in 1:B){
    sample<-sample(length(MFA$MatrixEFG),size=k,replace=TRUE) #sample k-tables w/replacement
    llist<-matrix(rep(0,ncomps*nrow(dataset)),ncol=ncomps) #dummy matrix
    for(i in sample){
      llist<-llist+MFA$MatrixEFG[[i]] #add each table up
    }
    F_star[[j]]<- llist/k #first F_star value
  }

  # Finding bootstrap mean, variance and ratio
  mean<-list()
  var<-list()
  ratio<-list()
  axis1<-vector()
  for(m in 1:ncomps){
    axis1<-vector()
    for(b in 1:B){
      axis1<-cbind(axis1,as.vector(F_star[[b]][,m]))
    }
    mean[[m]]<-rowMeans(axis1)
    var[[m]]<-apply(axis1,1,var)
    ratio[[m]]<-mean[[m]]/var[[m]]
  }

  summary<-list(Mean=mean, Variance=var, Ratio=ratio, Size=B, Dimension=ncomps)
  class(summary) <- "bootstrap"
  summary

}


#' @title Plot method for 'bootstrap' object
#' @description This function defines a plot method for "bootstrap" object
#' @param  x - bootstrap-object
#' @param \dots further arguments ignored
#' @return plot method for "bootstrap" object
#' @export



plot.bootstrap<-function(x, ...){

  for(i in 1:x$Dimension){
    barplot(x$Ratio[[i]], xlab=paste0("Axis",i), horiz=TRUE,xaxt='n', yaxt='n',xlim=c(min(x$Ratio[[i]]),max(x$Ratio[[i]]))
            #          , names.arg=x$names,pos=3
    )
    abline(v=0,col="gray60")
    text(0, 0, "0", col = "red")
    abline(v=-3,col="gray60")
    text(-3, 0, "-3", col = "red")
    abline(v=3,col="gray60")
    text(3, 0, "+3", col = "red")
    title(sprintf("Bootstrap Ratios with %s Dimensions",x$Dimension))
  }

}


