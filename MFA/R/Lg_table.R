#' @title Compute Lg coefficient between tables
#' @description This function gets the matrix of lg coefficient
#' @param dataset - dataset
#' @param sets - the list of table
#' @param center - logical argument, whether to be centered
#' @param scale - logical argument, whether to be scaled
#' @return lg - the matrix of rv coefficient
#' @export
#' @examples
#' lg.table <- Lg_table(mtcars,sets=list(1:3,4:6,7:8,9:11),center=TRUE,scale=TRUE)

Lg_table<-function(dataset,sets,center=TRUE,scale=TRUE){
  check_sets(dataset,sets)
  check_center(center)
  check_scale(scale)

  numtable <-length(sets)

  MFA<-mfa(dataset,sets,ncomps=NULL,center,scale)

  weight<-unique(MFA$MatrixPCol)

  table<-list()

  for (i in 1:numtable)
    table[[i]] <-MFA$MatrixZ[,sets[[i]]]

  lg<-matrix(,nrow=numtable,ncol=numtable)

  for (i in 1:numtable)
  {
    for (j in 1:numtable)
      lg[i,j]<-Lg(MFA,table[[i]],table[[j]],i,j)
  }
  return(lg)
}
