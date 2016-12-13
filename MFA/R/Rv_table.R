#' @title Compute RV coefficient between tables
#' @description This function gets the RV coefficients to study the Between-Table Structure
#' @param dataset - dataset
#' @param sets - the list of table
#' @param center - logical argument, whether to center
#' @param scale - logical argument, whether to scale
#' @return rv - the matrix of rv coefficient
#' @export
#' @examples
#' rv.table <- RV_table(mtcars,sets=list(1:3,4:6,7:8,9:11),center=TRUE,scale=TRUE)

RV_table<-function(dataset,sets,center=TRUE,scale=TRUE){
  check_sets(dataset,sets)
  check_center(center)
  check_scale(scale)

  numtable <-length(sets)
  table<-list()
  for (i in 1:numtable)
  {
    table[[i]] <-dataset[,sets[[i]]]
  }

  rv<-matrix(nrow=numtable,ncol=numtable)

  for (i in 1:numtable)
  {
    for (j in 1:numtable)
      rv[i,j]<-RV(table[[i]],table[[j]],center=center,scale=scale)
  }
  return(rv)
}
