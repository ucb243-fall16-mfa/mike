#' @title Compute RV coefficient between 2 tables
#' @description This function gets the RV coefficients to study the Between-Table Structure
#' @param table1 - matrix of the first table
#' @param table2 - matrix of the second table
#' @param center - logical argument, whether to center
#' @param scale - logical argument, whether to scale
#' @return RV - RV coefficients of two tables
#' @export
#' @examples
#' mfa1 <- mfa(data=mtcars,sets=list(1:3,4:6,7:8,9:11), ncomps = NULL, center = TRUE, scale = TRUE)
#' table_1 <- mfa1$MatrixZ[,1:3]
#' table_2 <- mfa1$MatrixZ[,4:6]
#' rv.table <- RV(table1=table_1,table2=table_2,center=TRUE,scale=TRUE)

RV<-function(table1,table2,center=TRUE,scale=TRUE){

  check_center(center)
  check_scale(scale)

  table1<-as.matrix(table1)
  table2<-as.matrix(table2) #get the matrix of two tables

  check_tables(table1,table2)

  if (center==TRUE)
  {
    table1<-scale(table1,scale=FALSE)
    table2<-scale(table2,scale=FALSE)
  }                                   #center the table

  if (scale==TRUE)
  {
    table1<-scale(table1,center=FALSE)/sqrt(nrow(table1)-1)
    table2<-scale(table2,center=FALSE)/sqrt(nrow(table1)-1)
  }
  #scale the table

  table11<-table1%*%t(table1)
  table22<-table2%*%t(table2)

  upbefore<-table11%*%table22

  up<-sum(diag(upbefore))#get the  numerator

  table111<-table11%*%table11
  table222<-table22%*%table22

  downl<-sum(diag(table111)) #get the trace of left part
  downr<-sum(diag(table222)) #get the trace of right part

  down<-sqrt(downl*downr)    #get the denominator

  RV<-up/down                #get the RV coefficient

  return(RV)

}

