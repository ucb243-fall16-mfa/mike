# auxiliar function to check the dimension of table1 and table2
check_tables<-function(table1,table2){
  if(nrow(table1)!=nrow(table2)){
    stop("\n'table1' and 'table2' must have same number of rows")
  }
  TRUE
}

# auxiliar function to check order1 and order2
check_orders<-function(order1,order2,MFA){
  if(order1>length(unique(MFA$MatrixPCol))){
    stop("\n'order1' is out of the range")
  }
  if(order2>length(unique(MFA$MatrixPCol))){
    stop("\n'order2' is out of the range")
  }
  TRUE
}

#' @title Compute Lg coefficient between 2 tables
#' @description This function gets the Lg coefficients to study the Between-Table Structure
#' @param  MFA - a "mfa" object
#' @param table1 - matrix of the first table
#' @param table2 - matrix of the second table
#' @param order1 - the number of table1 in order
#' @param order2 - the number of table2 in order
#' @return lg - lg coefficients of two tables
#' @export
#' @examples
#' mfa1 <- mfa(data=mtcars,sets=list(1:3,4:6,7:8,9:11), ncomps = NULL, center = TRUE, scale = TRUE)
#' table_1 <- mfa1$MatrixZ[,1:3]
#' table_2 <- mfa1$MatrixZ[,4:6]
#' lg <- Lg(MFA=mfa1,table1=table_1,table2=table_2,order1=1,order2=2)

Lg<-function(MFA,table1,table2,order1,order2){
  if(class(MFA)!="mfa"){
    stop("Please input 'mfa' object")
  }
  check_tables(table1,table2)
  check_orders(order1,order2,MFA)

  weight<-unique(MFA$MatrixPCol)

  table1<-as.matrix(table1)
  table2<-as.matrix(table2) #get the matrix of two tables

  table11<-table1%*%t(table1)
  table22<-table2%*%t(table2)

  upbefore<-table11%*%table22

  up<-sum(diag(upbefore))  #get the  numerator

  lg<-up*weight[order1]*weight[order2]

  return(lg)
}
