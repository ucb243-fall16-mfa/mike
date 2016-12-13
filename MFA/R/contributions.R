# auxiliar function to check the dimension of table1 and table2
check_types<-function(type){
  if(type>3 | type <1){
    stop("\n'type' must be 1, 2 or 3")
  }
  if(type%%1!=0){
    stop("\n'type' must be 1, 2 or 3")
  }
  TRUE
}


#' @title Compute contribution
#' @description This function gets the contribution
#' @param X - the raw matrix need to be dealt with
#' @param sets -  list of vectors indicating the sets of variables (i.e. the blocks)
#' @param type - 1: Contribution of an observation to a dimension
#' 2: Contribution of a variable to a dimension
#' 3: Contribution of a table to a dimension.
#' @return contributions : the contribution of required type
#' @export
#' @examples
#' contr <- contributions(X=mtcars,sets=list(1:3,4:6,7:8,9:11),type=1)

contributions<-function(X,sets, type=1) {
  check_sets(X,sets)
  check_types(type)
  ## initialize and get the mfa of the raw matrix

  MFA<-mfa(X,sets,ncomps=NULL,TRUE,TRUE) #get the mfa of raw matrix

  rcontribution<-NULL

  contribution1<-matrix(nrow=nrow(MFA$MatrixF),ncol=ncol(MFA$MatrixF))

  contribution2<-matrix(nrow=nrow(MFA$MatrixV),ncol=ncol(MFA$MatrixV))

  contribution3<-matrix(nrow=length(sets),ncol=ncol(MFA$MatrixV))

  eigen<-NULL  #the eigenvalue

  a<-NULL #the alpha weight

  m<-rep(1/nrow(X),nrow(X))  #the weight of each row

  start<-1   #start point of type3

  ##get the contribution

  if(type==1){
    for (j in 1:ncol(MFA$MatrixF))
    {
      eigen[j]<-MFA$MatrixA[j,2] #get the eigenvalue of jth dimension

      for (i in 1:nrow(X))
        contribution1[i,j]<-(m[j]*(MFA$MatrixF[i,j])^2)/eigen[j] # Contribution of ith observation to jth dimension.
    }
    rcontribution<-contribution1
  }

  else{
    if(type==2)
    {
      a<-MFA$MatrixPCol
      for (j in 1:ncol(MFA$MatrixV))
      {
        for (i in 1:nrow(MFA$MatrixV))
          contribution2[i,j]<-(MFA$MatrixV[i,j])^2*a[j]  #Contribution of ith variable to a jth dimensionn
      }
      rcontribution<-contribution2
    }

    if(type==3)
    {
      a<-MFA$MatrixPCol
      for (j in 1:ncol(MFA$MatrixV))
      {
        for (i in 1:nrow(MFA$MatrixV))
          contribution2[i,j]<-(MFA$MatrixV[i,j])^2*a[j]
      }

### Revise begin
      for (j in 1:ncol(contribution2))
      {
        for (h in 1: length(sets))
        {
          if(h==1)
            start<-1
          else
            start<-length(sets[[h-1]])+start
          contribution3[h,j]<- sum(contribution2[start:(start+length(sets[[h]])-1),j]) #Contribution of hth table to j dimension.
        }
      }
### Revise end
      rcontribution<-contribution3
    }
  }

  return(rcontribution)

}
