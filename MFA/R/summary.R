#' @title Print summary of eigen values of MFA
#' @description This function gets summary the eigens value of MFA
#' @param MFA - a "mfa" object
#' @param ncomps - the number of eigen values -default
#' @return List containing:
#'
#' Singular values
#'
#' Eigenvalues
#'
#' % cumulative eigenvalues
#'
#' % Inertia
#'
#' % cumulative Inertia
#' @export

summary.mfa<-function(MFA,ncomps=NULL){
  if(class(MFA)!="mfa"){
    stop("Please input 'mfa' object")
  }
  if(is.null(ncomps))
    Eigens<-MFA$MatrixA
  else
    Eigens<-MFA$MatrixA[1:ncomps,]
  return(Eigens)
}
