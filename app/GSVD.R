#' @title Generalized Singular Value Decomposition
#' @description This function performs Generalized Singular Value Decomposition
#' @param Data - raw Matrix used for Decomposition
#' @param PLin - Vector with weights for lines
#' @param PCol - Vector with weights for columns
#' @return List containing:
#'
#' d - Vector line with the singular values of the result
#'
#' u - Line eigenvectors
#'
#' v - Column relative eigenvectors
#' @export
#' @examples
#' gvsd1 <- GSVD(Data=mtcars)

GSVD <- function(Data, PLin = NULL, PCol = NULL) {

  if (is.null(PCol)) PCol <- rep(1, ncol(Data))

  if (is.null(PLin)) PLin <- rep(1, nrow(Data))

  else if (is.numeric(PLin)) PLin = PLin / sum(PLin)


  if (!is.numeric(PLin))
    stop("Input to 'PLin' must be of the numeric vector type. Check!")

  if (!is.numeric(PCol))
    stop("Input to 'PCol' must be of the numeric vector type. Check!")

  if (nrow(Data) != length(PLin))
    stop("The number of elements in 'Plin' must be equal to the number of lines of the 'Data' component. Check!")

  if (ncol(Data) != length(PCol))
    stop("The number of elements in 'PCol' must be equal to the number of columns of the 'Data' component. Check!")


  PLin <- as.vector(PLin)

  PCol <- as.vector(PCol)

  ncv <- min(nrow(Data)-1,ncol(Data)) # Number of valid columns

  AA = sweep(Data, 2, sqrt(PCol), FUN = "*")

  AA = sweep(AA, 1, sqrt(PLin), FUN = "*")

  MSVD <- svd(AA)
  d <- MSVD$d
  MU <- MSVD$u
  MV <- MSVD$v

  P <- diag(sqrt(1/PLin))%*%MU

  Q <- diag(sqrt(1/PCol))%*%MV

  Resp <- list(d = d[1:ncv], u = P[,1:ncv], v = Q[,1:ncv])

  return(Resp)
}
