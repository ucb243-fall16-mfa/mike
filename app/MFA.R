# auxiliar function to check the range of the sets
check_sets<-function(data,sets){
  for(i in 1:length(sets)){
    for(j in 1:length(sets[[i]]))
      if(sets[[i]][j]>ncol(data)){
        stop("\n'sets' is beyond range of the dataset")
      }
  }
  TRUE
}

# auxiliar function to check the logical input for center
check_center<-function(center){
  if(!is.logical(center)){
    stop("\n'center' must be TRUE or FALSE")
  }
  TRUE
}

# auxiliar function to check the logical input for scale
check_scale<-function(scale){
  if(!is.logical(scale)){
    stop("\n'scale' must be TRUE or FALSE")
  }
  TRUE
}

# auxiliar function to check the number of the components
check_ncomps <- function(data,ncomps) {
  if (!is.null(ncomps)){
    if (!is.numeric(ncomps)|ncomps==0) {
      stop("\n'ncomps' must be a number bigger than 0")
    }
    if (ncomps>=nrow(data)) {
      stop("\n'ncomps' must be smaller than number of the total variables")
    }
  }

  TRUE
}


#' @title Multiple factor analysis
#' @description This function demostrates basic functionalities of Multiple Factor Analysis
#' @param data - data set
#' @param sets - list of vectors indicating the sets of variables
#' @param ncomps - integer, how many number of components
#' @param center - logical argument, whether to center
#' @param scale - logical argument, whether to scale
#' @return List containing:
#'
#' Dimension - Number of the components
#'
#' Sets - Number of the sets
#'
#' M - masses for the rows
#'
#' A - squared singular value derived weights for the k tables
#'
#' PLin - Vector with weights for lines
#'
#' PCol - Vector with weights for columns
#'
#' MatrixA  - Matrix with eigenvalues (Variances)
#'
#' MatrixU  - Matrix U of the SVD of Matrix Z
#'
#' MatrixV  - Matriz V of the SVD of Natrix Z
#'
#' MatrixF  - Compromise Factor Score Matrix
#'
#' MatrixEFG - Partial Factor Score Matrix
#'
#' MatrixCCP - Matrix with Correlation of Principal Components with Groups
#'
#' MatrixEscVar - Matrix of Partial Inertia
#' @export
#' @examples
#' mfa1 <- mfa(data=mtcars,sets=list(1:3,4:6,7:8,9:11), ncomps = NULL, center = TRUE, scale = TRUE)

mfa <- function(data, sets, ncomps = NULL, center = TRUE, scale = TRUE) {
  check_sets(data,sets)
  check_ncomps(data,ncomps)
  check_center(center)
  check_scale(scale)

    MBQ <- function(DataQ, center=TRUE, scale=TRUE) {
      #center, scale and get the weight of M and A

      # Input:
      # DataQ - raw data to be dealt with
      # center - whether centering
      # scale - whether scale

      # Returns:
      # MZ   - centered and scaled matrix(according to choice)
      # PLin - line weights
      # PCol - column weights

      MZ <- NULL    #
      PLin <- NULL   #
      PCol <- NULL   #

      ### Begin - Center and scale data  ###
      MC <- as.matrix(DataQ) #

      if(center==TRUE)
      {
        Media <- apply(MC,2,mean) # Matrix with averages by columns

        MC <- sweep(MC, 2, Media, FUN = "-") # Centering
      }

      if(scale==TRUE){

        Media <- apply(MC,2,mean) # Matrix with averages by columns

        MC_after <- sweep(MC, 2, Media, FUN = "-") # Centering

        SqSum <- sqrt(colSums(MC_after^2))

        MC <- sweep(MC, 2, SqSum, FUN = "/") # Scaling
      }

      ### End - Center and scale ###

      PLin <- rep(1/nrow(MC),nrow(MC))

      Pe <- (svd(MC)$d[1])^2   # find the first eigenvalue of MC

      PCol <- cbind(PCol,t(rep(1/Pe,ncol(MC)))) # matrix weights of column

      Lista <- list(MZ=MC, PLin=PLin, PCol=PCol)

      return(Lista)
    }


    if (is.null(sets)) # Creates names for the variables if not exist
      sets <- paste("Variable", 1:10, sep = " ")

    ### Begin - Get the weight of the groups of variables
    NumSets = length(sets) # number of groups formed

    MZG   <- NULL  # general Z null array
    PLinG <- NULL  # general matrix with null line weights
    PColG <- NULL  # general matrix with null column weights

    for (i in 1:NumSets) {
### Start revising
      if (TRUE) { # we only process quantitative data now

        if (i==1)
          j<-1
        else
          j<-j+length(sets[[i]])
        MB   <- MBQ(data[,sets[[i]]],center=center,scale=scale)
        MZ   <- MB$MZ
        PLin <- MB$PLin
        PCol <- MB$PCol
        colnames(PCol) <- colnames(data[,sets[[i]]])
      }
### End revising
      PLinG <- PLin  # general matrix with line weights

      PColG <- cbind(PColG,PCol) # general matrix with column weights

      MZG   <- cbind(MZG,MZ)     # centered and scaled matrix

    }

    PColG <- t(PColG)
    ### End - Get the weight of the groups of variables ###

    ### Begin - Find the eigenvectors and eigenvalues ###
    MDS <- GSVD(MZG, PLinG, PColG) #
    MAutoVlr  <- MDS$d  #
    MAutoVecU <- MDS$u  #
    MAutoVecV <- MDS$v  #

### Start revising### Revisedx2
    MEigen <- as.data.frame(matrix(NA, length(MAutoVlr), 5))
    rownames(MEigen) <- paste("Axis", 1:length(MAutoVlr))
    colnames(MEigen) <- c("Singular value","Eigenvalue", "% cumulative eigenvalue","% Inertia","% cumulative Inertia")
    MEigen[, "Singular value"] <- MAutoVlr
    MEigen[, "Eigenvalue"] <- MAutoVlr^2
    MEigen[, "% cumulative eigenvalue"] <- cumsum(MEigen[, "Eigenvalue"])
    MEigen[, "% Inertia"] <- (MAutoVlr^2/sum(MAutoVlr^2)) * 100
    MEigen[, "% cumulative Inertia"] <- cumsum(MEigen[,"% Inertia"])
### End revising

    NumAutoVlr <- length(MAutoVlr) # Number of variables

    NE <- length(MAutoVlr[MAutoVlr>1e-10]) # Number of significant elements
    ### End - Find the eigenvectors and eigenvalues ###

    ### Begin - Matrix of compromise factor score ###
    MF <-  MAutoVecU[,1:NE]%*%diag(MAutoVlr[1:NE],NE) # Matrix F - compromise factor score
    rownames(MF) <- rownames(data) # name the rows
    colnames(MF) <- paste("Axis", 1:ncol(as.matrix(MF)), sep = " ") # name the columns
    ### End - Matrix of compromise factor score ###

### Start revising
    ### Begin - Matrix of factor scores by group ###

    LMFGrupo <- as.list(1:NumSets) #

    for (i in 1:NumSets) {
      if (i==1)
        j<-1
      else
        j<-j+length(sets[[i-1]])

      MFG <- NumSets * MZG[,j:(j+length(sets[[i]])-1)]

      MFG <- sweep(MFG, 2, PColG[j:(j+length(sets[[i]])-1)], FUN="*")

      LMFGrupo[[i]] <- MFG%*%MAutoVecV[j:(j+length(sets[[i]])-1),] # create matrix of partial factor scores by group

      colnames(LMFGrupo[[i]]) <- paste("Axis", 1:ncol(as.matrix(LMFGrupo[[i]])), sep = " ") #

    }

    names(LMFGrupo) <- paste("Group", 1:NumSets, sep = "") # name of group
    ### End - Matrix of factor scores by group ###
### End revising

    ### Begin -  Correlation of Principal Components with Original Variables ###
    CCP <- sweep(as.matrix(MAutoVecV), 2, MAutoVlr, FUN = "*")
    CCP <- t(CCP)
    rownames(CCP) <- paste("Axis", 1:NumAutoVlr, sep = " ")
    colnames(CCP) <- colnames(MZG)
    ### End -  Correlation of Principal Components with Original Variables ###

    ### Begin - Partial matrix of inertia/scores of variables ###
    CoordVar <- sweep(as.matrix(MAutoVecV), 2, sqrt(MAutoVlr), FUN = "*")  # Coordenadas das variaveis

    ContrVar <- sweep(as.matrix(CoordVar^2), 2, MAutoVlr, "/") # Contribuicao das variaveis

    ContrVar <- sweep(as.matrix(ContrVar), 1, PColG, "*")

    ContrGru <- matrix(data = NA, nrow = NumSets, ncol = NumAutoVlr) # Matriz com Contribuicoes dos Grupos

### Begin revising
    for (i in 1:NumSets) {
      if(i==1)
        j<-1
      else
        j<-j+length(sets[[i-1]])
      ContrGru[i,] <- apply(ContrVar[j:(j+length(sets[[i]])-1), ], 2, sum) # Matriz com Contribuicoes dos Grupos

    }
### End revising

    EscVar <- sweep(ContrGru, 2, MAutoVlr^2, "*") # create matrix of variables scores/partial inertia

    colnames(EscVar) <- paste("Axis", 1:ncol(as.matrix(EscVar)), sep = " ") # name the columns

    #rownames(EscVar) <- c("group 1", "group 2") # name of rows ???
    ### End - Partial matrix of inertia/scores of variables ###
    dim <- ncol(as.matrix(EscVar))
    A= unique(PColG)
    rownames(A)<-paste0("table",1:length(A))
    colnames(A)<-c("weight")


    if(is.null(ncomps))
      Lista <- list(Dimension = dim, Sets= NumSets, M = unique(PLinG), A= A,
                    MatrixPLin = PLinG,
                    MatrixPCol = PColG, MatrixZ = MZG, MatrixA = MEigen,
                    MatrixU = MAutoVecU, MatrixV = MAutoVecV, MatrixF = MF,
                    MatrixEFG = LMFGrupo, MatrixCCP = CCP, MatrixEscVar = EscVar)

    else {
      EFG<-list()
      dim = ncomps
      for(i in 1:NumSets)
        EFG[[i]]<-LMFGrupo[[i]][,1:ncomps]

      Lista <- list(Dimension = dim, Sets= NumSets, M = unique(PLinG), A= A,
                    MatrixPLin = PLinG,
                    MatrixPCol = PColG, MatrixZ = MZG, MatrixA = MEigen[1:ncomps,],
                    MatrixU = MAutoVecU[,1:ncomps], MatrixV = MAutoVecV, MatrixF = MF[,1:ncomps],
                    MatrixEFG = EFG, MatrixCCP = CCP, MatrixEscVar = EscVar[,1:ncomps])
    }

  class(Lista)<-"mfa"
  return(Lista)
}

#' @title Print method for 'mfa' object
#' @description This function defines a print method for "mfa" object
#' @param  x - mfa-object
#' @param \dots further arguments ignored
#' @return basic information of the 'mfa' object including class, dimension, number of the sets, row mass and table weight of the 'mfa' object
#' @export

print.mfa<-function(x, ...){
  cat('object: "mfa"\n')
  cat(sprintf('Dimension: "%s"\n', x$Dimension))
  cat(sprintf('Number of the sets: "%s"\n', x$Sets))
  cat(sprintf('Mass of each row:"%s" \n',x$M))
  cat("Weight of each set:\n")
  print(x$A)
}

