##Creating conditions to calculate an inverse matrix: Square matrix

MakeCacheMatrix <- function(square){
  X <- as.matrix(square)
  
  
  if(ncol(X)>=3 | (nrow(X)>=3)) {
    stop('Theres not square matrix')
  }
  if(ncol(X)==1){
    stop('Is a vector')
  }
  if(ncol(X)==2){
    
##We have to get the determinant (Y) of the matrix and test the last condition Y==0
    a <- X[1]
    b <- X[3]
    c <- X[2]
    d <- X[4]
    Y <- a * d - b * c
    if(Y==0){
      stop("Determinant = 0 means inverse matrix doesnt exist")}

##And now, we could get each element of the inverted matrix  
    A <- d/Y
    B <- -b/Y
    C <- -c/Y
    D <- a/Y
    
##Finally, we're gonna create an inverted matrix 
    
    newmatrix <- as.matrix(cbind(c(A,C), c(B,D)))
    }
  }
