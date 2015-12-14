#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()){
  m_inv <- NULL
  
  #set function to set matrix
  setMatrix <- function(x){
    m <<- x
    m_inv <<- NULL
  }

  # get function to retrieve matrix details
  getMatrix <- function(){
    return(m)
  }

  # set matrix inverse function
  setMatrixInv <- function(x_inv){
    m_inv <<- x_inv
  }

  #get matrix inverse function
  getMatrixInv <- function(){
    return(m_inv)
  }

  #Create a list
  list(setMatrix = setMatrix, 
	 getMatrix = getMatrix,
	 setMatrixInv = setMatrixInv,
	 getMatrixInv = getMatrixInv)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  inv <- x$getMatrixInv()

  #if cached matrix is not null, return it
  if(!is.null(inv)){
    message("Retrieving Cached matrix:")
    return(inv)
  }
  
  # calculate inv of matrix
  matrixData <- x$getMatrix()
  
  #Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
  inv <- solve(matrixData)
  
  #set inverse 
  x$setMatrixInv(inv)
  
  #return inv matrix
  return(inv)
}


