##Assignment 2
## x is a square invertible matrix

makeCacheMatrix<- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
## this function returns a list containing function to set and get
## the matrix and then set and get the inverse

  get <- function() x
  setinverse <- function(inverseerse) inverse <<- inverseerse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## returns the inverse of matrix
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()			   
## check if inverse is already calculated, then skip and get the matrix from cache instead
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
## if not calculated already, then calculate the inverse 
  mat.data <- x$get()
  inverse <- solve(mat.data, ...)
  
## set the value of inverse 
  x$setinverse(inverse)
  return(inverse)	## return the inverse
}
