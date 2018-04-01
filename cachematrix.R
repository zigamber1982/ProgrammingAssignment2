## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#set the value of the matrix
#get the value of the matrix
#set the inverse of the matrix
#get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  invMtemp <- NULL
  
  set <- function(y) {
    x <<- y
    invMtemp <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(invM) invMtemp <<- invM
  
  getInv <- function() invMtemp
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## Write a short comment describing this function
## Compute the inverse and cache result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMtemp <- x$getInv()
  
  #if
  if(!is.null(invMtemp)){
    message("getting cached matrix")
    return(invMtemp)
  }
  #else
  data <- x$get()
  
  invMtemp <- solve(data, ...)
  
  x$setInv(invMtemp)
  
  #Return
  invMtemp
}
