## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly.
## This file contains a pair of functions that can be used to cache the inverse of a matrix.

## Example usage:
## > x <- matrix(rnorm(9), nrow= 3)        # Create a matrix
## > mcx <- makeCacheMatrix(x)             # Create the special matrix
## > mcx$get()                             # Get the special matrix
## > cacheSolve(mcx)                       # Return the inverse matrix
## > cacheSolve(mcx)                       # Return the inverse matrix from cache, while calling cacheSolve(mcx) again

## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## It returns list of functions to:
  # Set the value of the matrix
  # Get the value of matrix
  # Set the value of inverse
  # Get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  # Get the matrix
  get <- function() x
  
  # Set the inverse of matrix
  setinverse <- function(inverse) invr <<- inverse
  # Get the inverse of matrix
  getinverse <- function() invr
  
  # Return the list of functions
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function "cacheSolve"computes the inverse of the special "matrix" returned by "makeCacheMatrix" above.
## If the inverse has already been calculated (and the matrix has not changed),
## then this function will retrieve the inverse from the cache and return it.

cacheSolve <- function(x, ...) {
  invr <- x$getinverse()
  
  # If inverse is already calculated and available in cache, return the cached data.
  if(!is.null(invr)) {
    message("getting cached inverse matrix data.")
    return(invr)
  }
  
  # If inverse is not calculated yet, calculate it.
  data <- x$get()
  invr <- solve(data, ...)
  
  # Cache the inverse
  x$setinverse(invr)
  
  # Return the inverse
  invr
}
