## This file has two functions: makeCacheMatrix, cacheSolve
##
## makeCacheMatrix creates a special "matrix" with four functions:
## Set matrix value; get matrix value; set inverse of the matrix, get inverse of the matrix
##
## cacheSolve checks whether the inverse of the matrix is already solved. 
## If the inverse is available, it will retrun the value directly.
## If not, it will solve the inverse, save it, and return the value.


## makeCacheMatrix creates a list of four function.
## set: save the value of matrix, and clear the inverse
## get: get the value of matrix
## setinverse: set the value of inverse of matrix
## getinverse: get the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve first checks whether the inverse is available
## If available, it will return the inverse directly
## If not, it will solve the inverse, save it, and return the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
