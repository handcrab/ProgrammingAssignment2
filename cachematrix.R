## These functions allow us to cache the inverse of a matrix: 
## makeCacheMatrix creates a special matrix-object that can store(cache) its inverse, 
## and cacheSolve calculates the inverse of a given special matrix-object 
## in case it doesnt cache one already.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    # if matrix has been chanched - reset the inverse
    inverse <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse
  setinverse <- function(inv) inverse <<- inv
  # get the value of the inverse
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {    
    inv <- x$getinverse()
    # if it's inverse was calculated before - return the value
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    # else - calculate the inverse and save it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    # return the inverse value
    inv
}
