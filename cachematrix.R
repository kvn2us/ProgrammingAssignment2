## Caching the Inverse of a Matrix

## This script has the pair of functions used to cache the inverse of a matrix.

## The pair consists of two user defined functions named 
## 'makeCacheMatrix' and 'cacheSolve':

## 1. 'makeCacheMatix' function creates a special "matrix" object that can 
##    cache its inverse.
## 2. 'cacheSolve' function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. If the inverse has already been 
##    calculated (and the matrix has not changed), then the 'cacheSolve' 
##    should retrieve the inverse from the cache.

## The first function takes the steps:
##    begins by setting the inverse matrix 'm' to NULL as a placeholder 
##    for a future value;
##    then applies functions 'set' to set the matrix 'x' to a new matrix 'y' 
##    and resets 'm' to NULL and 'get' to returns the vector, x;
##    further it uses 'setInverse' to set the inverse, m, to Solve that returns 
##    inverse of 'x', and 'getInverse' to return 'm';
##    and finally, returns the 'special matrix' containing all of these functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(i) m <<- solve(x)
  getInverse <- function() m
  list(set = set,get = get, setInverse = setInverse, getInverse = getInverse)
}



## The second function calculates the inverse of special "matrix",
## that is previously defined by the first function. Initially, it checks if 
## the inverse of the matrix has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse of the data and sets the inverse matrix in the cache
## via 'setInverse' function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    matrx <- x$get()
    m <- solve(matrx, ...)
    x$setInverse(m)
    m
}
