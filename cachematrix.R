## R Programming Assignment 2

## These two functions will cache the inverse of a matrix.
## In order to avoid computing an inverse more than once,
## these functions will store a matrix and cache its
## inverse.
## The next time the same matrix inverse is tried to be
## computed, the cached inverse will be returned (without
## calculating it again).

## 1st function: makeCacheMatrix
## Creating an object (matrix) that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## 2nd function: cacheSolve
## Computing the inverse of the matrix that was created in
## the 1st function above. If this inverse has been computed
## before, the cached inverse will be retuned, skipping
## the calculation.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}