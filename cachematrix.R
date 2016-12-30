## Coursera Programming Assignment #2

## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Function 1 - makeCacheMatrix: This function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        imat <- NULL
        set <- function(y) {
          x <<- y
          mat <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) imat <<- inverse
        getInverse <- function() imat
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
        }


## Function 2 - cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        imat <- x$getInverse()
        if (!is.null(imat)) {
              message("retrieving data")
              return(imat)
        }
        xmat <- x$get()
        imat <- solve(xmat, ...)
        x$setInverse(imat)
        imat
  }
