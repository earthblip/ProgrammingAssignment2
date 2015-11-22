## Coursera R Programming Course Assignment 2:
## R functions makeCacheMatrix and cacheSolve: Calculate the inverse of
## a matrix and also cache it to avoid its recalculation.
## makeCacheMatrix: Returns a special "matrix" (actually a list of functions)
## used by cacheSolve.
## cacheSolve: Use makeCacheMatrix results as input to return the cached
## inverse of a matrix if available or else calculate the inverse and cache it.


## Accept a regular matrix as input and return a special "matrix" 
## (actually a list of functions) that allows caching of a matrix
## and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    matInv <- NULL
    set <- function(y) {
        x <<- y
        matInv <<- NULL
    }
    get <- function() x
    setinv <- function(matrixInverse) matInv <<- matrixInverse
    getinv <- function() matInv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return the cached inverse of a matrix when possible, or else invert the
## matrix, using as input the special "matrix" (actually a list of matrices)
## created by makeCacheMatrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matInv <- x$getinv()
    if(!is.null(matInv)) {
        message("getting cached data")
        return(matInv)
    }
    data <- x$get()
    matInv <- solve(data, ...)
    x$setinv(matInv)
    matInv
}
