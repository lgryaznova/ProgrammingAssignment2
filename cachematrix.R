## Below is a pair of functions that cache the inverse of a matrix
## to optimize potentially time-consuming computations.

## Creates a special "matrix" object that can cache its inverse. Returns
## a list of 4 functions: set & get matrix value, set & get inverse value

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(newInverse) {
        inverse <<- newInverse
    }
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not 
## changed), it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}