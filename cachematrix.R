## Below is a pair of functions that cache the inverse of a matrix
## to optimize potentially time-consuming computations.

## Creates a special "matrix" object that can cache its inverse. Returns
## a list of 4 functions: set & get matrix value, set & get inverse value

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## sets initial values for the matrix and its inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## gets the value of the original matrix
    get <- function() x
    ## sets the new inverse value (if it changed)
    setInverse <- function(newInverse) {
        inverse <<- newInverse
    }
    ## gets inverse value
    getInverse <- function() inverse
    ## returns the list of these 4 functions
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not 
## changed), it retrieves the inverse from the cache. Returns the inverse.

cacheSolve <- function(x, ...) {
    ## gets inverse from cache, checks if it is not null
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ## if no inverse in cache, gets matrix and computes its inverse
    data <- x$get()
    inverse <- solve(data, ...)
    ## pushes computed inverse into cache, returns the inverse
    x$setInverse(inverse)
    inverse
}