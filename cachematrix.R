############################################################################################
##
##      Auxiliary functions for inverting matrices and caching result
##
##      - makeCacheMatrix:
##              Creates a cache-object from an input square matrix
##
##      - cacheSOlve:
##              Takes a cache-object as input and returns the inverted matrix
##              The inverted matrix is calculated and cached (if not previously cached)
##              or it is retrieved from the cache (if previously calculated and cached)
##
##########################################################################################

############################################################################################
## function: makeCacheMatrix
##
##      creates a wrapper object around a matrix
##      The wrapper consists of 4 functions:
##          get, set:       get and set the value of the original matrix wrapped here
##          getInv, setInv  get and set the inverse of the original matrix wrapped here
##
##      Usage:
##          makeCacheMatrix(x = matrix())
##              x:  a matrix (should be a square, invertible matrix)
##

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(invToSetTo) inv <<- invToSetTo
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


############################################################################################
##  function:   cacheSolve
##
##      Takes as input a wrapper object (created by makeCacheMatrix)
##      Returns the inverse of the matrix wrapped in the input object
##      When called, caches the inverse of the matrix, so on subsequent calls
##          the inverse is not recalculated
##      Assumes the matrix wrapped in the wrapper object is invertible
##          (No error handling!)
##
##      Usage
##          cacheSolve(x,...)
##              x   object returned from makeCacheMatrix
##              ... arguments to pass to the inversion function 'solve'
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
