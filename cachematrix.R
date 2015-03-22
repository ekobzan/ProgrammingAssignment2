## These functions cache the inverse of a square matrix rather than
## computing it repeatedly

## This function creates a matrix object based on inputs and defines 
## functions that will allow the calculation of its inverse.
## The following functions are defined:
## set: stores a matrix that is passed as a parameter
## get: returns the stored matrix
## setsolve: sets the inverse
## getsolve: returns the stored inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y = matrix()) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function takes the previous matrix object as a parameter and if an
## inverse has not already been cached, it calculates it, caches it, and returns
## the value.  If it has been previously cached, it returns the cached value 
## with a message "getting cached data".

cacheSolve <- function(x=matrix(), ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setsolve(i)
        i
}