## Matrix inversion is a costly computation and it may be beneficial to cache
## the inverse of the matrix rather than compute it repeatedly. The purpose of
## the functions below is to cache the computed inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It Includes functions that can set and get the matrix, and set and get the
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## Cashed matrix inverse
        inv <- NULL
        ## Set and get the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        ## Set and get the matrix inverse
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        ## Return a list of functions for the matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned
## by the function makeCacheMatrix. If the inverse has already been
## computed (and the matrix has not changed), then the cacheSolve
## function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
