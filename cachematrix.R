## Assignment2: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of 
## a matrix rather than computing it repeatedly. 
## The assignment is to write a pair of functions that cache the inverse of a matrix.


## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## inverse stores the values of the matrix and the setvalue function sets the values of the matrix
        inverse <- NULL
        
        setvalue <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        ## getvalue gets the value of the matrix
        getvalue <- function() x
        
        ## setinverse sets the value of the inverse
        setinverse <- function(solve) inverse <<- solve
        ## getinverse gets the value of the inverse
        getinverse <- function() inverse
        
        ## returns the matrix 
        list(setvalue = setvalue, getvalue = getvalue, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        
        ## In case the inverse was already calculated, it skips the calculation and returns the cached data
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## in case the inverse is not yet calculated, it calculates it
        data <- x$getvalue()
        inverse <- solve(data, ...)
        
        ## Caches the inverse
        x$setinverse(inverse)
        
        ## Returns it
        inverse
}
