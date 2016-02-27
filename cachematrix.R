## The purpose of cacheMatrix is to avoid computing again the inverse of a Matrix, 
## which is costly in terms of resource, if the caluckation has already been done.
## inversion, which is usually a costly computation. This is done with caching, which  
## uses two funcions, one for creating the cache for the inversed matrix and
## one which fills the cache matrix the first time it runs and calculates the inverse
## and skips the calculation afterwards.
##
## makeCacheMatrix creates a list of functions which:
## - set the value for the matrix
## - get the value for the matrix
## - set a value for the inverse matrix
## - get the value of the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## cacheSolven returns the inverse of the matrix. If the inverse has not been computed yet
## (first time it is run) it computes it (through solve()), and fills the cache (through setinverse()) 
## If it has already been computed, it skips the calcuation and give the cached result 
## with the message "getting cached data" 
##
cacheSolve <- function(x, ...) {
        c <- x$getinverse()
        if(!is.null(c)) {
                message("getting cached data.")
                return(c)
        }
        data <- x$get()
        c <- solve(data)
        x$setinverse(c)
        c
}
