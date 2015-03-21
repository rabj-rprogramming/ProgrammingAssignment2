## R Programming Assignment 2
##
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly
##
## 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # Holds the cached inverse (if present)
    inv <- NULL
    
    ## The set function sets the matrix that will be used in the computation
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # The get function returns the matrix that eill be used in the computation
    get <- function() x
    
    ## The setinverse function sets the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # The getinverse function returns the cached inverse
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## Attempt to retrieve cached value
    inv <- x$getinverse()
    if(!is.null(inv)) {
        # Successfully retrieved cached value - return this to the caller
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    
    ## Calculate inverse
    inv <- solve(data, ...)
    
    ## Store value in cache
    x$setinverse(inv)
    
    inv
}
