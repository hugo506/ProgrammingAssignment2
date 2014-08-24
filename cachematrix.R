## The 2 functions bellow makeCacheMatrix and cacheSolve work in toguether to 
## cache the inverse of a matrix and calculate the inverse only when needed.
## An sample invertible matrix can be createw with c=rbind(c(1, -1/4), c(-1/4, 1)),
## which will create a matrix like:
##       [,1]  [,2]  
## [1,]  1.00 -0.25  
## [2,] -0.25  1.00  

## makeCacheMatrix: 
## Creates a special "matrix" object that can 
## cache its inverse. Does not calculate the inverse itself, only stores 
## the matrix and its inverse. 
## Defines the following functions:
## set()
## get()
## setinv()
## getinv()
## The x matrix parameter is presumed to be matrix always invertible.
makeCacheMatrix <- function(matrix = matrix()) {
    
    # the inverse of the matrix x, initially null
    inverse <- NULL
    
    
    # return the matrix and clear the inverse
    set <- function(m) {
        # assign the matrix 
        matrix <<- m
        # reset the inverse
        inverse <<- NULL
    }
    # return the current matrix
    get <- function(){
        matrix
    }
    # sets the inverse of the matrix
    setinv <- function(i){
        inverse <<- i
    }
    # return the inverse matrix
    getinv <- function(){
        inverse
    }
    
    list(
        set = set, 
        get = get,
        setinv = setinv,
        getinv = getinv)

}


## cacheSolve: 
## This function computes the inverse of the special "matrix" object 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    # does the x object already have the inverse?
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # no inverse found, calculate it
    
    # get the original matrix
    data <- x$get()
    
    # calculate the inverse matrix
    i <- solve(data, ...)
    
    # store the inverse for future use
    x$setinv(i)
    
    
    # return the inverse
    i    
}
