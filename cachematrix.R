## Inverting a very large matrix is a time-consuming computation, 
## so calculating the inverse and then caching it for future use 
## is potentially time-saving.  
##
## The two R functions in this exercise, makeCacheMatrix and cacheSolve, 
## calculate and cache the inverse of a (numeric) input matrix. 
## It is assumed that the input matrix is invertible. 
##
## 
## makeCacheMatrix creates a list of four objects from an input matrix. 
## One object is the inverse of the matrix, which will be passed to 
## the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL				
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,		 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve compares the inverse of the input matrix that was
## returned by makeCacheMatrix to a stored matrix inverse.
## If the inverse has already been calculated, and the input matrix 
## has not changed, cacheSolve returns a message that cached data 
## is being accessed, and it retrieves the inverse from the cache.  

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
