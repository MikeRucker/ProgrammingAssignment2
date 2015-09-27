## The contents of the this file are two functions that create a 
## special object that stores a matrix and caches its inverse.

## The function makeCacheMatrix creates a special matrix object 
## that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function cacheSolve computes the inverse of the function makeCacheMatrix. 
## When the inverse has been calculated it retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("retrieves cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
