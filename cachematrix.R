## Put comments here that give an overall description of what your
## functions do

## This function is used to create a special object that stores a matrix and cache's its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function is used to get the inverse of the matrix created by the above function.
## In addition to that, it first checks whether inverse is already computed for the matrix.
## If the inverse is already computed, then it skips the computation and gets the inverse
## directly from the cache. If the inverse is not computed, then it computes the inverse
## and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
