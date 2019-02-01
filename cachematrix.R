## Put comments here that give an overall description of what your
## functions do

## These two functions create a cached version of the inverse of a matrix
## that can be accessed within the operation of the functions without
## recaluclating the inverse during every execution and thereby conserving
## memory

## Write a short comment describing this function

## makeCacheMatrix creates an environment that includes a memory cache, 
## terms, and variables to be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m = NULL
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


## Write a short comment describing this function

## cacheSolve references the environment and cache defined in makeCacheMatrix
## in order to solve the inverse of the matrix given as an argument. If
## the inverse has already been solved and stored within makeCacheMatrix, 
## cacheSolve returns the cached inverse of its argument, rather than
## recalculating the inverse.

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
