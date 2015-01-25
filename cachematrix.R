## Function descriptions:
## 
## Matrix inversion is usually a costly computation and their may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## 
## Two functions are defined here in order to facilitate the operations.
## 
##   makeCacheMatrix: This function creates and returns a special matrix object that 
##                    can cache its inverse. 
##
##   cacheSolve: The function computes the inverse of the special matrix object returned
##               from makeCacheMatrix. If the inverse of the matrix has already been 
##               calculated, then the cacheSolve should retrieve it from the cache.
##
##   Usage example:
##       
##    > x <- makeCacheMatrix()
##    > x$set(matrix(1:4, 2, 2))
##    > cacheSolve(x)
##         [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
##    > cacheSolve(x)
##    getting cached data
##         [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
##    >

##
## makeCacheMatrix:
##  
## Create and return a special matrix object that contains methods in getting and setting 
## its inverse of the matrix.
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(matrix) m <<- matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## 
## cacheSolve:
##
## Given the matrix object returned from the makeCacheMatrix, 
##
##     1. If its inverse is null, compute the inverse of the matrix, 
##        cache and return it.
##  
##     2. If the inverse of the matrix is not null, it means it was 
##        previously computed. Just return it.
##

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
	  
	  ## Return a matrix that is the inverse of 'x'
        m
}

