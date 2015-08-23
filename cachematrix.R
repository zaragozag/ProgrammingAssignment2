## These functions compute the inverse of a given matrix and then store said
##inverse to a cache. When we wish to compute the inverse of the matrix and the 
##matrix has not changed since the last time we computed it, instead of computing it 
##again, the second of these functions (cacheSolve) will return the cached value via 
##the getinverse function created in the makeCacheMatrix function.

##The first function, makeCacheMatrix creates a list containing 4 functions
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse
##4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     get <- function() x
     setinverse <- function(inv) inverse <<- inv
     getinverse <- function() inverse
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


##cacheSolve checks to see if the inverse of the matrix has already been 
##computed. If so, it returns the cahced inverse. Otherwise, it calculates
##the inverse, stores the inverse to the cache, and returns the inverse 

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inverse <- x$getinverse()
     if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
     }
     data <- x$get()
     inverse <- solve(data, ...)
     x$setinverse(inverse)
     inverse
}