## This two functions were create for the Programming Assignment 2 
## of R Programming Course of Coursera.
## The main purpose is to use the <<- operator to assign a value to an 
## object in an environment that is different from the current environment.


## The makeCacheMatrix function creates a "special" matrix object that can
## cache its inverse. This function will return a list of functions that 
## stores a matrix and its inverse on its environment.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       
       ## The setobj function assign the matrix on environment.
       setobj <- function(u) {
               x <<- u
               m <<- NULL
       }
       
       ## The getobj function gets the matrix from enviornment
       getobj <- function() x
       
       ## The setinverse function assign the inverse matrix of x on environment
       setinverse <- function(solve) {
               m <<- solve
       }
       
       ## The getinverse function gets the inverse matrix of x from environment
       getinverse <- function() m
       
       ## Return list of functions
       list(setobj = setobj, 
            getobj = getobj, 
            setinverse=setinverse, 
            getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated,
## then cacheSolve will retrieve the inverse from the cache, otherwise it will
## calculate the inverse and then assign the result into cache.

cacheSolve <- function(x, ...) {
       
       ## Assign m from the cache using getinverse function.
       m <- x$getinverse()
       
       ## If m exists, then skpis the computation
       if (!is.null(m)) {
         
               message("Getting cached data for inverse of a matrix.")
               
       } else {
               
               message("Calculating the inverse of a matrix and assign the result in cache.")
               ## If m doesn't exist, then assign m0 with tha cache of x 
               ## using getobj function.
               m0 <- x$getobj()
               ## Calculate inverse matrix of m0, using solve function.
               m <- solve(m0) 
               ## Set the inverse matrix of x on cache
               x$setinverse(m) 
               
       }
       
       ## Return m - the inverse matrix of x
       return(m)
}
