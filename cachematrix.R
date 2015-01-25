## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. These functions are designed to do it.

## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
    ## 'mat' is a matrix object which we want to compute and 
    ## cache its inverse.
    
    ## Return a list containing a function to
    ## 1. set the value of the matrix
    ## 2. get the value of the matrix
    ## 3. set the value of the inverse
    ## 4. get the value of the inverse
    
    inv <- NULL
    
    ## function for setting the value of the matrix
    set <- function(m) {
        mat <<- m
        inv <<- NULL
    }
    ## function for getting the value of the matrix
    get <- function() mat
    
    ## function for setting the value of the inverse
    setinverse <- function(invmat) inv <<- invmat
    ## function for getting the value of the inverse
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.

cacheSolve <- function(mat, ...) {
    ## 'mat' is a special "matrix" object returned by the
    ## makeCacheMatrix function.
    
    ## Return the inverse of the matrix.
    
    ## check if the inverse has already been calculated
    inv <- mat$getinverse()
    if(!is.null(inv)) {
        ## get the inverse from the cache and skip the computation
        message("getting cached data")
        return(inv)
    }
    ## calculate the inverse
    data <- mat$get()
    inv <- solve(data, ...)
    ## set the value of the inverse in the cache
    mat$setinverse(inv)
    inv
}