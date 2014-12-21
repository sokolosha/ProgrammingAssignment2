## The functions below are designed to calulate the inverse of a matrix
## or return a cached inverse if available

## makeCacheMatrix creates several helper function to calculate an inverse 
## and to return an inverse already calculated
## the input is a matrix (assumed to be a square one), th eoutput is a list of helper functions
## and the environment they exist in
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    if (nrow(x) != ncol(x)) {
        message("Input matrix must be a square one")
        return()
    }
    ## define set function
    set <- function(y) {
        x <<- y
        i <<- NULL  ## clear the inverse
    }
    
    ## define get function
    get <- function() x   ## return what you have

    setinverse <- function(solve) i <<- solve  ## apply solve fintion to calculate inverse

    getinverse <- function() i  ## just return the inverse you have 

    # return a function/environment list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    message("calculating fresh inverse")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    inv
}


