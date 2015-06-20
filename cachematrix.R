# This exercise is very similar to the example 'Caching the Mean of a Vector'. 
# It's key to understand how the example functions work though. If you get that,
# you can mostly reuse the two functions. Make sure you change the input for 
# makeCacheMatrix to a matrix instead of a numeric vector. Also make sure you are 
# using solve() instead of mean(). I have explained how i have interpreted the 
# functions makeCacheMatrix and cacheSolve along the way:


makeCacheMatrix <- function (x = matrix()) {
 
        # Define a set of functions to:
        #       - set the value of the matrix
        #       - get the value of the matrix
        #       - set the value of the inverse
        #       - get the value of the inverse
        # Then produce a list containing these functions
        # which you will use as input in cacheSolve
        
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}


cacheSolve <- function (x, ...) {
        
        # First, cacheSolve should do a lookup in makeCacheMatrix:
        # if getinverse is not null (meaning the inverse of x has been cached before)
        # the function should return the message "cached value" and return the inverse 
        # of x, skipping the calculation of the inverse.
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("cached value")
                return(inverse)
        }
        
        # If getinverse is null, the inverse of x should be calculated. 
        # The inverse of x is then passed to makeCacheMatrix so that the output of the 
        # calculation can be reused.
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse     
}


