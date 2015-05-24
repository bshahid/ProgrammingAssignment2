## This file contains functions to find the inverse of a matrix
## passed in as an argument, and also to store in cache the inverse
## of a matrix that has been computed already once.

## This function associates a matrix object with a matrix, providing
## it with the ability to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Variable to hold the inverse of the matrix passed in.
    i <- NULL
    
    ## "set" sets the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## "get" gets the value of the matrix
    get <- function() x
    
    ## setinverse sets the value of the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    
    ## getinverse gets the value of the inverse of the matrix if
    ## stored previously.
    getinverse <- function() i
    
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function checks to see if the inverse of the matrix under
## process is already stored in the cache (from a previous
## computation. If yes, it retrieves the value from the cache and
## return it.
##
## Otherwise, it computes the inverse and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## Tries to retrieve the cached value of the inverse from the
    ## cache.
    i <- x$getinverse()
    
    ## If the inverse of the matrix was retrievable from the cache,
    ## we can return now with it. Otherwise, we move forward.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Read the data in.
    data <- x$get()
    
    ## Call the function to compute the inverse anew.
    i <- solve(data)
    
    ## Store the calculated inverse for future retrieval, if and
    ## when required.
    x$setinverse(i)
    
    ## Return the newly calculated inverse.
    i
}
