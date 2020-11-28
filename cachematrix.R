## This file contains functions to create a matrix object, 
## then return its inverse.

## Makes a matrix from a list, then calculates the inverse and caches it.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## Set the value of the matrix.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Get the value of the matrix.
        get <- function() x
        
        ## Get the inverse.
        getInv <- function() m
        
        ## Set the inverse of the matrix.
        setInv <- function(solve) m <<- solve
        
        ## Return Matrix and its inverse.
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Return the inverse of a given matrix from cache or calculation.

cacheSolve <- function(x, ...) {
        ## Check if the the inverse has been cached.
        m <- x$getInv()
        
        ## If inverse is cached, return the inverse.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Else, calculate the inverse and return it.
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}