## Coursera R Course, Peter Olsen July, 2015
## Programing assignment 2
## These functions will invert a function and provide a way to cache the results
## 

## This Function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){             # Set the value of the original matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                             # Get the value of the original matix if called
        setsolve <- function(solve) m <<- solve         # Calculate and set m to the inverse matrix
        getsolve <- function() m                        # Give the inverse matrix if called
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
}


## This function caches the inverse matrix returned by
## the makeCacheMatrix function (above)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()       # get the inverse matrix from makeCacheMatrix if it exists
                                # m will be NULL if it doesn't exist
        
        ## check if the inverse matrix (m) is already cached
        if(!is.null(m)){
                # if m allready exists - notify user and return it
                message("getting cached data")
                return(m)
        }
        # if m - the inverse matrix- doesn't exist, create and save it.
        data <- x$get()                 # get the original matrix
        m <- solve(data, ...)           # compute the invere matrix if is not already cached
        x$setsolve(m)                   # set the inverse matrix
        m                               # return m
}
