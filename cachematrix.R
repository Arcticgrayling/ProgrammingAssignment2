## Coursera R Course, Peter Olsen July, 2015
## Programing assignment 2
## These functions will provide a way to cache the results
## from inverting a matrix

## This Function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){     # Set the value of the original matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x     # Give the value of the original matix if called
        setsolve <- function(solve) m <<- solve         # Calculate and set m to the inverse matrix
        getsolve <- function() m                        # Give the inverse matrix if called
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
}


## This function computes the inverse of the special "matrix" returned by
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
        data <- x$get()  # get the matrix to inverse
        m <- solve(data, ...)  # solve for the invere matrix
        x$setsolve(m)           # set the matrix
        m                       # return m
}
