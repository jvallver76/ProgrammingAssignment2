## The following functions are used to calculate and cache the 
## inverse of a matrix


## makeCacheMatrix defines a structure that later on can be used
## to calculate, with the help of cacheSolve, and cache the 
## inverse of the provided matrix

makeCacheMatrix <- function(x = matrix()) {
        ## 'x' is the matrix whose inverse we want to calculate
        ## Returns a structure that can be later on passed to 'cacheSolve' as parameter
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve will first of all check if the inverse of the 
## original matrix has been already calculated. If that's the case,
## the previous calculation will be returned. Otherwise, it will 
## calculate and return the inverse matrix as well as cache the result 
## for future requests

cacheSolve <- function(x, ...) {
        ## 'x' is a structure previously created with 'makeCacheMatrix'
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$setmatrix(m)
        m
}
