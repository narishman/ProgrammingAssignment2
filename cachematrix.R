# These functions help optimize the runtime of creating inverse matrices by caching 
# the computed inverse matrix and reuse them if the base matrix does not change.

## Function : makeCacheMatrix
## This function creates an object with the following attributes
## x              - holds the matrix
## invmatrix      - holds the inverse matrix
## set()          - sets up the matrix within the object for x
## get()          - gets the matrix from within the object from x
## setinvmatrix() - sets up the inverse matrix within object for invmatrix
## getinvmatrix() - gets the inverse matrix within object from invmatrix

makeCacheMatrix <- function(x = matrix()) {
    invmatrix <- NULL
    set <- function (y) {
        x <<- y
        invmatrix <<- NULL
    }
    get <- function() x
    setinvmatrix <- function (i) invmatrix <<- i
    getinvmatrix <- function () invmatrix
    list (set = set, get = get,
          setinvmatrix = setinvmatrix,
          getinvmatrix = getinvmatrix)
}


## Function : cacheSolve
## This function takes in an object of 'makeCacheMatrix' and queries it for an inverse matrix
## If an inverse matrix is found cached, it returns the inverse matrix
## Else it computes the inverse matrix and caches it within the object and also returns it to 
## the caller.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinvmatrix()
    if (!is.null(i)) {
        message("getting cached inverted matrix") 
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix)
    x$setinvmatrix(i)
    i
    
}
