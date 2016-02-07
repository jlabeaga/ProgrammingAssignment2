## These functions allow to compute the inverse of a matrix
## and store it in memory so that we can use the cached value
## once it has been computed for the first time
##
## An example of utilization would be;
##
##     # create the initial matrix
##     m <- matrix(c(1,2,3,4), 2, 2)
##     # create the caheable object
##     mc <- makeCacheMatrix(m)
##     # compute the inverse for the first time
##     inv <- cacheSolve(mc)
##     # the second time the cached value is returned
##     inv <- cacheSolve(mc)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
