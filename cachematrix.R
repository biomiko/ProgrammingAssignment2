
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #Reset cached values of input matrix and output inverted matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #Return value of input matrix
    get <- function() x
    #Set the cached version of the inverted matrix 
    setSolve <- function(solve) m <<- solve
    #Return the cached version of the inverted matrix
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

#If the inverse is not changed, then result is retreived from the cache. Otherwise, inverse matrix is calculated.

cacheSolve <- function(x, ...) {
    # Attempt to retrieve inverted matrix from the cache
    m <- x$getSolve()
    #If inverted matrix is found in cache, return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #otherwise, matrix has not been inverted
    data <- x$get()
    m <- solve(data, ...)
    #cache the inverted version of the matrix
    x$setSolve(m)
    #return the inverted version
    m
}