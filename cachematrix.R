# Both these functions work together to take a square matrix and
# determine its inverse
# If the matrix's inverse was already calculated, then instead of
# calculating it again, a cached inverse value is returned


# This function makes a new data structure that holds a square matrix
# along with its cached inverse, if it had been calculated in the past
# and not changed
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function calculates the inverse of a given square matrix
# If the result had already been calculated and cached, then the
# cached result is returned
# Otherwise, the matrix's inverse is calculated, cached, and returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
