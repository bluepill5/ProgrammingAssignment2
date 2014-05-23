# Matrix inversion is usually a costly computation and their may be some
# benefit to caching the inverse of a matrix rather than compute it 
# repeatedly. Computing the inverse of a square matrix can be done with 
# the solve function in R. For example, if X is a square invertible 
# matrix, then solve(X) returns its inverse. The pair of functions below 
# cache the inverse of a matrix.

# NOTE: We assume that the matrix supplied is always invertible

# 1. makeCacheMatrix: This function creates a special "matrix" object that 
#    can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# 2. cacheSolve: This function computes the inverse of the special "matrix" 
#    returned by makeCacheMatrix above. If the inverse has already been
#    calculated (and the matrix has not changed), then the cachesolve 
#    should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setInverse(i)
    i
}
