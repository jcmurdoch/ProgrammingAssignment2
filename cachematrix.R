## Code to calculate the inverse of a matrix and cache the result
## so that it can be called again without having to recalculate 
## the inverse each time the user needs it.

## Makes a list of functions that include (1) set - sets the value
## of the matrix, (2) get - returns the matrix, (3) setinv - sets
## the value of the inverse of the matrix, and (4) getinv - returns
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
               x <<- y
               i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Returns the inverse of a matrix of type makeCacheMatrix.
## If the inverse is cached, returns the cached inverse.
## If the inverse is not cached, calculates the inverse,
## caches the inverse, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
