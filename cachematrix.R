## The purpose of these functions is to first establish a matrix and cache its inverse,
## and then to call upon that cached inverse rather than re-calculate repeatedly.

## This function lets you define a matrix, invert the matrix, and cache the result for future use.

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function () x
                setInv <- function() i <<- solve(x)
                getInv <- function () i
                list(set = set, get = get, setInv = setInv, getInv = getInv)
}       



## This function lets you recall the cached matrix inverse rather than re-calculate it.

cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
        i  ## Return a matrix that is the inverse of 'x'
}
