
## The function creates a special object - "matrix" that can cahche the inverse of itself.

makeCacheMatrix <- function(mtx = matrix()) {
    invrs <- NULL
    set <- function(x) {
        mtx <<- x;
        invrs <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) invrs <<- inv;
    getinv <- function() return(invrs);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## The function above calculates the inversion of the "matrix" returned by "makeCacheMatrix".If the inverse has
## "cacheSolve" should retrieve the inverse from the cache only if the inverse has already been calculated.


cacheSolve <- function(mtx, ...) {
    invrs <- mtx$getinv()
    if(!is.null(invrs)) {
        message("Getting the cached data...")
        return(invrs)
    }
    data <- mtx$get()
    invrs <- solve(data, ...)
    mtx$setinv(invrs)
    return(invrs)
}
