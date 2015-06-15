## Below two function that cache and
## compute inverse of a metrix

## Create an object that cache its inverse

makeCacheMatrix <- function(mrix = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mrix <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mrix);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}



## Compute inverse matrix and pass to the above function

cacheSolve <- function(mrix, ...) {
    inverse <- mrix$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    data <- mrix$get()
    invserse <- solve(data, ...)
    mrix$setinv(inverse)
    return(inverse)
}
