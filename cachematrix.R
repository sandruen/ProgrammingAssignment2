## These functions are used to compute the inverse of a matrix and
## cache the result. So if we need the value of the inverse again, it can be looked
## up in the cache instead of computing it again.

## This function creates a special matrix

makeCacheMatrix <- function(x) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {inv <<- inverse}
    getinverse <- function() {inv}
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'
## If the inverse value already exists, it sends a message, fetches the value and
## returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
