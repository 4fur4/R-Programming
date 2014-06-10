## This function creates a extended version of matrix that caches its inverse
## when calculating it. It relies on getters and setters created add-on.

makeCacheMatrix <- function(x = matrix()) {
    inInver<- NULL
    set <- function(y) {
        x <<- y
        inInver <<- NULL
    }
    get <- function() x
    setInverse <- function(outInver) inInver <<- outInver
    getInverse <- function() inInver
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function calculates the inverse of a matrix of the type makeCacheMatrix when this is not 
## already calculated. When the inverse is already calculated it directly returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inInver <- x$getInverse()
    if(!is.null(inInver)) {
        message("getting cached data")
        return(inInver)
    }
    data <- x$get()
    inInver <- solve(data, ...)
    x$setInverse(inInver)
    inInver
}
