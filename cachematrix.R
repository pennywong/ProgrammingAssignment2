## Caching the inverse of a matrix rather than computing it repeatedly.

## creates a special "matrix" object that can cache its inverse
## just like create an object that has a private cached element and some access methods
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
    set <- function(y) {
    	x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseArgus) inverse <<- inverseArgus
    getInverse <- function() inverse
    list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}


## if the inverse has already been calculated, then retrieve the inverse from the cache
## or calculate the inverse and set it to the cache
cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}