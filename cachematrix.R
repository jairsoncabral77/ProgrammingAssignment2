## Put comments here that give an overall description of what your
## functions do

## make a matrix which inverse is stored in cache
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- null
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if ( ! is.null(i) ) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}

