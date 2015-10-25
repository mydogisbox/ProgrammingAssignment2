## makeCacheMatrix creates a list which has the ability to get and set a matrix value
## and cache the inverse of the matrix

## This function creates a list which contains four functions:
## 1. a function to get the wrapped matrix
## 2. a function to set the wrapped matrix
## 3. a function to get the cached inverse of the matrix
## 4. a function to set the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    set <- function(y = matrix())
    {
        x <<- y
        inverse <<- NULL
    }
    getinverse <- function() inverse
    setinverse <- function(i) inverse <<- i
    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## This function takes a "CacheMatrix" and returns a matrix which is the
## inverse of the "CacheMatrix" by either returning the cached inverse if
## it exists or by calculating (and caching) the inverse

cacheSolve <- function(x, ...) {
    cachedInverse <- x$getinverse()
    if(!is.null(cachedInverse)) 
    {
        message("getting cached data")
        return(cachedInverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
