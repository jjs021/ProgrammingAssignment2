## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        i <<- NULL
}
get <- function() x 
setmean <- function(mean) m <<- mean
getmean <- function() m
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set = set, get = get,
    setmean = setmean,
    getmean = getmean,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Given a CacheMatrix will either give the cached solution [inverse] or calculate,
## cache and return the solution.

cacheSolve <- function(x, ...) {
    i <- x$getmean()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
## Return a matrix that is the inverse of 'x'

## Given a CacheMatrix will either give the cached mean or calculate,
## cache and return the mean.

cacheMean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
## Return a mean of the matrix 'x'

