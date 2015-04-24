## With these functions we can wrap a matrix and store its inverse along side it.
## Usage is demonstrated below:
## > source('cachematrix.R')
## > cacheTree <- makeCacheMatrix(tree)
## > cacheTree$get()
## [,1] [,2] [,3]
## [1,]    2    1    0
## [2,]    2    0    0
## [3,]    2    0    1
## > cacheTree$getmean()
## NULL
## > cacheTree$getinverse()
## NULL
## > cacheSolve(cacheTree)
## [,1] [,2] [,3]
## [1,]    0  0.5    0
## [2,]    1 -1.0    0
## [3,]    0 -1.0    1
## > cacheTree$getmean()
## NULL
## > cacheTree$getinverse()
## [,1] [,2] [,3]
## [1,]    0  0.5    0
## [2,]    1 -1.0    0
## [3,]    0 -1.0    1
## > cacheMean(cacheTree)
## [1] 0.8888889
## > cacheTree$getmean()
## [1] 0.8888889
## > cacheTree$getinverse()
## [,1] [,2] [,3]
## [1,]    0  0.5    0
## [2,]    1 -1.0    0
## [3,]    0 -1.0    1
## >

## Creates an object wrapping a matrix with cached mean and inverse.

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
## Return the mean of the matrix 'x'

