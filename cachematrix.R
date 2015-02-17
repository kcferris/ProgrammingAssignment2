## These two function serve to create and manipulate pseudo-
## matrix objects that can cache the value of their inverse.
## This is useful because calculating matrix inverses can be
## resource intensive.

## makeCacheMatrix creates a "matrix" object that is really
## a list of four functions. These functions let you get the
## data in the matrix, set the data in the matrix, get the
## inverse of the matrix, and set the inverse of the matrix.
## Note that the matrix inverse is initially undefined.
## This object allows you to cache the inverse of a matrix and
## retrieve it later.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) i <<- inverse
     getInverse <- function() i
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## cacheSolve works with objects created by makeCacheMatrix.
## When run on those objects, it first checks if the inverse
## has already been calculated and is cached. If so, it simply
## returns the cached value. Else, it will calculate the inverse
## matrix and set the value in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
     i <- x$getInverse()
     if(!is.null(i)) {
          message("retrieving cached mean")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setInverse(i)
     i
}