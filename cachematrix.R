## These functions alleviate the potentially costly
## computations necessary to perform matrix inversion
## by caching the inverse of a matrix rather than
## computing it repeatedly.

## The makeCacheMatrix function creates a special
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          set <- function(y) {
                  x <<- y
                  i <<- NULL
          }
          get <- function() x
          setinv <- function(inverse) i <<- inverse
          getinv <- function() i
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)
}


## The cacheSolve function computes the inverse of
## the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
            message("getting cached inverse")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
