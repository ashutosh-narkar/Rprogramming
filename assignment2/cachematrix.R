## Caching the Inverse of a Matrix

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve retrieves the inverse from the cache.


## makeCacheMatrix creates a special "matrix" and provides methods to:
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the inverse
## 4 - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
        
}


## Compute the inverse of a square invertible matrix.
## If inverse is already calculated get it from the cache and do not compute again
## else calculate inverse of matrix and cache result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # check if inverse(x) is already calculated
    # if so, get it from the cache and do not compute again
    inverse <- x$getinverse()
    
    if (!is.null(inverse)) {
        message("getting cached inverse of matrix")
        return(inverse)
    }
    
    # calculate inverse of matrix as its not cached
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
    
}
