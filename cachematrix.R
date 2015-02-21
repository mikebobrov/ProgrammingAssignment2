## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix"
## that is really a list of functions that can
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse value of the matrix
## 4. get the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ## Cached value
    solution <- NULL
    ## Set the matrix value
    set <- function(y) {
        x <<- y
        solution <<- NULL
    }
    ## Get the matrix value
    get <- function() x
    ## Setting the inverse value
    setinverse <- function(inv) solution <<- inv
    ## Getting the inverse value
    getinverse <- function() solution
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function first checks if the inverse value of the matrix
## has already been calculated. If yes, it gets the inverse value
## from the cache and doesn't do the costly computation.
## Otherwise it calculates the inverse of the matrix and caches the
## result
cacheSolve <- function(x, ...) {
    result <- x$getinverse()
    if(!is.null(result)) {
        message("getting cached inverse value")
        return(result)
    }
    data <- x$get()
    result <- solve(data, ...)
    x$setinverse(result)
    result
}
