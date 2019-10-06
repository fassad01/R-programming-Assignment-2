## a programming assignment to demonstrate concept of closure and
## lexical scoping in R.

## function makeCacheMatrix returns four pointers to the parent 
## environment.  Pointers are returned as a list.
## 1. set: creates pointers to input matrix and its inverse 
## 2. get: fetches input matrix
## 3. setInverse: creates pointer to inverse matrix
## 4. getInverse: fetches the inverse of matrix that has been cached

makeCacheMatrix <- function(x = matrix()) {
    answer <- NULL
    set_m <- function(y) {
        x <<- y
        answer <<- NULL
    }
    get_m <- function() x
    set_mInverse <- function(inverse) answer <<- inverse
    get_mInverse <- function() answer
    list(set_m = set_m, get_m = get_m,
         set_mInverse = set_mInverse,
         get_mInverse = get_mInverse)
}
 

## function cacheSolve uses objects created by makeCacheMatrix 
## to invert a matrix or if inversion has been done already, it
## retrieves cached data.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    result <- x$get_mInverse()
    if (!is.null(result)) {
        message("getting cached data")
        return(result)
    }
    mat <- x$get_m()
    result <- solve(mat, ...)
    x$set_mInverse(result)
    result
}

