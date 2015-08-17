## The `makeCacheMatrix` is a matrix factory function the creates matrices
## capable of caching their inverse.
## The `cacheSolve` function takes an invertible matrix created using the
## aforementioned matrix factory function and returns its inverse created with
## the same matrix factory function.


## The `makeCacheMatrix` function creates a special matrix object from a given
## one capable of caching its inverse. The returned object offers getters and
## setters for both the matrix and its inverse.

makeCacheMatrix <- function(m=matrix()) {
    mat <- m
    inv_mat <- NULL
    set <- function(m) {
        mat <<- m
        inv_mat <<- NULL
    }
    get <- function() {
        mat
    }
    setInverse <- function(inv) {
        inv_mat <<- inv
    }
    getInverse <- function() {
        inv_mat
    }
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## The `cacheSolve` function takes an invertible matrix created using the
## aforementioned `makeCacheMatrix` function and returns its inverse created
## with the same matrix factory function. The `cacheSolve` function
## additionally takes other optional arguments that are passed to the inner
## call to the `solve` function used to compute the inverse.

cacheSolve <- function(x, ...) {
    inv_mat <- x$getInverse()
    mat <- x$get()
    if(!is.null(inv_mat)) {
        message("getting cached data")
    } else {
        inv_mat <- solve(mat, ...)
        x$setInverse(inv_mat)
    }
    inv_mat <- makeCacheMatrix(inv_mat)
    inv_mat$setInverse(mat)
    inv_mat
}
