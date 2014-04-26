## These functions create an object for a cached matrix inverse.
## They create an object that stores the matrix and its inverse
## and contain a function to solve for the inverse, recalling
## the cached one if already computed for the input matrix.

## Creates the matrix inverse cache object. Stores the matrix
## and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        y <- NULL
        set <- function(z) {
                x <<- z
                y <<- NULL
        }
        get <- function() x
        setInverse <- function(z) y <<- z
        getInverse <- function() y
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Solves for the inverse of a matrix.
## Uses cached solution if already computed for input matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        y <- x$getInverse()
        if ( !is.null(y) ) {
                # Inverse already computed: return from cache
                message("Retrieving cached inverse")
                return(y)
        } else {
                # Need to compute inverse: get matrix and find inverse,
                # store and return solution
                m <- x$get()
                y <- solve(m)
                x$setInverse(y)
                return(y)
        }
}
