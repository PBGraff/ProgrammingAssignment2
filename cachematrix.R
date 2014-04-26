## These functions create an object for a cached matrix inverse.
## They create an object that stores the matrix and its inverse
## and contain a function to solve for the inverse, recalling
## the cached one if already computed for the input matrix.

## Creates the matrix inverse cache object. Stores the matrix
## and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inverse to NULL
        y <- NULL
        ## sets the matrix and initializes the inverse to NULL
        set <- function(z) {
                x <<- z
                y <<- NULL
        }
        ## returns the original matrix
        get <- function() x
        ## saves the inverse
        setInverse <- function(z) y <<- z
        ## returns the saved inverse, whether NULL or solution
        getInverse <- function() y
        ## outputs the list object with these attributes
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Solves for the inverse of a matrix.
## Uses cached solution if already computed for input matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        y <- x$getInverse()
        ## Check if inverse returned is NULL or solution
        if ( !is.null(y) ) {
                ## Inverse already computed: return from cache
                message("Retrieving cached inverse")
                return(y)
        } else {
                ## Need to compute inverse: get matrix and find inverse,
                ## store and return solution
                m <- x$get()
                y <- solve(m)
                x$setInverse(y)
                return(y)
        }
}
