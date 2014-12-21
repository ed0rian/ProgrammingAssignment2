## Data structure to store and manipulate a given matrix as well as its inverse
##
##
## Creates the data structure and manipulates and/or returns its contents
## x                    stores the matrix
## inv                  stores the inverse of the matrix, initialized to NULL
## set(y)               replaces the previously stored matrix with the matrix y
##                      and sets the inverse to NULL
## get                  returns the matrix stored in local variable x
## setinverse(newInv)   sets the stored inverse to the value given by newInv
## getinverse()         returns the inverse stored in local variable inv
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(newInv) inv <<- newInv
    getinverse <- function() inv
    list( set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse
    )
}

## Checks whether the inverse of a matrix, stored in a makeCacheMatrix 
## data structure, is cached. 
## If not, it computes the inverse using the solve() function and stores it in
## the data structure using the setinverse() function.
## Returns the inverse matrix
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("retrieving inverted matrix from cache")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}