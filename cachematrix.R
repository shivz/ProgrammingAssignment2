## makeCacheMatrix and cacheSolve together can be used to get the 
## inverse of a matrix. Using these function has the 
## advantage that the inverse of a patricular matrix is calculated 
## just once and cached, and every subsequent call to cacheSovle will just
## return the cached inverse rather than calculating it each time.

## makeCachematrix
## Creates a special matrix that can cache it's inverse.
## Arguments: 
## x    an invertible matrix
## Return:
## list of four generic functions mentioned below: 
## set          : caches the matrix
## get          : gets the cached matrix
## setinverse   : calculate and cache the inverse
## getinverse   : return the cached inverse
makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(mat) {
        m <<- mat
        inv <<- NULL
    }
    
    get <- function() m
    setinverse <- function(inv) inv <<- inv
    getinverse <- function() inv
    
    # return the list of functions created
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.
## Arguments:
## x    the special matrix returned from makeCacheMatrix
## ...  further arguments that can be passed to the solve method
## Return:
## inverse of the matrix
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        # inverse of the matrix was already calculated
        # print("Returning the cached inverse")
        return(inv)
    }

    m <- x$get()
    inv <- solve(m, ...)
    x$setinverse(inv)
    inv
    
}
