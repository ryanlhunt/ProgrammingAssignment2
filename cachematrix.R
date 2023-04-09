## Caching the Inverse of a Matrix
## Lexical Scoping

## Creates a matrix that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
    # init
    m <- NULL
    
    # Set matrix
    set <- function(y) {
          x <<- y
          m <<- NULL
    }
    
    # Get matrix
    get <- function() x
    
    # Set inverse
    set.inverse <- function(setinv) m <<- setinv
    
    # Get inverse
    get.inverse <- function() m
    
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(wallet, ...) {
    #gets the stored inverse
    inv <- wallet$get.inverse()
    # If it's there it returns it
    if(!is.null(m)) {
          message("looking for cache in wallet")
          return(inv)
    }
    # It not stored, it calculates
    data <- wallet$get()
    m <- solve(data, ...)
    wallet$set.inverse(m)
    
    
        ## Return a matrix that is the inverse of 'x'
    m
}

