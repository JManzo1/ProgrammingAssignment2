## makeCacheMatrix and cacheSolve are used on cunjunction to create a matrix
## where the inverse of the matrix can be stored and the  inverse of the does
## not need to be calculated every time it is needed

## makeCacheMatrix creates a matrix with a cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {

    # set inverse matrix to NULL when creating
    x_inv <- NULL
    
    # replace the matrix with the new matrix and set the inverse to NULL
    set <- function(y) {
        
        x <<- y
        x_inv <<- NULL
        
    }
    
    # get the matrix
    get <- function() x
    
    # set the matrix inverse
    setinv <- function(m_inv) x_inv <<- m_inv
    
    # get the matrix inverse
    getinv <- function() x_inv
    
    # return the four matrix functions in a list
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}

## cacheSolve computes the inverse of a square, invertible matrix
## and sets it so that it can be reused without recalculating it
cacheSolve <- function(x, ...) {
    
    # get the store inverse
    x_inv <- x$getinv()
    
    # if not null, return the stored inverse
    if(!is.null(x_inv)) {
        
        message("Getting cached data")
        return(x_inv)
        
    }
    
    # if not already calculated, get the matrix, calculate the inverse,
    # set (cache) the inverse, and return the inverse
    x_mat <- x$get()
    x_inv <- solve(x_mat, ...)
    x$setinv(x_inv)
    x_inv
    
}
