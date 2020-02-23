## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a matrix with a chached inverse matrix
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
    
    # set the matrix insever
    setinv <- function(m_inv) x_inv <<- m_inv
    
    # get the matrix inverse
    getinv <- function() x_inv
    
    # return the four matrix functions in a list
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Write a short comment describing this function
## cacheSolve computes the inverse of a square, invertible matrix
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
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
