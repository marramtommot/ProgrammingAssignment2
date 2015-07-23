## Create an object that caches a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## assign NULL as default value for the inverse matrix
    s <- NULL
    
    ## set the matrix and reset its inverse
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## retrieve the matrix
    get <- function() x
    
    ## cache the inverse matrix
    setsolve <- function(solve) s <<- solve
    
    ## retrieve the cached inverse matrix
    getsolve <- function() s
    
    ## return a list of the functions we want
    ## to be accessible from this environment
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x';
## if the matrix has already been calculated
## return its cached value

cacheSolve <- function(x, ...) {
    ## retrieve the cached inverse matrix
    s <- x$getsolve()
    
    ## if the matrix has already been calculated 
    if(!is.null(s)) {
        ## return the cached matrix
        return(s)
    }
    
    ## retrieve the original matrix
    data <- x$get()
    
    ## calculate the inverse matrix
    s <- solve(data, ...)
    
    ## cache the inverse matrix
    x$setsolve(s)
    
    ## return the inverse matrix
    s
}
