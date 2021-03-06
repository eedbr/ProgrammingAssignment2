## In this Programming Assignment will take advantage of the scoping rules of the R 
## language to create two functions that, together, should cache the inverse 
##  of a matrix to avoid recalculating the materix if the inverse is already cached

## the function makeCacheMatrix creates a matrix object that can cahche its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve ## set the inverse matrix if not cached
        getmatrix <- function() m ## get the inverse matrix if cached
        list(set = set, get = get, 
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## the function cacheSolve should compute the inverse of a matrix stored with makeCacheMatrix
## if the inverse has already been caclulated then cacheSolve should retrieve the stored inverse

cacheSolve <- function(x = matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached matrix data") ## get the inverse matrix if cached
                return(m)
        }
        data <- x$get() 
        m <- solve(data) ## calculate inversee
        x$setmatrix(m) ## set inverse matrix if not cached
        m
}
