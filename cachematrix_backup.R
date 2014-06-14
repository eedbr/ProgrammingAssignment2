## The two functions should either store a matrix 
## functions do

## the function makeCacheMatrix should create a matrix object that can cahche its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, 
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## the function cacheSolve should compute the inverse of a matrix stored with makeCacheMatrix
## if the inverse has already been caclulated then cacheSolve should retrieve the stored inverse

cacheSolve <- function(x = matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached matrix data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setmatrix(m)
        m
}
