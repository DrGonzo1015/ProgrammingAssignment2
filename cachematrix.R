## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        imat <- NULL
        set <- function(y) {
                x <<- y
                imat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) imat <<- inverse
        getinverse <- function() imat
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        imat <- x$getinverse()
        if (!is.null(imat)) {
                message("getting cached inverse matrix")
                return(imat)
        }
        mat <- x$get()
        imat <- solve(mat, ...)
        x$setinverse(imat)
        imat
}