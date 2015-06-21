## This a pair of functions cache the inverse of a matrix.

## Function "makeCacheMatrix" creates a special"Matrix"object that can cache 
## its inverse.

makeCacheMatrix <- function( m = matrix() ) {
        i <- NULL
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        get <- function() {
                m
        }
        setInverse <- function(inverse) {
                i <<- inverse
        }
        getInverse <- function() {
                i
        }
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function "cacheSolve" computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## and the matrix has not changed, then cacheSolve should rerieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setInverse(m)
        m
}
