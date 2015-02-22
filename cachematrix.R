makeCacheMatrix <- function(mat = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                mat <<- y
                invMatrix <<- NULL
        }
        get <- function() mat
        setinverse <- function(inverse) invMatrix <<- inverse
        getinverse <- function() invMatrix
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix defined in makeCacheMatrix

cacheSolve <- function(mat, ...) {
        invMatrix <- mat$getinverse()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- mat$get()
        ## solve is a function the calculates the inversion of the matrix received
        invMatrix <- solve(data, ...)
        mat$setinverse(invMatrix)
        invMatrix
}
