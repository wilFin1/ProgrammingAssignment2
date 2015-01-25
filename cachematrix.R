##	This is the maxtrix structure which will provide the
##	inverse from the data structure	
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- solve(x)
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##	This is the maxtrix structure which will generate
##	an inverse and store both into the data structure
##	This needs to be adjusted to account for non-invertable
##	matrices.
##	Note:	The solve function will default to the inverse
##	if the second input parameter is missing.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
