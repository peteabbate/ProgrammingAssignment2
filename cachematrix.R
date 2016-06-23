## My function

## This function does 4 computations
## First, it stores the cached value as NULL
## Then it creates the matrix "y" in the working environment
## It gets the value of the matrix
## Uses solve to compute the inverse & store it in the cache
## And finally returns the functions to the working environment 

makeCacheMatrix <- function(x = matrix()) {
      
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## This tells R to get the inverse if it has already been calculated
## R displays the message "getting cached data" if it gets the inverse
## Otherwise, it calculates the inverse using solve(data, ...)
## then stores the inverse in the cache

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
