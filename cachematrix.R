## Put comments here that give an overall description of what your functions do

## This function creates a matrix, or rather a list containing a function to
# set the value of the matrix via set()
# get the value of the matrix via get()
# set the value of the matrix inverse via setInverse()
# get the value of the matrix inverse via getInverse()
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        # The above means that each call of set() will remove the cache! That is 
        # why it is not necessary to check, wheather the matrix has changed...
        # If the matrix was changed using set(y), then the inverse will be
        # calculated again. 
        # One could add a check wheather y == x to avoid unnecessarily removing 
        # the cache. 
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The following function calculates the inverse of the special "matrix" 
# created with the above function. However, it first checks to see if the 
# inverse has already been calculated. If so, it gets the inverse from the 
# cache and skips the computation. Otherwise, it calculates the inverse of the 
# data and sets the value of the inverse in the cache via the setInverse 
# function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) # Here, the inverse is actually calculated. 
    x$setInverse(inv)
    inv
}
