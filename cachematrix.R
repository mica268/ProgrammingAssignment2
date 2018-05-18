## Functions that are used to create a special object that stores a matrix and cache's its inverse

## This function creates a special "matrix" object that will be use in cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, 
         get = get,
         setInv = setInv, 
         getInv = getInv)
}


## This function calculates the mean of the special matrix with makeCacheMatrix but firts it checks to see if the means has already been calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv    
}


