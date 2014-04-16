## These functions will create a special "Matrix" object that stores a Matrix
## and caches it's inverse

## Function to create a special "Matrix" object. 
## The function accepts a standard Matrix as it's input and allows setting the Matrix's 
## inverse as part of the object so that subsequent calculations of the inverse will be 
## returned from cache and save the compute time needed to generate the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function(){
                x
        } 
        setInverse <- function(inverse){
                inv <<- inverse      
        } 
        getInverse <- function(){
                inv
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## A function to calculate and set the inverse of a Matrix.
## this function accepts a "CacheMatrix" object and returns it's inverse.
## On the first time this function runs on an inputted CachMatrix, it will calculate 
## the inverse using the standard solve() function and will store the inverse as a variable
## in the CacheMatrix object. 
## On subsequent runs of the function on the same CachedMatrix the inverse will be fetched from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        # if the inverse is already set for x return from cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # Otherwise, calculate the inverse and store in x
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
