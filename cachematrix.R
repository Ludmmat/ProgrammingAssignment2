## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

# cacheSolve function returns the inverse of the matrix. However, it first checks 
# to see if the inverse has already been calculated. 
# If so, it gets the cache result and skips the computation. 
# Otherwise, it calculates the inverse and sets the value in the cache via
# setinverse function.
# Furthermore, this function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}

##Sample run:
# x <- cbind(c(1,2),c(-2,-3))
# m <- makeCacheMatrix(x)
# m$get()
##  No saved (cache) data in the first round
# cacheSolve(m)
##  Getting cached data in the second round
# cacheSolve(m)
