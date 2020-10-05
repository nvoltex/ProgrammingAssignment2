## The goal of this script is to create the tools to calculate the inverse of a 
## matrix and cache its value

# Creates an object meant to hold a matrix and store its inverse
#  
# @param   x: invertible matrix to be stored
# @return  list containing a getter and setter for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Calculates or returns the cached inverse of the Matrix stored in its argument
# When calculating the inverse it also stores it in the object passed as argument
#  
# @param   x: object returned by "makeCacheMatrix"
# @return  Inverse of the Matrix stored in x

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached value for the inverse")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix)
    x$setinverse(i)
    i
}