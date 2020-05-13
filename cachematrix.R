#A pair of functions that are used to create a special object that stores a matrix and cache's its inverse

#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {                         #function to set the value of the matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x                             #function to get the value of the matrix
    setinverse <- function(inverse) i <<- inverse   #function to set the value of the inverse matrix
    getinverse <- function() i                      #function to get the value of the inverse matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")              #inverse of the matrix is already available in the cache
        return(i)
    }
    data <- x$get()                                 #inverse of the matrix is unavailable in the cache 
    i <- solve(data, ...)                           #and is computed using the solve() function
    x$setinverse(i)
    i
}
