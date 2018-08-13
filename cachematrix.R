## The first function, makeCacheMatrix creates a spetial matrix to:
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse of the matrix
##4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
    x <<- y
    i <<- NULL
    }
    get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the matrix that is created with the makeCacheMatrix function if the inverse has not been already calculated. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
  	if(!is.null(i)) {
    message("getting cached data")
    return(i)
    }
    data <- x$get()
 	i <- solve(data, ...)
 	x$setinverse(i)
  i
}
