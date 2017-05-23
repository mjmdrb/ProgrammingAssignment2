## Function makeCacheMatrix creates a matrix object that can cache its inverse

## Function cacheSolve finds the inverse of matrix returned by makeCacheMatrix
## if necessary.

## i = NA sets empty matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NA
	set <- function(z) {
		x <<- z
		i <<- NA

	}
	get <- function() x
	setinverse <- function(solve) i <<- solve 
	getinverse <- function() i
	list(set = set, get = get, 
             setinverse = setinverse, 
	     getinverse = getinverse)
}
	

## This function will find the inverse of matrix x and return
## if x was unchanged, it will simply return the cached inverse

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
	if(!is.na(i)) {				 ##no need to find inverse, use cache
		message("getting cached matrix")
		return(i)
	}
	data <- x$get()				##solve inverse since i = NA
	i <- solve(data)
	x$setinverse(i)
	i
}
