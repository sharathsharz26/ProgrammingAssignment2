## makeCacheMatrix function creates a aspecial kind of matrix that can cache its inverse.
##cacheSolve function computes inverse of above matrix and retrieves inverse if already present. 

## Creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse 
	getinverse <- function() inv 
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Caches the inverse of above matrix if present 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
		if(!is.null(inv)) {
		message("getting cached matrix inverse")
		return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv)
		inv

}
