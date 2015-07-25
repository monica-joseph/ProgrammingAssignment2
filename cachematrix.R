## THis function has been coded for Asst 3
## It helps cache a long running task of taking 
## an inverse of a matrix

## makeCacheMatrix is used to create a vector of functions
## that will get or set a cache value
## or get the inverse or set the inverse
## the point to note is <<- operator
## this saves data in another environment
## Here the concept of lexical scoping in R helps.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) inverse <<- solve
	getInverse <- function() inverse
	list(set = set, get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)
}


## cachesolve will take the specialvector created by 
## makeCacheMatrix as input and use it to get the inverse
## if the inverse calculation is not over then data in
## cache is retrieved

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
	inverse
}
