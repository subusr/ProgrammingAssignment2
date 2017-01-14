## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix Gets and sets the matrix value as well as inverses

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set = set, get = get,
	setinv = setinv,
	getinv = getinv)
}


## cacheSolve tries to retrive the inverse from the cache
## If value not found in cache then inverse is computed

cacheSolve <- function(x, ...) {
	m <- x$getinv()
        if(!is.null(m)) {
		message("getting cached data")
	        return(m)
	}

	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}
