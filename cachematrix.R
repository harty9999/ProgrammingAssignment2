# Creates a special "matrix" object that can cache its inverse.
# Arguments: (x) - Matrix to be used in set command
#
#   Returns: List containing matrix object used in set

makeCacheMatrix <- function(x = matrix()) {
	
	m <- NULL
	# Simple getters and setters
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	
	# Return list object
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
	
}


# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix function. If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
#
# Arguments: (x) - Matrix to be used for inversion
#
#   Returns: (m) - Inversed matrix object

cacheSolve <- function(x, ...) {
	
	m <- x$getinverse()
	# See if inverse object already exists; if so return
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	# If it doesn't recalculate it
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}

