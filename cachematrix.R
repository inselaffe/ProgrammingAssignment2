##	Calculate and cache the inverse of a matrix on the first occasion
##	that the inverse is requested
##	On subsequent occasions of the inverse being requested, return the 
##	cached value

##	If inverse of matrix supplied as an argument does not exist, it is 
##	calculated and stored (as 's')
##	Returns the inverse as calculated or retrieved from cache	
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y)  {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setinv <- function(inv) s <<- inv
	getinv <- function() s
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv
	)
}

##	Attempts to retrieve cached value of inverse of a matrix that has been 
##	stored using 'makeCacheMatrix'
##	If cached value is not available, uses 'solve' to calculate the inverse 
##	of the matrix
##	Stores the calculated inverse in 'makeCacheMatrix'
##	Returns the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s <- x$getinv()
	if (!is.null(s))  {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setinv(s)
	s
}
