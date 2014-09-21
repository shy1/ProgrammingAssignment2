## a pair of functions that cache/return the inverse of a matrix

## makeCacheMatrix creates a list of 4 functions that set or get the value of
## a matrix as well as set or get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
	## sets the value of i to NULL when functions and values are first created
	i <- NULL
	
	## the set function assigns the value of the y argument to the x variable
	## and sets the value of i to NULL. the <<- assignment operator sets the
	## values of the x and i variables in environments outside the immediate
	## scope of the set function
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	## the get function returns the value of x
	get <- function() x
	
	## the setinv function sets the value of i to the value of the inv argument
	setinv <- function(inv) i <<- inv
	
	## the getinv function returns the value of i
	getinv <- function() i
	
	## creates and returns(as the last value of the makeCacheMatrix function)
	## a labeled list of the 4 functions (label name = function name)
	list(set = set, get = get, setinv = setinv, getinv = getinv)
	
}


## cacheSolve returns a matrix that is the inverse of the x argument

cacheSolve <- function(x, ...) {
	## sets variable i to the result of the getinv function
	i <- x$getinv()
	
	## if the value of i is not NULL return the cached inverse of the matrix
	if(!is.null(i)) {
		message("retrieving cached inverse..."
		return(i)
	}
	## if the value of i is NULL then calculate the inverse and cache it
	
	## sets variable mdata to the result of the get function
	mdata <- x$get()
	
	## sets variable i to the inverse of the mdata matrix using solve()
	## and calls the setinv function passing that inverse as the argument
	i <- solve(mdata, ...)
	x$setinv(i)
	
	## returns the value of i
	i
}
