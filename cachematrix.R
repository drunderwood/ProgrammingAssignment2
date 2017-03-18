## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly
## Your assignment is to write a pair of functions that cache the inverse of a matrix.
## Stub functions were used from assignment instructions.
## My assignment comments are retrieved from the makeVector() instructions provided by: 
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	
	## x is initialized as a matrix function argument
	## i is set to NULL, initializing it as an object
	i <- NULL
	
	## Define getters and setters for the matrix
	set <- function(y) {
		## Assign the input argument y to the x object in the parent environment
		x <<- y
		## Assign the value of NULL to the i object in the parent environment. 
		## This line of code clears any value of i that had been cached by a prior 
		## execution of cachesolve()
		i <<- NULL
	}
	## Since the symbol x is not defined within get(), R retrieves it from the parent environment of makeCacheMatrix()
	get <- function() x
	
	## Define the setter for the solve i
	setinverse <- function(solve) i <<- solve
	## Define the getter for the solve i
	getinverse <- function() i
	## Create a new object by returning a list()
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Ellipsis allows the caller to pass additional arguments into the function
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
	## Attempt to retrieve an inverse matrix from the object passed in as the argument
	i <- x$getinverse()
	## Check to see whether the result is not NULL and return the inverse
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	## If the result of !is.null(i) is FALSE, cachesolve() gets the matrix from the input object, 
	## calculates a solve(), uses the setinverse() function on the input object to set the inverse
	## in the input object, and then returns the value of the inverse to the parent environment 
	## by printing the inverse object.
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}

