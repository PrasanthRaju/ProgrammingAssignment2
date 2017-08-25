## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	invMatrix <- NULL
	set <- function(y){
		x <<- y
		invMatrix <<- NULL
	}
	getMatrix <- function() x
	setInverseMatrix <- function(inverseMatrix){
		invMatrix <<- inverseMatrix
	}
	getInverseMatrix <- function() invMatrix
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		invMatrix <- x$getInverseMatrix()
		if(!is.null(invMatrix))
		{
			message("Getting cached data")
			return (invMatrix)
		}
		dMatrix <- x$getMatrix()
		invMatrix <- solve(dMatrix)
		x$setInverseMatrix(invMatrix)
		invMatrix
}
