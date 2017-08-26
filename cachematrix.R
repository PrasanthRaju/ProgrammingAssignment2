## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list of functions that are used to set and return matrix, cache the inverse matrix and return it

makeCacheMatrix <- function(x = matrix()) {
	invMatrix <- NULL
	## Set matrix and it's inverse to initial values
	set <- function(y){
		x <<- y
		invMatrix <<- NULL
	}
	
	## Return matrix
	getMatrix <- function() x
	
	## Cache Inverse of Matrix with <<- operation
	setInverseMatrix <- function(inverseMatrix){
		invMatrix <<- inverseMatrix
	}
	
	##Return inverse of the matrix
	getInverseMatrix <- function() invMatrix
	
	##Return a 1-by-4 matrix (which is really a list) of functions defined
	list(set = set, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
## This function checks if the inverse of a matrix is already cached. If cached, it returns the cached result. If not, it calculates and returns  inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		## Get the inverse matrix
		invMatrix <- x$getInverseMatrix()
		matricks <- x$getMatrix()
		
		## If inverse matrix is already cached AND matrix has not changed, then return cached version of the inverse matrix
		if(!is.null(invMatrix))
		{
			message("Getting cached data")
			return (invMatrix)
		}
		
		#If not cached, then get the matrix
		dMatrix <- x$getMatrix()
		
		## Calculate the inverse of the matrix
		invMatrix <- solve(dMatrix)
		
		## Cache the inverse matrix
		x$setInverseMatrix(invMatrix)
		
		## Return inverse matrix
		invMatrix
}
