## ProgrammingAssignment2: Calculating the inverse of a large matrix can be time-consuming. When a matrix is
## not changed, the inverse remains the same. Create a function that enables the caching of the
## inverse of a matrix (makeCacheMatrix), and then create a function that retrieves the cached inverse when
## it is available or calculates the inverse when it is not (cacheSolve). Assume the matrix is invertible. Ensure that
## when the makeCacheMatrix is called, the inverse is reset. 
## CREDITS: Many thanks to the explanations posted by Gregory Horne, Bill Hilton, and Rock Pereira in the discussion threads...

## makeCacheMatrix : Creates a list object that contains the given matrix and associated functions
##     x : Given input is a matrix

makeCacheMatrix <- function(x = matrix()) {
  	inverseMatrix <- NULL					# Always initialize the inverseMatrix to NULL

  	set <- function(y) {					# Function to set the saved matrix to be y, reset inverse
  		x <<- y
  		inverseMatrix <<- NULL
	}

	get <- function() { x }					# Function to return the currently saved matrix

	setInverse <- function(solve) { 		# Function to set the inverseMatrix
		inverseMatrix <<- solve 
	}	

	getInverse <- function() { 				# Function to return the inverseMatrix
		inverseMatrix 
	}				

	list(set=set, get=get,					# Matrix created, List object exposing functions 
   		setInverse =setInverse,
   		getInverse=getInverse)

}


## cacheSolve : Returns the inverse of a matrix, assuming it is invertible. Uses the cached inverse if it has
## already been calculated and the matrix has not changed.
##     matrixList : Given input is a list object created by makeCacheMatrix

cacheSolve <- function(matrixList , ...) {
        inverseMatrix <- matrixList$getInverse() 	# Retrieve the inverseMatrix of matrixList 

    	if (!is.null(inverseMatrix )) {				# If matrixList already has an inverse, then return it
      	message("Using cached data")
      	return(inverseMatrix )
    	}

	# Otherwise, matrixList doesn't have an inverse, so we must find it
    	matrix <- matrixList$get()					# Retrieve the currently saved matrix in matrixList 	
    	inverseMatrix <- solve(matrix, ...)			# Calculate the inverse of the matrix 
    	matrixList$setInverse(inverseMatrix )		# Save the inverse in matrixList 
    	inverseMatrix 								# Return the inverse

}
