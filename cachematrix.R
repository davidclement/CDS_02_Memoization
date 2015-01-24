## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	I <- NULL
	set <- function(y) {
		x <<- y
		I <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) I <<- inverse
	getInverse <- function() I
	list(set = set, get = get, 
		setInverse = setInverse,
		getInverse = getInverse )
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCache
#    Matrix above. If the inverse has already been calculated (and the matrix has not changed), 
#    then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getInverse()
        if(!is.null(I)) {
        	message("getting cached data")
        	return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}
