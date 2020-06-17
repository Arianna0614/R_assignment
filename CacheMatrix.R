## Put comments here that give an overall description of what your
## functions do
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
	inverse_ <- NULL 
	set <- function(y){
		x <<- y
		inverse_ <<- NULL
		
	}
	get <- function()x
	setinverse <- function(inverse) inverse_ <<- inverse 
	getinverse <- function() inverse_
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


#The following function calculates the mean of the special "vector" created with the above function. #However, it first checks to see if the mean has already been calculated. If so, it gets the mean from #the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the #value of the mean in the cache via the setmean function.



cacheSolve <- function(x, ...) {
	
	## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}
























