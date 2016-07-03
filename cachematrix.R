## assigns inverse matrix as NON-SQUARE matrix
## Returns a list of 4 functions
##     1st Fn will set the value of the matrix
##     2nd Fn will get the value of the matrix
##     3rd Fn will set the value of the inverse
##     4th Fn will get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
	invrs <- matrix(nrow=1,ncol=2)		## create NON-SQUARE matrix
	set <- function(y){
		x <<- y
		invrs <<- matrix(nrow=1,ncol=2)  ## create NON-SQUARE matrix
	}
	get <- function() x
	setinverse <- function(solve) invrs <<- solve
	getinverse <- function() invrs
	list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes a list created from makeCacheMatrix as input
## if cached inverse matrix is a SQUARE Matrix returns the inverse matrix
## if cached inverse matrix is NON-SQUARE then calculates and returns inverse matrix

cacheSolve <- function(x, ...) {
	invrs <- x$getinverse()
	if( nrow(invrs) == ncol(invrs) ) {
		message("getting cached data")
		return(invrs)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}