## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## B is used to store the inverse of X. When makeCacheMatrix(y) is called, x will be assigned to be y, and set B to be NULL.
makeCacheMatrix <- function(x = matrix()) {
	B<-NULL
	set<-function(y){
		x <<- y
		B <<-NULL
	}
	get <-function() x
	setSolve <-function(solves) B<<- solves
	getSolve <-function() B
	list(set=set , get = get,
	setSolve = setSolve,
	getSolve = getSolve)
	
}


## Write a short comment describing this function
## cacheSolve(x) will read x$getSolve() first. If it is NULL, then compute the inverse of x, and store it to x$B. 
## If it is not NULL, which means that x's inverse has been computed, then read x's inverse from cache directly.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    B<-x$getSolve()
	if(!is.null(B)){
		message("getting cached data")
		return(B)
		
	}
	data<-x$get()
	B <-solve(data,...)
	x$setSolve(B)
	B
}
