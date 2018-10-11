## Caching the inverse of a vector

## makeCacheMatrix creates a vector which is really a list which sets and gets the value of a vector including seting and getting
##the value of the invers

makeCacheMatrix <- function(x = matrix()) {
	s<-NULL
	set<-function(y){
		x<<-y
		s<<-NULL
	}
	get<-function()x
	setsolve<-function(solve) s<<-solve
	getsolve<-function()s
	list(set=set,get=get,
		setsolve=setsolve,
		getsolve=getsolve)

}


## The next function clculates teh inverse of the vector created in the function above after checking to see if the inverse
## has already been calculated. 

cacheSolve <- function(x, ...) {
	s<-x$getsolve()
	if(!is.null(s)){
		message("getting cached data")
		return(s)
	}
	data<-x$get()
	s<-solve(data,...)
	x$setsolve(s)
	s
}


