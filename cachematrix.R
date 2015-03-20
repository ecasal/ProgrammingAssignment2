## This first function creates a special matrix.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
  	set<-function(y){	## Sets the matrix value
  		x<<-y
  		m<<-NULL
	}
	get<-function() x	## Gets the matrix value		
	setmatrixinv <-function(solve) m<<- solve	## Sets the matrix inverse
	getmatrixinv <-function() m		## Gets the matrix inverse
	list(set=set,get=get,setmatrixinv=setmatrixinv,getmatrixinv=getmatrixinv)
}


## This second function calculates the inverse, but first, it checks if it has
## calculated before; so, it gets the matrix from cache.

cacheSolve <- function(x, ...) {
	m<-x$getmatrixinv()
    	if(!is.null(m)){
      		message("getting cached data")
      		return(m)
   	}
    	matrix<-x$get()
    	m<-solve(matrix, ...)
    	x$setmatrixinv(m)
    	m
        ## Return a matrix that is the inverse of 'x'
}
