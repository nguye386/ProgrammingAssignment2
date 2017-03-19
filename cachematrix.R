#(similar to the Caching the Mean of a Vector example)'makecacheMatrix' creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix<-function(x=matrix()){
	m=NULL
	set<-function(y){					# set values of the matrix
		x<<-y 							# assign a value to an object in an environment that 										# is different from the current enviroment
		m<-NULL
	}
	get<-function() x					# get the values of the matrix
	setinverse<-function(inverse) m<<-inverse	#set the inverse of the matrix
	getinverse<-function() m					# get the inverse of the matrix
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#(similar to the Caching the Mean of a Vector example). 'cacheSolve' computes the inverse of the matrix. If the inverse matrix has already been calculated, it gets the inverse from the cache and skips computation. Otherwise it calculates the inverse and sets the matrix in the cache via setinverse function.

cacheSolve<-function(x,...){
	m<-x$getinverse()					# output of makeCacheMatrix
	if(!is.null(m)){					# checks if inverse has already been calculated
		message("getting cached data")	# if so - it skips the calculation and get the 												# inverse from the cache.
		return(m)
	}									
	data<-x$get()						# calculates the inverse
	m<-solve(data, ...)
	x$setinverse(m)						# sets the value of the inverse in the cache
	m
}