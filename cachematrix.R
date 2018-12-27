#!/usr/bin/Rscript

## This is ProgrammingAssignment2

## Create a special matrix that remembers its inverse

makeCacheMatrix <- function(x = matrix()) {
	#m stores the inverse of x
	m <- NULL

	#set new matrix
	set <- function(y){
		x <<- y 
		m <<- NULL
	}

	#get the current matrix
	get <- function(){
		x
	}

	#set new inverse
	setinv <- function(inv){
		m <<- inv
	}

	#get current inverse
	getinv <- function(){
		m
	}

	list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

	#get cached data
	m <- x$getinv()

	#if the inverse is already cached, return the cached inverse
	if( !is.null(m)){
		message("getting cached inverse")
		return(m)
	}

	#if the inverse is not cached, first get the current matrix
	data = x$get()

	#then calculate its inverse
	m <- solve(data,...)

	#and set the inverse
	x$setinv(m)

	#return the inverse
	m
}
