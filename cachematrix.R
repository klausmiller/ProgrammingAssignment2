## This is the R code for Programming Assignment 2 within the Coursera course "R Programming"
## The two functions create a special matrix object that can cache its inverse and solve it from the cache. 

## Usage example
##
## > source('cachematrix.R')
## > x <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## > cacheSolve(x)
##
## Usage example output
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { #input x will be a matrix
  
  i <- NULL # i will be our inverse of x and set to NULL every time makeCacheMatrix is called
  
  set <- function(y) { # create a list
    x <<- y # containing the original matrix x
    i <<- NULL		# and the inverted matrix i
  }
  
  get <- function() x # this function returns the value of the original matrix
  
  setsolve <- function(solve) i <<- solve # this is called by cacheSolve() during the inital run
  
  getsolve <- function() i # this will return the cached value to cacheSolve() on subsequent runs
  
  list(set = set, get = get, # this is a list of the internal function ('methods') for a calling function 
       setsolve = setsolve,	# to know how to access these methods
       setsolve = setsolve, 
       getsolve = getsolve)
}


## The function cacheSolve computes the inverse i of the special "matrix" x created by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) { # the input matrix x is an object created by makeCacheMatrix
  
        i <- x$getsolve() # accesses the object 'x' and stores the inverse of matrix x in the variable i
        
        if(!is.null(i)) { # if the inverse matrix i was already cached (not NULL) ...
          message("getting cached data") # ... send this message to the console
          return(i) # ... and return the inverse matrix i
        }
        
        data <- x$get() # we reach this code only if x$getsolve() returned NULL
        
        i <- solve(data, ...) # if i was NULL the we have to calculate the inverse matrix
        
        x$setsolve(i) # store the calculated inverse matrix value in x (see setsolve() in makeCacheMatrix
        
        i # return the inverse matrix to the console
}
