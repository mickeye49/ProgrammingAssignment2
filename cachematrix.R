##
## Programming Assignment 2: Lexical Scoping for Johns Hopkins University
## R Programming course.
##
## See the README.md file for the assignment instructions.

## These functions implement creating, caching, and accessing the
## inverse of a matrix. makeCacheMatrix creates a special matrix and
## cacheSolve returns the inverse of the matrix.

## makeCacheMatrix returns a special matrix which is actually a
## list of functions:
##      1. set - set the value of the matrix 
##      2. get - get the value of the matrix
##      3. setinverse - set the matrix inverse
##      4. getinverse - get the matrix inverse
##
## The agrument to makeCacheMatrix is optional. You should set the
## matrix value using the set function if you do not provide an
## argument when calling makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        # initialize the matrix inverse
        i <- NULL

        # function to set matrix
        set <- function(y) {
                x <<- y         # set the matrix value - note that the
                                # value is in a different environment
                i <<- NULL      # initialize the inverse value
        }
        
        # function to get matrix
        get <- function() {
                x
        }
        
        # function to set inverse
        setinverse <- function(inverse) {
                i <<- inverse
        }
        
        # function to get inverse
        getinverse <- function() {
                i
        }
        
        # return a list of functions
        list (set = set,
              get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix. If the inverse
## has already been calculated it is returned from cache else it
## calculates the index, stores it in cache, and then returns the
## inverse.
##
## The argument to cacheSolve is the special matrix returned
## from makeCacheMatrix.
## The ... argument is passed to the solve function

cacheSolve <- function(sm, ...) {
        # get the inverse from cache
        i = sm$getinverse()

        # return the inverse if it has already been calculated
        if(!is.null(i)) {
                message("getting inverse from cache")
                return(i)
        }
        
        # calculate the inverse, store it in cache, and return it
        m = sm$get()
        i = solve(m, ...)  # addition arguments go here
        sm$setinverse(i)
        i
}
