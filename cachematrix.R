## These functions implement creating, caching, and accessing the
## inverse of a matrix. makeCacheMatrix creates a special matrix and
## cacheSolve returns the inverse of the matrix.

## makeCacheMatrix returns a list of functions (special matrix) to
## 1. setmatrix - set the value of the matrix 
## 2. getmatrix - get the value of the matrix
## 3. setinverse - set the matrix inverse
## 4. getinverse - get the matrix inverse
##
## The agrument to makeCacheMatrix is optional. You should set the
## matrix value using the set function if you do not provide 
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
## calcultes the index, stores it in cache, and then returns the
## inverse.
##
## The argument to ccheSolve is the list (special matrix) returned
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
