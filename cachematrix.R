## Returning the inverse matrix of a given invertible square matrix
## using an "if not yet cached then compute and cache else return cached value"
## approach:
#
# Below are two functions that are used to create a special object that 
# stores a matrix and caches its inverse matrix. 
# Please be aware that even though  the first function "makeCacheMatrix" accepts
# any kind of matrix not necessarily square matrices the second function
# "cacheSolve" will only succeed with invertible square matrices.
# So please use only invertible square matrices for testing/validating, 
# as e.g. matrices produced by matrix(c(2, 1, 5, 3), 2, 2) or by matrix(1:4, 2, 2).
#
# A typical test scenario would be:
#   > m1 <- matrix(c(2, 1, 5, 3), 2, 2)
#   > cm1 <- makeCacheMatrix(m1)
#   > cacheSolve(cm1)
#   > cacheSolve(cm1)
##############################################################################

## Function "makeCacheMatrix":
# Given a matrix return a special "matrix object" internally represented
# by a list with its elements being functions
# - to set a given matrix into the variable x of the "matrix object",
# - to get the orginal matrix data being stored within variable x of the "matrix
#   object",
# - to set a given inversematrix into the variable im of the "matrix object"
#   (this it the "fill the result cache" functionality)
# - to get the value of variable im of the "matrix object" (this is the "get
#   the cache value" funtionality). Note that NULL will be returned if the
#   cache was not yet set.
# Variable x of the "matrix object" is intended to hold the original "input" matrix.
# Variable im of the "matrix object" is intended to hold/cache the inverse
# matrix of x (i.e. the result).
makeCacheMatrix <- function(x = matrix()) {
        # im: variable of the special "matrix object" to store/cache an inverse
        #     matrix into. Here: its initialization.
        im <- NULL
        
        # set the value of the given "input" matrix into variable x
        # and do (re)initialize the im variable: 
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        
        # get the "input" matrix:
        get <- function() x
        
        # set the value of the inverse matrix (the cached "result"):
        setinverse <- function(inversematrix) im <<- inversematrix
        
        # get the value of the "inverse matrix"
        getinverse <- function() im
        
        # return the special matrix object (represented by a list object):
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function "cacheSolve":
# This function calculates the inverse matrix of a special "matrix object" 
# created with the "makeCacheMatrix" function. 
# However, it first checks to see if the inverse matrix has already been calculated. 
# If so, it gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse matrix of the data (= the original matrix) 
# and sets the value of the inverse matrix into the cache via the setinverse 
# function.        
cacheSolve <- function(x, ...) {
        # For given special matrix object x (represented as a list) use
        # its getinverse function to return its own "object variable" (im) 
        # that might already contain the inverse matrix:
        invm <- x$getinverse()
        
        # If this retrieved matrix does really exist (i.e., not null) then return
        # it with the textual information that it was retrieved from cache.
        if(!is.null(invm)) {
                message("getting cached inverse matrix")
                return(invm)
        }
        
        # Reaching this code place it is clear that the inverse matrix was not
        # yet cached! So use the "get" function of the special matrix object
        # to retrive the original matrix. Then use the R-built-in solve-function
        # to compute the inverse of the original matrix and finally set
        # this inverse matrix into (the local variable im of) the special matrix
        # object x. Return the inverse matrix.
        data <- x$get()
        invm <- solve(data, ...)
        x$setinverse(invm)
        invm
}
