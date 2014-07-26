#######################################################################################################################
#######################################################################################################################
##                                                                                                                   ##
## The following functions calculate the inverse matrix of a given matrix and store it for later uses.               ##
##                                                                                                                   ##
#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
#######################################################################################################################
##                                                                                                                   ##
## makeCacheMatrix(matrix)                                                                                           ##
##                                                                                                                   ## 
#######################################################################################################################
##                                                                                                                   ##
## This function has a input parameter "x" (matrix) and make a new object integrated by these elements:              ##
## - Original Matrix "x".                                                                                            ##
## - Inverse Matrix "i".                                                                                             ##
## - Function "set(matrix)", Initialize original and inverse matrix.                                                 ##
## - Function "setinverse(matrix)", initialize inverse matrix with "matrix" parameter.                               ##  
## - Function "getinverse()", return the inverse matrix "i".                                                         ##
## - Function "get()", return the original matrix "x".                                                               ##
##                                                                                                                   ##
#######################################################################################################################
#######################################################################################################################

makeCacheMatrix <- function(x = matrix()) {

     ## Initialize  "i" 
     i <- NULL
     
     ## Definition of function set(matrix)
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     
     ## Definition of function get()
     get <- function() x
     
     ## Definition of function setInverse(matrix)
     setinverse <- function(inverse) i <<- inverse
     
     ## Definition of function getInverse()
     getinverse <- function() i
     
     
     ## Return a array with the functions that let us work with the new object
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#######################################################################################################################
#######################################################################################################################
##                                                                                                                   ##
## cacheSolve(makeCacheMatrix)                                                                                       ##
##                                                                                                                   ## 
#######################################################################################################################
##                                                                                                                   ##
## This function return the inverse matrix of its input  "x" (matrix), if exist, or is calculated for later use.     ##
##                                                                                                                   ##
#######################################################################################################################
#######################################################################################################################

cacheSolve <- function(x, ...) {
     
     ## The inverse matrix is obtained
     i <- x$getinverse()
     
     ## It determines whether the inverse matrix calculated above
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     
     ## Obtain the matrix for calculate its inverse
     data <- x$get()
     i <- solve(data, ...)
     
     ## The inverse matrix is stored in the cache
     x$setinverse(i)
     
     ## Return the inverse matrix
     i
}