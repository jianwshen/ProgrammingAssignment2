##  There are two functions:
##	1. makeCacheMatrix
##	2. cacheSolve
##  
##  Function makeCacheMatrix is to create a special matrix 
##	for the original matrix;
##  Function cacheSolve is to get the inverse matrix that is 
##  corresponding to the original matrix, the input is the 
##	special matrix created by makeCacheMatrix.


##	makeCacheMatrix function
## 	This function creates a special matrix object that can 
##	be used to cache the inverse matrix.
##  There are four methods
##	1. set
##	2. get
##	3. setMatrix
##	4. getMatrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(matrix) m <<- matrix
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}

##	cacheSolve function
##	This function computes the inverse matrix for a given matrix. 
##  The input matrix is from function makeCacheMatrix. 
##  If the inverse matrix does not exisit, this function will calculate 
##	the inverse and keep it in cache;
##  If the inverse has already been calculated, this function will 
##	fetch it from cache and return it;
##  The purpose of these two functions it to avoid duplicate calcuation 
##	of inverse matrix for a given matrix.
##	
##  Notes: if the inverse matrix is fetched from cache, it will print out 
##  a message as "inverse matrix is fetched from cache".
##
##  The inverse function for a matrix is using "solve(x, ...)" from library
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("inverse matrix is fetched from cache")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}
