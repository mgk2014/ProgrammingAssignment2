## cachematrix.R
## author: mgk2014
## date created - 21-Jul-2014
## coursera - r-programming, programming assignment 2
## 
## Test() function - this script includes a test function that runs a test code to validate the functions
##

## makeCacheMatrix(x = matrix())
#   This function takes a matrix parameter and creates an instance of this function
#   The parameter matrix x, is stored as a private variable inside the function
#   get() - gets the matrix used instantiance this function
#   set() - allows the orginal matrix to be overridden. This code will then set the cached inverse to NULL
#   setinverse() - allows the inverse to be re-written. this function is used by the cacheSolve ()
#   getinverse(0 - can be queries to return the cached inverted matrix
#
#   Assumption - this function assumes that the matrix used to instantiate the function is invertible
#
makeCacheMatrix <- function(x = matrix()) {

    # set the inverted matrix to NULL
    invMatrix <- NULL
    
    # set() - allows a new matrix to be set into the function. 
    #   this call will invalidate the computed cached inverse matrix
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
    # get() - simply returns the original matrix
    get <- function() x
    
    # setinverse() allows an inverted matrix to be set in this function
    setinverse <- function(inverse) invMatrix <<- inverse
    
    # getinverse() simply return the inverted matrix. It would return null, if 
    #   an inverted matrix has not been set
    getinverse <- function() invMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## cacheSolve(x = makeCacheMatrix())
#   This function takes an instance of function makeCacheMatrix, checks if the inverse have been computed
#   If the inverse exists, then it simply returns the cached inverse matrix
#   If not, this function computes the inverse and stores it into the makeCacheMatrix() instance using the setinverse() function
#
#   Assumption - this function assumes that the matrix contained within makeCacheMatrix() is invertible
# 
cacheSolve <- function(x, ...) {
    
    # get the inverted matrix from the passed in makeCacheMatrix() instance
    invMatrix <- x$getinverse()
    
    # if inverted matrix is not null (i.e. already computed), return 
    #       inverted matrix. Otherwise the code will proceed to compute the inverted matrix
    #       This acts as the cache 
    if (!is.null(invMatrix)){
        message("getting cached data")
        return(invMatrix)
    }
    
    # get the original matrix
    data <- x$get()
    
    # use solve function to compute the inverse
    invMatrix <- solve(data)
    
    # set the inverst back into the makeCacheMatrix() instance
    x$setinverse(invMatrix)
    
    # return the inverted matrix
    invMatrix
}


## test()
#   This function verifies the functions and serves as a sample code 
#   to test these functions
# 
test <- function() {

    # create an instance of the function
    x <- makeCacheMatrix(matrix(sample(1000,16), 4,4)) 
    
    # check the newly created matrix
    y <- x$get()                                   
    print("new matrix")
    print(y)
    
    # the inverse should be empty at this point
    z <- x$getinverse()
    print(paste("inverse matrix", z))
          
    # cache the inverse
    cacheSolve(x)                                   
    z <- x$getinverse() 
    print("inverse matrix after caching")
    print(z)
          
    # check that the the newly created inverse is correct. 
    print("checking if inverted matrix is correct. Correct result shows a matrix of all TRUEs")
    print(z == solve(x$get()))
}
