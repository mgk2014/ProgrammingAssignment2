## cachematrix.R
## author: mgk2014
## date created - 21-Jul-2014
## coursera - r-programming, programming assignment 2
## test - script includes a test() for reference

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

    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invMatrix <<- inverse
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
    
    invMatrix <- x$getinverse()
    if (!is.null(invMatrix)){
        message("getting cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data)
    x$setinverse(invMatrix)
    invMatrix
}


## test()
#   This function verifies the functions and serves as a sample code to test these functions
# 
test <- function() {

    # create an instance of the function
    x <- makeCacheMatrix(matrix(sample(100,25), 5,5)) 
    
    # check the newly created matrix
    y <- x$get()                                   
    print("new matrix")
    print(y)
    
    # the inverse should be NULL at this point
    z <- x$getinverse()
    print(paste("inverse matrix", z))
          
    # cache the inverse
    cacheSolve(x)                                   
    z <- x$getinverse() 
    print("inverse matrix after caching")
    print(z)
          
    # check that the the newly created inverse is correct
    print("checking if inverted matrix is correct")
    print(z == solve(x$get()))
}
