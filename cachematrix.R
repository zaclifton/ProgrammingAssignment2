## makeCacheMatrix assigns cached variables 'x' and 'm'
## 	through its return functions
##		$set - takes input matrix as its argument and assigns it to 'x'
##		$get - returns 'x'
##		$setsolve - assigns inverse matrix to 'm'
##		$getsolve - returns 'm'
## cacheSolve calculates the inverse and assigns it to 'm', if 'm' is NULL
##	returns 'm' - if it exists or if calculated
##	main input argument is the assigned output from calling makeCacheMatrix()


## returns a set of functions to assign (environment) variables 'x' and 'm'
## 'x' is the input matrix; 'm' is its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y     #y is the input matrix
        m <<- NULL  #m is the inverse, which is nothing yet
    }
    get <- function() x  #this is the matrix assigned by 'set'
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## computes inverse of matrix, if it's not already cached,
## and assigns inverse to (environment) variable 'm'

cacheSolve <- function(f, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- f$getsolve()
    #if inverse exists - !is.null - return it along with a message
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #otherwise - create the inverse; assign it to 'm'; and return it
    data <- f$get()  #returns the input matrix ('x') from makeCacheMatrix
    m <- solve(data) #create the inverse
    f$setsolve(m)    #assign (cache) inverse to 'm'
    m                #return 'm'
}
