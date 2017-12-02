## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix() function stores getters and setters (as a list) of a "matrix" object"
#...so that its inverse can be cached 
#cacheSolve() function calculates and returns inverse of a matrix created by first function and stores it in a cache
#... or gets it from a cache if inverse was already calculated and stored


## Write a short comment describing this function

#1. The function takes one default argument of class matrix
#2. variable i = NULL (empty variable) is defined
#...(it will show if the right inverse is stored in cache or not)
#3. there are four nested functions in the list:
#a. set(y) sets new value of a function argument, and resets the "i" value 
#... << symbol chnges initialized values in the parent environment
#b. get() gets us the matrix which was set just before
#c. setinverse() calculates inverse of that matrix
#d. and getinverse() returns the value calculated inverse

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
   }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
  
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
  
## Write a short comment describing this function

#1. looks into cache if inverse (i) of a matrix created by makeCacheMatrix() is already calculated
#a. if (!is.null(i)) it writes a messege and returns value of i from cache
#b. (else) creates a new inverse for the matrix (using solve() function) and returns it


cacheSolve <- function(x, ...) {
  
    i <- x$getInverse()
        
        if(!is.null(i)) {
            message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data,...)
    x$setInverse(i)
    i
}
