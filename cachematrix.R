## Put comments here that give an overall description of what your
## functions do
## 
## The makeCacheMatrix() function below returns series of functions which work on a square matrix
## The first line of code in the body of the function creates an empty object called m 
## The first function within the body caches the value of a free variable y
## (which I have defined on top of the function) to the empty matrix called x(x is the input variable). 
## it does same for the empty value m (caches m)
##  The subsequent functions within the body do the following: 
##          get() gets the cached x value, below the get() is defined the inverse of x
##          setinerse() caches the result (a) which is an inverted matrix to the empty variable "m",
##          getinvers() retrieves the cached object "m"
##  
##  The function (makeCacheMatrix) basically outputs a list of functions
##  which sets and caches the square matrix
##  which gets the cached matrix(not-inverted)
##  which sets and caches the inverse of the matrix
##  which gets the cached inverse of the matrix
##  

## Write a short comment describing this function

y <- matrix(1:4, nrow = 2, ncol = 2, byrow = F)

makeCacheMatrix <- function(x = matrix(NA, nrow = 2, ncol = 2)) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() {get("x", environment(set))}
      x <- solve(x)
      setinverse <- function(x) {m <<- solve(x)}
      getinverse <- function() {get("m",environment(setinverse))}
      list(set=set, get=get,
           setinverse=setinverse,
           getinverse=getinverse)
}



## Write a short comment describing this function
## 
## The function makeCacheMatrix() attempts to compute the inverse of x
## it first checks to see if the inverse is already computed in the if statement and if 
## it returns "TRUE", the returns the already computed inverse and if "FALSE"
## it retrieves the cached x value and computes the inverse using the solve function
## and then caches the inverse using setinverse()

a <- makeCacheMatrix()
cacheSolve <- function(a, ...){
      m <- a$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- a$get()
      m <- solve(data)
      setinverse(m)
      m
}



#cacheSolve <- function(x, ...) {
#        ## Return a matrix that is the inverse of 'x'
#}
