## Put comments here that give an overall description of what your
## functions do

## This function creates a list containing four functions that
## store a matrix and its inverse
## The Matrix given must be square (# rows = # cols) and non-singular (i.e. inverse of matrix != 0)

makeCacheMatrix <- function(x = matrix()) {
## My inv variable is initially set to Null
  inv <- NULL
## set() is the 1st out of four functions created by the makeCacheMatrix function
## This function sets the values for the arguments X and m in the parent environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
## get() is the 2nd function that is created
## This function just grabs the value of X as it was defined by the user
  get <- function() x
## setsolve() is the 3rd function that is created
## This function assignse the inverse matrix to the inv argument
  setsolve <- function(solve) inv <<- solve
## getsolve() is the 4th function that is created
## This function just grabs the value of inv as it was defined by setsolve()
  getsolve <- function() inv
## This assigns names to each object in the list so that they can be called out by name in cacheSolve
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}


## This function computes the inverse of the matrix stored in makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve should retrieve the inverse from the cache
## I decided to name the variable for this function mx instead of x
## because the fact that both functions were using x that each have a different significance confused me

cacheSolve <- function(mx, ...) {
## Here it is assigning the value stored in getsolve() to inv
 inv <- mx$getsolve()
## Here it is checking if inv is Null, if not, grab cached value, else move onto the next line of code
## Finally, return the value of inv
 if(!is.null(inv)) {
   message("getting cached data")
   return(inv)
 }
## Here its just computing the inverse of the matrix using the get() function to grab
## the matrix defined by the user in the makeCacheMatrix function
## inv is returned
 data <- mx$get()
 inv <- solve(data, ...)
 mx$setsolve(inv)
 inv
}
