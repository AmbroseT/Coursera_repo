## This code will take a square matrix, calculate its inverse,
## and store it in cache if not there already.  Calling the solve
## function will return the result from cache if it is already
## there, otherwise it will calculate and return the result. The
## key lesson in this code is lexical scoping in R, in particular, 
## the symbol, <<-

## I just want to say kudos to ADAM GRUER from the discussion 
## board who correctly illustrated the proper use of the stored
## functions.  This made it easier for all to understand how to
## actually use the stored functions (.e. 'set' and 'get')

## Using lexical scoping for caching allows you to save time
## with complex coding.

## NOTE: Since you are calculating the inverse of a matrix, the matrix must
## be square; meaning that the matrix has an equal amount of rows to
## columns, i.e. 2x2 matrix, 3x3 matrix, 4x4 matrix, etc.

## also NOTE: some matrices are invertible, meaning that you cannot
## calculate its inverse. This is where the determinant is 0 (dividing
## by 0). This invertible matrix is called a singular matrix.

## An example of a singular matrix is a 3x3 matrix with data 1 to 9.
## in R this would be represented as matrix(1:9, 3, 3).  Trying to calculate 
## the inverse of this matrix in R will result in an error. 

## For illustration purposes, use a 'safe' 2x2 matrix such as 
## matrix(c(4, 2, 7, 6), 2, 2). Its inverse will be returned as,
## in matrix form:
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

## The following function stores a list of 4 functions called set, 
## get, setinv, and getinv.  Note the symbol <<- being used. Calling 
## this function into a variable will store the variables using 
## lexical scoping. Once set, you can call the functions within to 
## perform the functions as coded by set, get, setinv, and getinv. 
## Using the above example:
## > m <- makeCacheMatrix()
##
## Now you can run the defined set function with a 'safe' matrix,
## as follows:
## > m$set(matrix(c(4, 2, 7, 6), 2, 2))
##
## Calling the defined get function will return the matrix you entered:
## > m$get()
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(solve) {
    inv <<- solve
  }  
  getinv <- function() {
    inv
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## The following function uses the variable that you 
## created with the matrix, calls the listed function getinv
## and stores it into a variable. Note the lexical scoping. From
## there, if it is empty, it will calculate the inverse of
## your matrix and return the solution, after storing it in
## cache (lexical scoping). If you call this function 
## again, it will retrieve the solution from cache. Using
## the example above:
## > cacheSolve(m)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
##
## calling the solve function again will retrieve from cache, and
## will let you know it is retrieving from cache:
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

cacheSolve <- function(x, ...) {
 
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
