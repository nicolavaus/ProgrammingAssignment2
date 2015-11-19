## Matrix inversion is usually a costly computation and 
##there may be some benefit to caching the inverse of a matrix rather than
##compute it repeatedly 
##
## The functions makeCacheMatrix and cacheSolve will calculate the inverse of a square matrix
## and cache it's result
## 
##
## This function creates a special "matrix" object that can cache its inverse.
## The matix x has to be an invertible square matrix
## For example 
##        x <- matrix(1:4,2,2)
##        tmp <- makeCacheMatrix(x)
##        cacheSolve(tmp)
##              will print on the screen the square matrix
##
## to verify that it is the inverse of matrix x
## the user can execute the following command:
##
##        x %*% cacheSolve(tmp)
##              will print the identity matrix
##
## Example Results:
## 
## 1. Create matix x
##
##     > x <- matrix(1:4,2,2)
##
## 2. print matrix x on the screen
##     > x
##             [,1] [,2]
##       [1,]    1    3
##       [2,]    2    4
## 
## 3. Call function makeCacheMatix and store the results in variable tmp
##
##    > tmp <- makeCacheMatrix(x)
##
## 4. Check the variable tmp has the correct matrix
##    > tmp$get()
##            [,1] [,2]
##      [1,]    1    3
##      [2,]    2    4
##
## 5. Call function cacheSolve(tmp) and see results on the screen
##
##     > cacheSolve(tmp)
##              [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5
##
## 6 Verify results using %*% on the matrix and its inverse will
##   give as a result the identity matrix, and that the value was already in cache
##    
##    x %*% cacheSolve(tmp)
##    getting cached data
##             [,1] [,2]
##       [1,]    1    0
##       [2,]    0    1 
##
##
##
##
##
##
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse of a matrix
  setinverse <- function(solve) m <<- solve
  
  ## get the value on the inverse of a matrix
  getinverse <- function() m
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}