## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL           
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #creates and returns a list with set, get, setinverse and getinverse
}


# cacheSolve(): This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve retrieves the inverse from the cache. 
# If the inverse was not calculated before, the function will calculate using solve().

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse() 
  if(!is.null(inverse)){
    message("getting cached data - inverse of matrix")
    return(inverse)           #in case of cached data, it returns the inverse of matrix
  }
  data <- x$get()
  inverse <- solve(data, ...)   #uses function solve to calculate the inverse of matrix
  x$setinverse(inverse) 
  inverse                      #returns the inverse of matrix
}
