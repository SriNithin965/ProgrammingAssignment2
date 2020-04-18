## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse){ n <<- inverse}
  getinverse <- function(){ n}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x["getinverse()"]
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x["get()"]
  i <- solve(data) %*% data
  x["setinverse(i)"]
  i
  
        ## Return a matrix that is the inverse of 'x'
}
