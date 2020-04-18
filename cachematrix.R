## A pair of functions that cache the inverse of a matrix

##Creates a matrix object
makeCacheMatrix <- function(x = matrix()) {
  
  n <- NULL ##intailize of inverse matrix
  ## method to set
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  ## method to get
  get <- function() x
  ## method to setinverse
  setinverse <- function(inverse){ n <<- inverse}
  ## method to getinverse
  getinverse <- function(){ n}
  ## list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## compute the inverse of the matrix and aslo check the cache value

cacheSolve <- function(x, ...) {
  ##get the inverse
  i <- x["getinverse()"]
  if(!is.null(i)) { ##checking inverse exist are not if it is return it
    message("getting cached data")
    return(i)
  }
  data <- x["get()"]## getting the matxix
  i <- solve(data) %*% data ##performing inverse
  x["setinverse(i)"] ## set the inverse
  i ## return the inverse
  
}
