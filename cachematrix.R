
## makeCacheMatrix returns a list of functions. 
## Function get is for data input,
## Function setinverse is assign the inverse matrix to the global enviroment
## Function getinverse is returning the inverse matrix from the global enviroment

makeCacheMatrix <- function(x = matrix()) {
  
  it <- NULL 
  
  set <- function(y) { 
    x <<- y
    it <<- NULL
  }
  
  get <- function() x 
  setinverse <- function(solve) it <<- solve 
  getinverse <- function() it 
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}


##This function returns the inverse matrix if this matrix does not exist in the global enviroment

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  it <- x$getinverse() #assign it the value it has from the global enviroment
  
  ##if this matix in not NULL then ot doen't do the calculation and return it.
  
  if(!is.null(it)) { 
    message("getting cached data")
    return(it)
  }
  
  data <- x$get() ##getting the data (input matrix)
  it <- solve(data, ...) ##calculate the reverse matrix
  x$setinverse(it) ##setting the new value of iverse matrix to the global enviroment 
  it
}
