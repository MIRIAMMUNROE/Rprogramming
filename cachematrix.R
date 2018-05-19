
## The makeCacheMatrix function creates a matrix that is able to cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  get<- function() x
  
  setInvMatrix <- function (InvMatrix) m <<- InvMatrix
  getInvMatrix <- function () m
  
  list(set=set, get=get, setInvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
  
}


## The cacheSolve function computes the inverse of the makeCachematrix. 

cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix ()
  if(! is.null(m)){
    message("getting catched data")
    return(m)
  }
  
  data<- x$get()
  m <- InvMatrix (data, ...)
  x$setInvMatrix (m)
  solve(m)
  
  ## Return a matrix that is the inverse of 'x'
}

