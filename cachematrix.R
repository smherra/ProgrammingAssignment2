## Function creates a Matrix that uses the inverse feature
## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {   ##create function named makeCacheMatrix

  inverse<-NULL                               ##start inv as NULL to hold the matrix w/empty value
  set<-function(y){                           ##Define 'set' for new function
    x<<-y                                     ##Set value of matrix
    inverse<<-NULL                            ##New variable - this resets to NULL
  }
  get<-function()x
  setinverse<-function(inv)inverse<<-inverse  ##places the 'set' function
  getinverse<-function()inverse
  list(set=set, get=get)
      setinverse=setinverse
      getinverse=getinverse                   ##gets the inv value when called for
}


## This function provides the inverse response for the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("retrieving cached data...please wait")  ##Provides message during retrieval
    return(inv)
  }
  data <-x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


