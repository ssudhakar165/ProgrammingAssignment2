## makecatchedmatrix will catch a matrix and inverse the matrix.
##  I prefer to catch the matrix, set, get ,  inverse and store them.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  #initiallising the vectors
  set<-function(y){
    x<<-y
    m<<-NULL
       data<<-NULL
  }
  #store the matrix
  get<-function() x
  setmatrix<-function ()x
  #return to matrix
  getmatrix<-function() x
  ## inverse of the matrix
  catchinverse()<-function(solve) m<-solve
  #get the inverse matrix
  getinverse<-function()m
  #return a list
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix
       catchinverse=catchinverse
       getinverse=getinverse)
}
## #cacheSolve: This function computes the inverse of the 
#special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x=matrix(), ...) {
  #get the catched value
  m<-x$getinverse()
  # Catch value exists, return 
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #otherwise, catch the matix
  data<-x$getmatrix()
  #get the inverse of the matrix
  m<-solve(data, ...)
  x$catchinverse(m)
  #return the value
  m
  
  ## Return a matrix that is the inverse of 'x'
}
