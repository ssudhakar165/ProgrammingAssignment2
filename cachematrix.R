##  I prefer to catch the matrix, set, get ,  inverse and store both.
## We can catch the matrix, inverse and store it

##  working directory is set and the file is read. 

## makecatchmatrix will catch a matrix and inverse the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  #store the matrix
  setmatrix<-function ()x
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
  dat1<-solve(data, ...)
  Y$catchinverse(dat1)
  #return the value
  dat1

  
  ## Return a matrix that is the inverse of 'x'
}
