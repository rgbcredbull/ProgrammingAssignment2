## makeCacheMatrix builds new makeCacheMatrix object.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  ##declares m as null. 
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ##set declares m null at the environmental overiding parent declaration.
  ##sets input as x.
  get<-function(){
    x
  }
  ##get function returns the unaltered value of x.
  setsolve<-function(solve){
    m<<-solve
  }
  ##setsolve sets m as the cache file.
  getsolve<-function(){
    m
  }
  ##getsolve returns the cache file.
  
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## cacheSolve evaluates the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
  m<-x$getsolve()
  ##returns the cache value.
  if(!is.null(m)){
    message("getting catched data")
    return(m)
  }
  ##if there is a cache value, returns cache data.
  data<-x$get()
  ##returns current value for x (unlatered data).
  m<-solve(data,...)
  ##solves the matrix.
  x$setsolve(m)
  ##sets the cache value for m.
  m
  ## Return a matrix that is the inverse of 'x'.
}

