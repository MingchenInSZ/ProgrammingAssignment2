## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#make cache to the matrix
makeCacheMatrix <- function(x = matrix()) {
keCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y;
    inv<-NULL;
  }
  get <- function()x
  setinv<-function(invs)inv<<-invs
  getinv<-function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
#first look up if the matrix is in the cache, if true then retun, else calculate the 
#inverse by using solve(A) function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv<-x$getinv()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinv(inv)
  inv
}
