## These two functions are used to store a given matrix 
## and to cache its inverse matrix 

## This function creats a list of funtions which set a matrix, get a matrix, set an inverse matrix and get an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   inv<-NULL   ## whenever we call the function with a new matrix (and in the first run) we set this argument to NULL
  set<-function(y) {
        x<<-y
        inv<<-NULL
    } 
    get<-function() {
      x
    } 
    setinverse<-function(inverse) {
      inv<<-inverse   ##after calculation the inverse (by the next function) the value is stored
    } 
    getinverse<-function() {
      inv
    }
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function calculates the inverse of a given matrix after checking wether it was calculated before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv<-x$getinverse() ##in the first run, or in the case of new matrix - the value of inv will be NULL 
   if (!is.null(inv)) {  ## if it is not NULL - the inverse matrix was calculated before
        message("getting cached data")
        return(inv)
   }
   data<-x$get()  ## if it is a new matrix we want to: 1. store it...
   inv<-solve(data,...)  ## 2.to calculate its inverse matrix...
   x$setinverse(inv)  ## 3. to cache the inverse matrix 
}
