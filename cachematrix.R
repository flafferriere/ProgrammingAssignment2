## Put comments here that give an overall description of what your
## functions do
## optimizing matrix inversion calculus  keeping inversion results into a cache


## Write a short comment describing this function
## return a list of function to set,get,setinverse, getinverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
 set <-function(y){
         x <<- y
         inv <<- NULL
 }
 get <-function() x
 setinv <-function(invert) inv <<-invert
 getinv <-function() inv
 list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
## compute the inverse of a matrix. If inverse had already been computed, use
## the cache value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv=x$getinverse()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
                
        }
        data=x$get()
        inv=solve(x,...)
        x$setinv(inv)
        inv
}
