#The first function, makeVector creates a special "vector", which is really a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}




## The following function calculates the inverse of the matrix created with the above function.However it checks whether the inverse is already
## calculated.If so it "get" s it from the cache rather than computing it.
## OtherWise it calculates the inverse and set the cache via "setinverse" function.



cacheSolve <- function(x, ...) {
        
   m <- x$getinverse()                    
   if(!is.null(m)) {              
    message("getting cached data") 
    return(m)                     #just return the cache, no computation needed
  }
  data <- x$get()                 #if there's no cache
  m <- solve(data, ...)           #we actually compute them here
  x$setinverse(m)                 #save the result back to x's cache
  m                               #return the result
}
