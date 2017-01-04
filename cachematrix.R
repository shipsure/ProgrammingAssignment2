## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
        ##      x is a square invertible matrix  
        ##         return: a list containing functions to  
        ##              1. set the matrix  
        ##              2. get the matrix  
        ##              3. set the inverse  
        ##              4. get the inverse  
        ##         this list is used as the input to cacheSolve()    
        
        inv = NULL  
        set = function(y) {                    
                x <<- y
                inv <<- NULL  }
        get = function() x  
        setinv = function(inverse) inv <<- inverse   
        getinv = function() inv  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



cacheSolve <- function(x, ...) {

          ## x: output of makeCacheMatrix()  
          ## return: inverse of the original matrix input to makeCacheMatrix()    
          
          inv = x$getinv()    
          # if the inverse has already been calculated  
          if (!is.null(inv)){    
                  # get it from the cache and skip the computation.     
                  message("getting cached data")    
                  return(inv)  
          }
          #  calculates the inverse if not calculated earlier  
          origdata = x$get()  
          inv = solve(origdata, ...)    # sets the value of the inverse in the cache via the setinv function. 
          x$setinv(inv)    
          return(inv)
        
}
