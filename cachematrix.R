## Put comments here that give an overall description of what your
## functions do
## setInverse(x)  stores x, the inverse calculated matrix in inverseMat
## getInverse  returns inverseMat stored matrix
## getMatrix   returns the last matrix used
## setMatrix   sets the matrix object to use in inverse calculation

## Write a short comment describing this function
## makeCacheMatrix sets 2 variable objects that contain matrix being used and the calculated inverse.
##                 Also, It holds the definitions of the setter and getter functions
makeCacheMatrix <- function(x = matrix()) {
   
   currentMat <- x
   inverseMat <- NULL
   
   list(
   setInverse = function (y) inverseMat<<- y
   ,
   getInverse = function()   inverseMat
   ,
   setMatrix =  function(y)  currentMat <<- y
   ,
   getMatrix =  function()   currentMat
   
   )
   
}


## Write a short comment describing this function
## calls the calculation of inverse and decides when to recalculate it
## or return the inverse previosly calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #get current values from cache
        cacheMat <- x$getMatrix()
        cacheInv <- x$getInverse()
        
        
        if ( !identical(x,cacheMat) ){ # if matrix changed
          
           x$setMatrix(x)
           x$setInverse(solve(cacheMat))
           
           x$getInverse()         
        }
        else { # if matrix didnt changed
          if ( is.null(cacheInv) ){ # first time finds inverse not set yet
            
            x$setInverse(solve(cacheMat))
            
            x$getInverse()
            
          }
          else { # on re-run dont need to do solve again, just get cached inverse
            
            x$getInverse()
          }
        }        
}

#example of use
#c=matrix(c(2, -1/8, -1/8, 2), nrow=2,ncol=2)
#objmatfun <- makeCacheMatrix(c)
#a<-cacheSolve(objmatfun)


