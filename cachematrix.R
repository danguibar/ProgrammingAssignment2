## Put comments here that give an overall description of what your
## functions do
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   
   currentMat <- x
   inverseMat <- NULL
   
   list(
   setInverse = function (y) inverseMat<<- y
   ,
   getInverse = function()  inverseMat
   ,
   getMatrix = function()   currentMat
   ,
   setMatrix =  function(y) currentMat <<- y
   )
   
}


## Write a short comment describing this function

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

c=matrix(c(2, -1/8, -1/8, 2), nrow=2,ncol=2)
objmatfun <- makeCacheMatrix(c)
a<-cacheSolve(objmatfun)


