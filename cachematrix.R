## This R script includes functions that aid in caching the 
## inverse of a square matrix. The function makeCacheMatrix() has set, get 
## methods for setting and getting the matrix input respectively. The CacheSolve() 
## function tries to retrieve the inverse of the matrix from the environment. 
## In case the inverse matrix for the given matrix input is not found in the 
## enviroment/cache, the inverse is computed and stored in the enviroment for 
## future retrieval. 

## makeCacheMatrix has set matrix , get matrix, set matrix inverse and 
## get matrix inverse functions. The <<- operator in set matrix inv function
## first tries to see if the inverse matrix is already available in the 
## environment

makeCacheMatrix <- function(x = matrix()) {
  matinv<- NULL  
  setmatrix <- function(y){
    x <<- y
    matinv <<- NULL
  }
  getmatrix <- function() x
  
  setmatrixinv <- function(mat){
    print('Caching the inverse matrix ')
    matinv <<- solve(mat)
    print(matinv)  
    
  }
  getmatrixinv <- function() matinv 
  
  list(set=setmatrix, get=getmatrix, getinv=getmatrixinv, setinv=setmatrixinv)
}


## CacheSolve takes the above makeCacheMatrix as input. The function calls
## makeCacheMatrix's get inv matrix method.If the value retruned is non-null, 
## the inverse matrix is retrived from the environment. If value returned is 
## null, makeCacheMatrix's set inv matrix method is called

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinv()
  if(!is.null(matinv)) {
    print("Inverse matrix already exists, retrieving from cache ...")
    return(matinv)
  }
  
  mat <- x$get()
  x$setinv(mat)
}
