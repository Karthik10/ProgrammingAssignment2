## There are two main functions makeCacheMatrix() and cacheSolve().
## makeCacheMatrix() creates a matrix and it can cache its inverse.

## Brief steps involved
## a) Set the matrix
## b) Get the matrix
## c) Set the inverse of the matrix
## d) Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
  {
      inv_mat <- NULL
      Set <- function(y) 
        {
            if(!is.matrix(y))
              {
                print("This is not a matrix")
                return()
              }
      x <<- y
      inv <<- NULL
        }
  
      Get <- function() x
      SetSolve <- function(inv_mat) i <<- inv_mat 
      GetSolve <- function() i
      list(set = Set,get = Get,setsolve = SetSolve,getsolve =GetSolve)
  }


}


## cacheSolve() function computes the inverse of the matrix that is returned by the 
## makeCacheMatrix(),ie. it checks if the inverse has already been calculated for the
## same matrix if yes then it retrives from the cache else it is calculated.

cacheSolve <- function(x, ...) 
  {
      inverse<-x$GetSolve()
      if(!is.null(inverse))
        {
          print("Getting from cached inversed matrix")
          return(inverse)
        }
      data1 <- x$Get()
      inverse <- solve(data1)
      x$SetSolve(inverse)
      inverse
  }


