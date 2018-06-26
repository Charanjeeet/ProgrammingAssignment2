##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
 -#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
 -#can cache its own object. 
 -
 -#<<- operator is used to assign a value to an object in an environment that is different 
 -#from the current environment 
 -
 -#take the matrix as an input

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


-## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
 -# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
 -# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
 -# and set the invertible  matrix by using the solve function.
 -# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
 -#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
 -#and the cached object
 -  
 -

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
  		if(!is.null(inv)){
    		message("getting cached data")
    		return(inv)
  		}
  	data <- x$get()
  	inv <- solve(data)
  	x$setInverse(inv)
  	inv      
}
