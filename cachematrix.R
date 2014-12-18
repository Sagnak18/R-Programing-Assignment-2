## This function creates an object that saves the input matrix as well as inverse matrix values.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL     # m is the inverse of the Matrix, reset to NULL every 
  # time that makeCacheMatrix is called
  
  # note these next three functions are defined but not run when makeCacheMatrix is called
  # instead, they will be used by cacheSolve() to get values for x or for
  # m (inverseMatrix) and for setting the inverse of the Matrix. These are usually called object 'methods'
  
  set <- function(y) { ## takes the input maatrix
    x <<- y            ## saves the input matrix  
    m <<- NULL         ## resets the inverse of the Matrix to NULL, basically what happens what a new object is generated
  } 
  get <- function() x ## this function returns the value of the original matrix 
  setmatrix <- function(matrix) {m <<- matrix} 
  ## this is called by cachematrix() during the first cachematrix()
  ## access and it will store the value using superassigment
  
  getmatrix <- function() m ## this willl return the cached value to cachematrix() on 
  ## subsequent acccesses
  
  list(set = set, get = get, ## OK, this is accessed each time makeCacheMatrix() is called,
       setmatrix = setmatrix,    ## that is, each time we make a new object. This is a list of 
       getmatrix = getmatrix)    ##  the internal functions ('methods') so a calling function
  ## knows how to access those methods.
}

## This function calculates the inverse of the matrix if it has not otherwisse been calculated, otherwise 
## it calls up the cached inverse matix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()              # accesses the object 'x' and gets the values for the inverse matrix
  if(!is.null(m)) {             # if the inverse was already cached (not Null) ...
    
    message("getting cached data") # ... send this message to the console
    return(m)                      # ... and return the matrix inverse ... "return" ends
    # the function cacheSolve(), note                                  
  }
  data <- x$get()       # we reach this code only if x$getmatrix() returned NULL  
  
  m <- solve(data, ...)  # if m was NULL then we have to calculate the inverse matrix 
  
  x$setmatrix(m)          # store the calculated matrix value in x
  
  m                     # return the inverse matrix to the code that called this function
}