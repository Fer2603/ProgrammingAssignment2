## Put comments here that give an overall description of what your
## functions do

#Lets start defining the function
#x is our entry argument, we suppose its invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # first, we initialize the inverse matrix with a null value
  #Now, lets set the value of the matrix
  set <- function(y){
    x <<- y #<< to state to R to use an object value from the environment we defined previously
    inv <<- NULL #<< allow us to have 2 level of parameters
  }
  get <- function() {x} #here we get the value of the matrix
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv} #lets get the value of the index
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}
  



#function that computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse() #gets inverse of x and stores it in inv
        #First, lets see if the inverse has already being calculated
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        #otherwise, we need to compute the value of the inverse matrix 
        mat <- x$get()
        #we use the instruction solve(), the standar R function for this
        inv <- solve(mat, ...)
        #sets the value of the inverse in the cache via de setInverse function
        x$setInverse(inv)
        #what our function will return:
        inv
}
