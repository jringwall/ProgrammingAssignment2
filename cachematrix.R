## makeCacheMatrix function:
##    takes an input value for a matrix
##    once new value for matrix is entered, sets inverse variable to null
##    creates 4 functions to retrieve and set value of matrix and inverse
##    stores list containing these 4 functions

makeCacheMatrix <- function(x = matrix()) {
      inverse<-NULL
      set<- function(y){
            x<<-y
            inverse<<-NULL
      }
      get<- function() x
      setinv<- function(invvalue) inverse<<-invvalue
      getinv<-function() inverse
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##cacheSolve function:
##    takes an input value that should be a variable created using function makeCacheMatrix
##    attempts to retrieve a stored value for the inverse of matrix from input
##    if no stored value exists, calculates the inverse of matrix from input
##    stores the value of the inverse in variable called "inverse"
##    prints contents of inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse<-x$getinv()
      if(!is.null(inverse)){
            message ("getting cached data")
            return(inverse)
      }
      data<-x$get()
      inverse<-solve(data,...)
      x$setinv(inverse)
      inverse
}
##Some tests to check how functions are working

#Create value for x matrix 3,4,5,11 in makeCacheMatrix
      acachematrix<-makeCacheMatrix(matrix(c(3,5,6,11),nrow=2,ncol=2))
#feed acachematrix to cacheSolve function to display inverse of x matrix
      solved<-cacheSolve(acachematrix)
      solved
#create value for x matrix 11,5,3,6 in variable bmatrix
      bmatrix<-makeCacheMatrix(matrix(c(11,5,3,6),nrow=2,ncol=2))
#call current value of matrix
      bmatrix$get()
#try to display cached inverse of matric. This is null because it hasn't yet been calced
      bmatrix$getinv()
#feed bmatrix into cacheSolve function in order to display the inverse of matrix
      cacheSolve(bmatrix)
#set new value for matrix
      bmatrix$set(matrix(c(3,5,6,11),nrow=2,ncol=2))
#retrieve new value of matrix
      bmatrix$get()
#try to retrieve cached inverse of matrix. there isn't one because input has changed and inverse not yet calced
      bmatrix$getinv()
#use cacheSolve to calc value of inverse
      cacheSolve(bmatrix)
#display cached value of inverse
      bmatrix$getinv()



