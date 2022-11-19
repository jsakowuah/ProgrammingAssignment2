## The first function shall create a matrix object that caches its inverse and the second shall compute the inverse of the 
#special matrix created. (Coursera Week 3 Programming Assignment 2)
#Name: Joseph Siaw Akowuah. 


library(MASS) #package in R that calculates inverse for non squared as well as square matrices

#Creating a function of a special 'matrix' object that caches its inverse. ====

makeCacheMatrix <- function(x = matrix()) { #function created with argument 'x' named with default matrix function. 
  inv<-NULL            #assigning NULL to inv to hold the value of the matrix inverse. 
  set<-function(y){    #assigning a function of 'y' to set 
    x<<-y               #where 'y' is the value of matrix in parent environment
    inv<<-NULL          #if new matrix, reset inv to NULL
  }
  get<-function()x             #function to get matrix x OR returns value of matrix argument
  setinv<-function(inverse)inv<<-inverse #assiging value to inv in parent environment
  getinv<-function(){ 
    inver<-ginv(x)
    inver%*%x           #function to obtain inverse of the matrix
  }
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this function
##This is used to get the cache data

cacheSolve <- function(x, ...) ##gets cache data      
{
  inv<-x$getinv()                  
  if(!is.null(inv)){                 #checking whether inverse is NUll 
    message("getting cached data!")
    return(inv)                       #returns inverse value
  }
  data<-x$get()
  inv<-solve(data,...)              #calculates inverse value
  x$setinv(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}

#Testing the functions to see if it works
z<- makeCacheMatrix(matrix(1:8, 2,4))  #creates a matrix of 8 numbers with 2 rows and 4 columns
z$getinv() #gets the inverse of z 
cacheSolve(z) #getting cached data