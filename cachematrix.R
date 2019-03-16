# makeCacheMatrix() will create a list with four functions and two variables. 
# cacheSolve() will find the inverrse of the matrix from makeCacheMatrix().

# functions
# get() gets the matrix x
# set() sets a new matrix x 
# getInv() gets the inverse of x
# setInv() finds the inverse of x 

# variables
# x is a square matrix, by default a random 2x2 matrix.
# xInv is the inverse of x (if xInv exists, which it almost always will)

# To run
# > aMatrix <- makeCacheMatrix() runs makeCacheMatrix() and assigns the output list to aMatrix 
# > aMatrix$get() gets the random 2x2 matrix you just created
# (the next few line won't match your output since the matrix is random)
# [,1]      [,2]
# [1,] 0.3589449 0.3589449
# [2,] 1.2539631 1.2539631
# > aMatrix$getInv() gets the inverse of that 2x2 matrix but it won't work because you haven't run 
# cacheSolve() yet, what's why you get NULL output
# NULL
# > aMatrix$set(matrix(rnorm(9),3,3)) this sets a new 3x3 matrix with 9 random normal numbers as x
# >cacheSolve(aMatrix) finds the inverse of the 3x3 matrix you just created
# (Your output won't match this exactly since the matrix is randomly created)
# [,1]       [,2]       [,3]
# [1,]  0.6058007 -0.5413059 -0.5498613
# [2,] -0.9802915  1.8129401 -0.4305452
# [3,]  0.3824738  0.2526347  0.1864437
# > aMatrix$getInv() gets the inverse of the 3x3 matrix you just created
# [,1]       [,2]       [,3]
# [1,]  0.6058007 -0.5413059 -0.5498613
# [2,] -0.9802915  1.8129401 -0.4305452
# [3,]  0.3824738  0.2526347  0.1864437
# > round(aMatrix$get() %*% aMatrix$getInv(),2) this multiplies the matrix and its inverse and 
# rounds the results to 2 places. You should obtain the identity matrix (1,1,1 on diagonal, 0s elsewhere)
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1


makeCacheMatrix <- function(x = matrix(rnorm(2),2,2)) {
  xInv <- NULL
  set <- function(y){
    x<<- y
    xInv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) xInv <<- solve
  getInv <- function() xInv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


#cacheSolve 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xInv <- x$getInv()
  if(!is.null(xInv)){
    message("getting cached data")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data,...)
  x$setInv(xInv)
  xInv
}

## REFERENCES
## ESSENTIAL READING (without this I would have never been able to finish this assignment.)
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## Useful links 
## https://www.statmethods.net/advstats/matrix.html
## a<- matrix(rnorm(4),2,2)  Creates a 2x2 matrix with random normal numbers
## a<- matrix(rnorm(9),3,3)  Creates a 3x3 matrix with random normal numbers
## solve(a) finds the inverse of a square matrix (if it exists)
## a %*% aInv a multiplied by its inverse (will give e-16 or smaller results)
## round(a %*% aInv,2) will give 1,0,0... form. 