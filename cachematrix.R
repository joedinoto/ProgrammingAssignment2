## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# x is a square matrix, by default a random 2x2 matrix.
# xInv is the inverse of x (if xInv exists, which it almost always will)


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


## Write a short comment describing this function

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

## Useful links 
## https://www.statmethods.net/advstats/matrix.html
## a<- matrix(rnorm(4),2,2)  Creates a 2x2 matrix with random normal numbers
## a<- matrix(rnorm(9),3,3)  Creates a 3x3 matrix with random normal numbers
## solve(a) finds the inverse of a square matrix (if it exists)
## a %*% aInv a multiplied by its inverse (will give e-16 or smaller results)
## round(a %*% aInv,2) will give 1,0,0... form. 