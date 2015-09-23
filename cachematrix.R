
## makeCacheMatrix is a function that stores a list of functions
## as well as the cached value for the inverse Matrix (if it is known)
## It acts like a class in object oriented programming and we will later
## create an object of it, which we use for our second function cacheSolve.

## The functions embedded in makeCacheMatrix are set, get, setInv and getInv
## (1) get - returns the matrix x stored in the main function (x 
##     has been passed on when calling the function/initialising the object)
## (2) set - set changes the matrix stored in the main function
##     (not really needed in our example)
## (3) setInv and getInv are effectively data storages. setInv saves
##     inv as new value for i when called with the argument inv
## (4) getInv recovers the current value of i - i will be our cached 
##     inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setInv <- function(inv) i <<- inv
   getInv <- function() i
   list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Our second function cacheSolve gets called using an object of 
## makeCacheMatrix. It then makes use of the functions 
## embedded in our object.
## The function first checks if i in our object is different to NULL.
## If it is different to NULL (CASE 1) it means we don't have a cached object
## available - no inverse matrix has been saved before. We therefore
## need to calculate it by calling solve() on the data of our object x
## this inverted matrix is then saved in our "class" makeCacheMatrix.
## (CASE 2) If at the check i is not NULL it means we have a saved 
## inverse matrix in our object. We therefore return that cached
## matrix which was saved in our object.

cacheSolve <- function(x, ...) {
   i <- x$getInv()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setInv(i) 
   i
   
}

# Example of an implementation
# 
# > a <- makeCacheMatrix(matrix(c(3,3.5,3.2,3.6),2,2))
# > cacheSolve(a)
# [,1] [,2]
# [1,] -9.00  8.0
# [2,]  8.75 -7.5
# > cacheSolve(a)
# getting cached data
# [,1] [,2]
# [1,] -9.00  8.0
# [2,]  8.75 -7.5
# > 
## Thanks for evaluating!
