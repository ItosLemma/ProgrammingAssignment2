## These functions, together, find the inverse of a matrix and 
## store it (cache it) for instant recall later in a program. 
## Three comments: (1) My functions are identical to the 
## example given in the instructions for this project, but                                      ## with new terms replacing "m" and "mean". This is not
## necessary -- you could keep the given names. The only
## necessary change is to replace "mean(x)" with "solve(x)".
## (2) The real issue is how to use the functions. After much 
## experimentation I concluded that calling "makeCacheMatix"
## directly doesn't work. You have to assign it some other
## name, then use that to get to the "get" and other 
## sub-functions in the list created by makeCacheMatrix.  
## Example: source the functions: source(makeCacheMatrix),
## source(cacheSolve); create a matrix to invert: 
## x <- cbind(c(2,1),c(-3,4)); give makeCacheMatrix a different ## name, such as "M": M <- makeCacheMatrix(); store, or set, the ## matrix: M$set(x); then invert it: cacheSolve(M). 
## (3) Though this is a useful exercise for learning R, I 
## don't see why it might be useful in a real program. 
## Why not just invert the matrix at the outset:
## Inv <- solve(x), where "Inv" is a global variable 
## always available when you need it?

## makeCacheMatrix is not an ordinary function you can use
## directly. It creates 4 "subfunctions", the things you really
## want to use. But the only way I could find to get to those
## subfunctions was, basically, to re-name makeCacheMatrix:
## M <- makeCacheMatrix(), then use "M" to access the
## subfunctions: M$set(x), etc. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, 
  getInverse = getInverse)
}

## cacheSolve inverts the matrix the first time it is called, 
## then stores that inverse (in makeCacheMatrix) for future
## recalls. Worth noting: the "return()" function not only
## returns its argument, it also exits cacheSolve: the code
## after return() is (apparently) not executed. Is that always 
## true? If you have an explicit return() somewhere in a
## funtion, does that always abort execution of any following 
## code? 

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse        
}
