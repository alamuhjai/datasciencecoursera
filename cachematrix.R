
###Intrsuctions:

#1.set the value of the matrix

#2.get the value of the matrix

#3.set the value of the inverse

#4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y)
  {
    x <<-  y
    m <<- NULL
  }
  get <- function() x
  set_inverse <-function(inverse) m<<-inverse
  get_inverse <- function() m
  list(set=set,
       get=get,
       set_inverse=set_inverse,
       get_inverse=get_inverse
  )

}


#cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
  
}
