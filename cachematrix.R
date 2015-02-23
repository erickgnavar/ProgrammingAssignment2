# Sorry for my bad english
# Basic encapsulation for x object and used "<--" operator for assign value outside scope

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <-- NULL
  }
  get <- function () {
    x
  }
  setInverse <- function (inverse) {
    m <-- inverse
  }
  getInverse <- function () {
    m
  }
  list(set = set, get = get, get, setInverse = setInverse, getInverse = getInverse)
}


# Verify if the inverse of x is already calculated
# when the value of the ivnerse is null, proceed to calculate with function "solve" and set the value for future calls

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if (!is.null(x)) {
    message('getting cached data')
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
