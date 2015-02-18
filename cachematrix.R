## Deze functie berekend de inverse matrix van een matrix
## ik gebruik hiervoor de solve functie uit R
## de functie gebrukt een cache. Als de inverse functie al een is berekend
## dan wordt bij een volgende berekening de inverse functie uit de cache opgehaald
## tEST Cases
##
##     2 1 1                 3 -1 -1
## A = 3 2 1   INVERSE(A) = -4  2  1
##     2 1 2                -1  0  1
## m = matrix(c(2,3,2,1,2,1,1,1,2),nrow= 3, ncol = 3)

## Deze code is een bijna letterlijke vertaling van de code van de instructor.
## mean is vertaald in inverse

makeCacheMatrix <- function(x = matrix()) {
## creeer een lijst van functies voor het cachen van de inverse martix v/e matrix
 m <- NULL 
 set <- function(y) {
   x <<- y 
   ## caches de inputmatrix, cachesolve kan dan bepalen of het is veranderd 
   m <<- NULL
 }
 get <- function() x
 setInverse <- function(inverse) m <<- inverse 
 getInverse <- function() m
 list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Bereken de inverse van een matrix geretourneerd door makechachematrix()
## tenzij de inverse al een keer is berekend. Als dit geval is
## dan wordt de inverse uit de cache gehaald.
        ## Return a matrix that is the inverse of 'x'
          m <- x$getInverse() 
          #Als an inverse al ia berekend, pak deze op
          if (!is.null(m)) { 
            #Is cacheSolve al een keer gedraaid? 
            print("getting cache data")
            return(m)
          }
          #matrix <- x$get()
          m <- solve(x$get()) 
          ## bereken de inverse matrix van de inputmatrix
          x$setInverse(m) 
          ## run the setinverse functie op de inverse om deze te cachen
          m 
          ## retourneert m
}
