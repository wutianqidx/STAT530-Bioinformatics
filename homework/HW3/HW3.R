## Problem 1
10
5 + 3 + 4
9 - 1
6 / 2
(2 * 4) + (4 - 6)
a <- 3
b <- a + 1
a + b + (a * b)
a <- b
if ((b > a) && (b < (a * b))) {
  b
} else {
  a
}
if (a == 4) {
  6
} else if (b == 4) {
  6 + 7 + a
} else {
  25
}

## Problem 2
f <- function(x,y,z){
  if (x<=y && x<=z) return (y*y+z*z)
  if (y<=x && y<=z) return (x*x+z*z)
  else return (x*x+y*y)
}


## Problem 3

sqrt_iter <- function(x,guess=1){
  guess = abs(guess)
  diff = abs(guess^2-x)
  if (diff<0.001) return (guess)
  sqrt_iter(x,(x/guess+guess)/2)
}
