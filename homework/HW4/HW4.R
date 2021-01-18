make_rat <- function(num, denom) {
  rat <- list(n = num, d = denom)
  return(rat)
}
x <- make_rat(1, 2)

numer <- function(x) {
  return(x$n)
}

denom <- function(x) {
  return(x$d)
}

print_rat <- function(x) {
  cat(numer(x), "/", denom(x), "\n")
}

add_rat <- function(x, y) {
  n <- numer(x) * denom(y) + numer(y) * denom(x)
  d <- denom(x) * denom(y)
  return(make_rat(n, d))
}

sub_rat <- function(x, y) {
  n <- numer(x) * denom(y) - numer(y) * denom(x)
  d <- denom(x) * denom(y)
  return(make_rat(n, d))
}

x <- make_rat(1, 2)
y <- make_rat(2, 3)
z1 <- add_rat(x, y)
z2 <- sub_rat(x, y)
print_rat(z1)
print_rat(z2)

#Problem 1
## multiplication
mul_rat <- function(x,y){
  n <- numer(x) * numer(y)
  d <- denom(x) * denom(y)
  return(make_rat(n,d))
}

## division
div_rat <-function(x,y){
  n <- numer(x) * denom(y)
  d <- denom(x) * numer(y)
  return(make_rat(n,d))
}

## testing
equal_rat <- function(x,y){
  return(numer(x)*denom(y)==numer(y)*denom(x))
}

# Problem 2

f <-function(n){
  if (n < 3) return(n)
  f_prev = 2
  f_prev_prev = 1
  for (i in 3:n){
    curr = f_prev+2*f_prev_prev
    f_prev_prev = f_prev
    f_prev = curr
  }
  return(curr)
}

# Problem 3

GCD <- function(x,y){
  for (i in 1:min(x,y)){
    if(x%%i==0 & y%%i==0) gcd=i
  }
  return(gcd)
}

simplify_rat <- function(x){
  gcd = GCD(numer(x),denom(x))
  n = numer(x)/gcd
  d = denom(x)/gcd
  return(make_rat(n,d))
}

# Problem 4
## Section 3.2.5 Exercises 2

attach(iris)
iris=data.frame(iris)
NCOL(iris)
NCOL(iris[,0])

df_coltypes <- data.frame(
  a = c("one", "two"),
  b = c(TRUE, FALSE),
  c = c(1L, 0L),
  d = c(1.5, 2),
  e = c("one" = 1, "two" = 2),
  g = factor(c("f1", "f2")),
  stringsAsFactors = FALSE
)

df_coltypes

data.matrix(df_coltypes)
