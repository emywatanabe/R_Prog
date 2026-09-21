matriz <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow =4, ncol =2, byrow = TRUE)
matriz[3, 1]

matriz <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow =4, ncol =2)
matriz[3, 1]

val1 <- 10L 
val2 <- 10
is.integer(val2)
is.double(val2)

x <- 10 %% 3
y <- 10 %/% 3
x
y

10 + NULL

seq(from = 1, by = 2, length.out = 5)

rnorm(n = 10, mean = 1.80, sd = 0.1)
pnorm(q = 10, mean = 1.80, sd = 0.1)
sample(1.80, 0.1, size = 10)
dnorm(x = 10, mean = 1.80, sd = 0.1)
man(women)
women::help()
women
browse(women)
help(summary)
help("women")
?women

x <- c(1,3,5)
y <- c(3,7,11)
qqplot(x,y)

vetor <- c(10L, "Data Science", TRUE)
class(vetor)

raw_sensor <- c(12.4, NA, 45.1, 0/0, 88.3, 105.0, -Inf, 34.2, 55.6, NA)
median(raw_sensor)

A <- matrix(rep(c(0, 1, 0, 1, 0), times = 5), nrow = 5, ncol = 5, byrow = TRUE)
diag(A) <- 0
conexoes <- which(A == 1, arr.ind = TRUE)
conexoes

A <- matrix(rep(c(0, 1, 0, 1, 0), times = 5), nrow = 5, ncol = 5, byrow = TRUE)
diag(A) <- 0
conexoes <- which(A == 1, arr.ind = TRUE)
conexoes

f <- factor(c("alta", "baixa", "media"), levels = c("baixa", "media", "alta"), ordered = TRUE)
is.factor(f)
is.ordered(f)

x <- x^2
deriv(x)
?save

total <-1 
for (i in 1:4) 
  { total <- total * i }
total

x <- 1
while (TRUE)
{if (x > 3) break
  x <- x +1}
x

x <- list(a = 1:3, b = letters[1:3])
x$b[1] 
x$b[[1]] 
x[[b]][1]
x[["b"]][1]

rep(2^(1:1024), 10) 
rep(2, 10) 
2^(1:10) 

airquality
airquality[!is.na(airquality$Ozone), ]
subset(airquality, is.na(Ozone)) 
airquality[which(Ozone != NA), ]
airquality[, !is.na(airquality$Ozone)]
airquality[is.na(airquality$Ozone), ]

?library
?require
m <- matrix(1:9, 3, 3)
any(m>0)
all(m>0)
m>0
all(m)

x <- 1 
repeat {x <- x * 2 
if (x> 10) break}
x

rep(1:3, each=2)

soma <- 0
for (i in 1:5) {
  if  (i %% 2 == 0) next
  soma <- soma + i}
soma

s <- 0 
for (i in seq(2, 10, 2)) 
{ s <- s + i } 
s

x <- seq(2,20,by=2)
x[x%%2==1]
x[seq(1,length(x),by=2)]
x[seq(2,length(x),by=2)] 
x[seq(0,length(x),by=2)] 

x <- 1
while (x<3)
{x <- x + 0.5}
x
m[[2,3]] 

vec <- c() 
for (i in 1:3) 
  { vec[i] <- i^2 } 
vec 

count <- 0 
for (i in 1:3) 
  { for (j in 1:2) 
    { count <- count + 1 } } 
count

rep  (7, times =10)

x <-0 
while (x <5)
{x <- x + 2 }
x
