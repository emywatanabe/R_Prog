#Elementos de programação P3

#Tutorial: Programação em R

#Exercício 1 – Sequência de Fibonacci Generalizada

#Considere a sequência definida por
#x1=2,x2=3
#e para n≥3,
#xn=xn−1+xn−2
#Com sua implementação em R responda:
#Qual é o valor de x20?

#Seja rn=xnxn−1
#Qual o menor valor de n para que
#|rn−rn−1|<10−7
#Para qual valor rn está convergindo? Reporte pelo menos 6 casas decimais.

x <- numeric(20)

x[1] <- 2
x[2] <- 3

for (i in 3:20){
  x[i] <- x[i-1] + x[i-2]
  }
x[20]

r <- x[-1]/x[-length(x)]

dif <- abs(diff(r))

which(dif < 1e-7)[1]
dif[16]

tail(r,1)

#Exercício 2 – Sequência de Tetranacci

n <- 30

x <- numeric(n)
x[1:4] <- 1

for (i in 5:n){
  x[i] <- (x[i-1]+x[i-2]+x[i-3]+x[i-4])
}

r <- x[-1]/x[-length(x)]
dif <- abs(diff(r))[5:length(r)]

which(dif < 1e-6)[1]

tail(r,1)

#Exercício 3 – Sequência Multiplicativa
x <- numeric(10)

x[1] <- 2
x[2] <- 3

for (i in 3:10){
  x[i] <- (x[i-1]*x[i-2])
}

y <- log(x)
r <- y[-1]/y[-length(y)]

tail(r,1)


