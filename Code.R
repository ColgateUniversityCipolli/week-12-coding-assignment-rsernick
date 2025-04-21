library(tidyverse)

##########################################################################################
### 1. a. and b.
alpha = 0.05
n.20 = 20

t.20 = qt(1 - alpha, n.20-1)

n.30 = 40

t.30 = qt(1 - alpha, n.30-1)


##########################################################################################
### 1. c.


library(VGAM)

sims = 10000
err = 0
set.seed(7272)

for (i in 1:sims){
  lap = rlaplace(n = 30, location = 0, scale = 4)
  lap.20 = lap[1:20]
  t.sim.20 = mean(lap.20)/(sd(lap.20)/sqrt(n.20))
  t.sim.30 = mean(lap)/(sd(lap)/sqrt(n.30))
  
  if((t.sim.20 > t.20) & (t.sim.30 < t.30)){
    err = err + 1
  }
}

t1.err = err/sims


##########################################################################################
#### 1. d. OPTIONAL CHALLENGE



##########################################################################################
### 2. a.



n = 15
t.high = qt(1 - alpha, n-1)
t.low = qt(alpha, n-1)
t.2.side = c(qt(alpha/2, n-1), qt(1-(alpha/2), n-1))

simulation = function(a, b, test){
  mu = (a/(a+b))
  err = 0

  t = c()
  mean = c()
  
  for (i in 1:sims){
    set.seed(7272+i)
    samp = rbeta(n = n, shape1 = a, shape2 = b)
    mean[i] = mean(samp)
    
    t[i] = (mean[i]-mu)/(sd(samp)/sqrt(n))
    if (t[i] < t.low & test == 'left.sided'){
      err = err + 1
    }else if (t[i] > t.high & test == 'right.sided'){
      err = err + 1
    }else if (t[i] < t.2.side[1] | t[i] > t.2.side[2] & test == 'two.sided'){
      err = err + 1
    }
  }
  mu.xbar = mean(mean)
  return(list(err/sims, mu.xbar))
}

simulations = tibble(alpha = c(10, 2, 10),
                     beta = c(2, 10, 10),
                     mu = c(10/(10+2), 2/(10+2), 10/(10+10)),
                     mu.xbar = c(simulation(10, 2, "left.sided")[2],
                                 simulation(2, 10, "left.sided")[2],
                                 simulation(10, 10, "left.sided")[2]),
                     err.left.t = c(simulation(10, 2, "left.sided")[1],
                                simulation(2, 10, "left.sided")[1],
                                simulation(10, 10, "left.sided")[1]),
                     err.right.t = c(simulation(10, 2, "right.sided")[1],
                                 simulation(2, 10, "right.sided")[1],
                                 simulation(10, 10, "right.sided")[1]),
                     err.two.t = c(simulation(10, 2, "two.sided")[1],
                               simulation(2, 10, "two.sided")[1],
                               simulation(10, 10, "two.sided")[1]))


graph.dat = tibble(x = seq(0, 1, length.out = 10000))

graph.dat = graph.dat |>
  mutate(pdf1 = dbeta(x, shape1 = 10, shape2 = 2),
         pdf2 = dbeta(x, shape1 = 2, shape2 = 10),
         pdf3 = dbeta(x, shape1 = 10, shape2 = 10))
ggplot(data = graph.dat) +
  geom_line(aes(x = x, y = pdf1))+ 
  geom_area(data = subset(graph.dat, x <= qbeta(0.05, shape1 = 10, shape2 = 2)),
            aes(x = x, y = pdf1),
            fill = "red", alpha = 0.4) +
  geom_vline(xintercept = simulations$mu[1], color = "blue") +
  geom_hline(yintercept = 0) +
  theme_bw()

ggplot(data = graph.dat) +
  geom_line(aes(x = x, y = pdf2))+ 
  geom_vline(xintercept = simulations$mu[2], color = "blue") +
  theme_bw()

ggplot(data = graph.dat) +
  geom_line(aes(x = x, y = pdf3))+ 
  geom_vline(xintercept = simulations$mu[3], color = "blue") +
  theme_bw()
