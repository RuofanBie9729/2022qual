library(lme4)
##### part 1: simulation
set.seed(1)
J = 200
n = 2
Beta <- NULL
meanY <- NULL
intcpt <- NULL
for(i in 1:1000){
beta=NULL

id <- 1:(J*n)
id_shuffle <- sample(id, J*n)
center <- rep(0, J*n)
for(j in 1:J){
  center[id_shuffle[((j-1)*n+1):(j*n)]] <- j
}
alpha <- rnorm(J, 0, 2)
### design 1
X <- rep(0, J*n)
for(j in 1:J){
  #X[which(center==j)] <- rbinom(n, 1, 0.5)
  temp <- rep(0, n)
  temp[sample(n, n/2)] <- 1
  X[which(center==j)] <- temp
}
Y <- X
for(j in 1:J){
  Y[which(center==j)] <- rnorm(n, Y[which(center==j)]+ alpha[j], 1)
}
#Y <- Y + rnorm(J*n, 0, 1)
Data <- data.frame(id=id, center=center, X=X, Y=Y)
mod.re <- lmer(Y~X+(1|center), data=Data)
beta <- summary(mod.re)$coefficients[2,1:2]

mod.lm.adj <- lm(Y~X+as.factor(center), data=Data)
beta <- c(beta, summary(mod.lm.adj)$coefficients[2,1:2])

mod.lm <- lm(Y~X, data=Data)
beta <- c(beta, summary(mod.lm)$coefficients[2,1:2])
Beta <- rbind(Beta, beta)
meanY <- c(meanY, mean(Y))
intcpt <- c(intcpt, summary(mod.lm)$coefficients[1,1])
}

apply(Beta, 2, mean)
apply(Beta, 2, sd)


### design 2
set.seed(1)

Beta <- NULL
for(i in 1:1000){
beta=NULL
intcpt <- NULL

id <- 1:(J*n)
id_shuffle <- sample(id, J*n)
center <- rep(0, J*n)
for(j in 1:J){
  center[id_shuffle[((j-1)*n+1):(j*n)]] <- j
}
alpha <- rnorm(J, 0, 2)
X <- rbinom(J*n, 1, 0.5)
Y <- X
for(j in 1:J){
  Y[which(center==j)] <- rnorm(n, Y[which(center==j)]+ alpha[j], 1)
}
#Y <- Y + rnorm(J*n, 0, 1)
Data <- data.frame(id=id, center=center, X=X, Y=Y)
mod.re <- lmer(Y~X+(1|center), data=Data)
beta <- summary(mod.re)$coefficients[2,1:2]

mod.lm.adj <- lm(Y~X+as.factor(center), data=Data)
beta <- c(beta, summary(mod.lm.adj)$coefficients[2,1:2])

mod.lm <- lm(Y~X, data=Data)
beta <- c(beta, summary(mod.lm)$coefficients[2,1:2])
Beta <- rbind(Beta, beta)
intcpt <- c(intcpt, summary(mod.lm)$coefficients[1,1])
}
apply(Beta, 2, mean)
apply(Beta, 2, sd)




