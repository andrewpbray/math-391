
# Random Walk

library(scales)
steps <- 10000
rw <- sample(c(-1, 1), 10000, replace = TRUE)
plot(c(1, 1), type = "n", xlim = c(0, steps), ylim = c(-200, 200), xlab = "", ylab = "Position")
it <- 500
for(i in 1:it) {
  rw <- sample(c(-1, 1), 10000, replace = TRUE)
  lines(cumsum(rw), type = "l", col = alpha("gray", .3))
}

lines(seq(0, 10000, 10), sqrt(seq(0, 10000, 10)), col = "tomato")


# Markov Chain
Tmat <- matrix(c(.8, .6, .2, .4), ncol = 2)
mu <- c(.2, .8)
mu %*% Tmat

library(expm)
mu %*% (Tmat %^% 40000)


# Gobbledepoetry

p <- c(8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094, 6.966, 0.153, 0.772,
4.025, 2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 9.056, 2.758, 0.978, 
2.360, 0.150, 1.974, 0.074)



v_ind <- c(1, 5, 9, 15, 21)
vowels <- letters[v_ind]
consonants <- letters[-v_ind]

nchar <- 50
Tmat <- matrix(c(.31, .31, .69, .69), nrow = 2)
X <- rep("v", 50)
X[1] <- sample(c("v", "c"), 1, prob = c(.31, .69))

for(i in 2:nchar){
  X[i] <- ifelse(X[i - 1] == "v",
                 sample(c("v", "c"), 1, prob = Tmat[1,]),
                 sample(c("v", "c"), 1, prob = Tmat[2,]))
}

paste(X, collapse = "")

Tmat <- matrix(c(.175, .526, .825, .474), nrow = 2)



if(X == "v") {
  Y <- sample(vowels, 1, prob = p[v_ind])
} else {
  Y <- sample(consonants, 1, prob = p[-v_ind])
}


