library("gtools")
library("tidyverse")
library("dslabs")
library("dplyr")
library("ggplot2")
setwd("C:/Users/Johannes/Documents/Johannes/Data Science")

# quiz
# p that ball is cyan
x <- rep(c("cyan", "magenta", "yellow"), times=c(3, 5, 7))
x
mean(x=="cyan") #p that draw is cyan

#sampling without replacement
outcomes <- replicate(2, sample(x, 1)) #draw 1, 2 times
outcomes
tab <- table(outcomes)
prop.table(tab)
mean(outcomes=="cyan")
p_nc <- 12/14 #p that second draw not cyan
p_nc_c <- 0.2*p_nc #p that second draw is not cyan if first is
p_nc_c
?sample
#sampling with replacement
p_nc_wr <- 12/15 #p not cyan with rep
p_nc_c_wr <- 0.2*p_nc_wr
p_nc_c_wr

#assesment 1/olympics
#3 medals across 8 runners
medals <- str(permutations(8, 3)) #medals across 8 runners
str(permutations(3, 3)) #distribute medals across 3 jamaicans

#p that all medals jamaica
mean(combinations(8, 3))
1/56

#monte carlo all medals jamaica
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
N <- 10000
set.seed(1)
medal <- replicate(N, {
  win <- sample(runners, 3)
  (win[1] %in% "Jamaica" & win[2] %in% "Jamaica" & win[3] %in% "Jamaica")
})
p <- mean(medal)

#assesment 2/restaurant
#different menu constellations (2 and 3 drinks/more sides)
side <- nrow(combinations(6, 3))
entree <- nrow(combinations(6, 1))
drink <- nrow(combinations(3, 1))

constellation <- side*drink*entree
constellation

#menu options with more entrees
n <- seq(1, 12, 1)
menu <- function(n){
  side <- nrow(combinations(6, 2))
  drink <- nrow(combinations(3, 1))
  entree <- nrow(combinations(n, 1))
  options <- (side*drink*entree)>365
}
a <- sapply(n, menu)
min(which(a==TRUE))

#constellation n side choices
m <- seq(2, 12, 1)
const <- function(m){
  entree2 <- nrow(combinations(6, 1))
  drink2 <- nrow(combinations(3, 1))
  sides2 <- nrow(combinations(m, 2))
  options2 <- (entree2*sides2*drink2)>365
}
b <- sapply(m, const)
min(which(b==TRUE))
b

#assesment 3/alcohol & tabacco use
data("esoph")
str(esoph) #as data
nrow(esoph) #number of groups
all_cases <- sum(esoph$ncases) #number of cases
all_controls <- sum(esoph$ncontrols) #number of controls
#patients in alc top segment
which.max(esoph$alcgp) #maximum alcohol cons group
alc_h <- filter(esoph, esoph$alcgp=="120+")
sum(alc_h$ncases)
sum(alc_h$ncontrols)
45+67
45/112 #p that sunbject is patient

#patients in lowest alc segment
which.min(esoph$alcgp)
alc_l <- filter(esoph, esoph$alcgp=="0-39g/day")
sum(alc_l$ncases)
sum(alc_l$ncontrols)
29+415
29/444
esoph %>% filter(alcgp == "0-39g/day") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols)+sum(ncases), probability=sum_cases/tot)

#p that case smokes >= 10g
c <- esoph %>% filter(ncases>0) %>% filter(tobgp != "0-9g/day")
p <- summarize(tot_c = sum(c$ncases), total = sum(c$ncases) + sum(c$ncontrols), 
               prob = tot_c / total)


sum(esoph$ncases)
sum(c$ncontrols)
?summarize
122 + 333
122/200

#p that control smokes >= 10g
esoph %>% summarize(total=sum(ncontrols)) %>% filter(tobgp != "0-9g/day") %>% 
  summarize(s_con=sum(ncontrols))
  
summarize(total=sum(esoph$ncontrols), tot_c_s=sum(s$ncontrols), 
          p=tot_c_s/total)

sum(s$ncontrols) / sum(esoph$ncontrols)

#assesment 4/ alcohol & tabacco use 2
esoph %>% summarise(total=sum(ncases)) %>% filter(alcgp=="120+") %>% 
  summarise(a_c=sum(ncases))
45/200 # p that case is in high alcohol group

#p that case in high tabacco group
which.max(esoph$tobgp)
esoph %>% filter(tobgp=="30+") %>% summarise(sum(ncases))
sum(esoph$ncases)
31/200

#p that case in highest tabacco and alcohol group
esoph %>% filter(alcgp=="120+") %>% filter(tobgp=="30+") %>%
  summarise(sum(ncases))
10/200

#p that case in highest tabacco or alcohol group
esoph %>% filter(alcgp=="120+"|tobgp=="30+") %>% 
  summarise(tot=sum(ncases), p=tot/200)

#p that control is in high alcohol group
esoph %>% filter(alcgp=="120+") %>% 
  summarise(total=sum(ncontrols), p=total/all_controls)

#cases more likely than controls in high alcohol
esoph %>% filter(alcgp=="120+") %>% 
  summarise(total=sum(ncases), p=total/all_cases)
0.225 / 0.06871795

#p that control in highest tabacco group
esoph %>% filter(tobgp=="30+") %>% 
  summarise(tot=sum(ncontrols), p=tot/all_controls)

#p that control in highest tabacco and alcohol group
esoph %>% filter(tobgp=="30+" & alcgp=="120+") %>% 
  summarise(tot=sum(ncontrols), p=tot/all_controls)

#p that case in highest tabacco or alcohol group
esoph %>% filter(tobgp=="30+" | alcgp=="120+") %>% 
  summarise(tot=sum(ncontrols), p=tot/all_controls)

#cases more likely in highest tabacco or alcohol group
esoph %>% filter(alcgp=="120+" | tobgp=="30+") %>%
  summarise(tot=sum(ncases), p=tot/all_cases, tot_con=sum(ncontrols),
            p_con=tot_con/all_controls, ratio=p/p_con)
B <- 10000
set.seed(1)
celtic_wins <- replicate(B, {
  simulated_games <- sample(c("win", "lose"), 4 , replace=TRUE, prob=c(0.4, 0.6))
  any(simulated_games=="win")>=1
})
mean(celtic_wins)
celtic_wins
!any(celtic_wins)

#assesment 5/continuous prob/ACT scores
#Set the seed to 16, then use rnorm() 
#to generate a normal distribution of 10000 tests with a 
#mean of 20.9 and standard deviation of 5.7. 
#Save these values as act_scores. You'll be using this dataset 
#throughout these four multi-part questions.
set.seed(16)
m <- 20.9
s <- 5.7
act_scores <- rnorm(10000, m, s)
mean(act_scores)
sd(act_scores)
sum(act_scores>=36) #perfect scores
sum(act_scores>=30)/length(act_scores) #p score >=30
sum(act_scores<=10)/length(act_scores) #p score <=10
#Set x equal to the sequence of integers 1 to 36. Use dnorm to 
#determine the value of the probability density function over x 
#given a mean of 20.9 and standard deviation of 5.7; save the result
#as f_x. Plot x against f_x.

x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x, type = "l")

#assesment 5/ACT scores to Z-scores
z_scores <- (act_scores-mean(act_scores))/sd(act_scores)
abs(sum(z_scores>=2)/length(z_scores)) #p for score >=2

2*sd(act_scores)+mean(act_scores) #ACT score 2 sd above mean

qnorm(0.975, mean(act_scores), sd(act_scores))

#Write a function that takes a value and produces the probability of 
#an ACT score less than or equal to that value (the CDF). Apply this 
#function to the range 1 to 36.
n <- 1:36
cdf <- function(n) mean(act_scores <= n)
sapply(n, cdf)

#Use qnorm() to determine the expected 95th percentile, the value for
#which the probability of receiving that score or lower is 0.95, given 
#a mean score of 20.9 and standard deviation of 5.7.
#What is the expected 95th percentile of ACT scores?

qnorm(0.95, 20.9, 5.7)

#Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), 
#the 1st through 99th percentiles of the act_scores data. Save these as
#sample_quantiles

sample_quantiles <- seq(0.01, 0.99, 0.01)
quantile(act_scores, sample_quantiles)>=26 #quantile for score 26

#Make a corresponding set of theoretical quantiles using qnorm() over 
#the interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard 
#deviation 5.7. Save these as theoretical_quantiles. Make a QQ-plot 
#graphing sample_quantiles on the y-axis versus theoretical_quantiles 
#on the x-axis.
t_q <- qnorm(sample_quantiles, 20.9, 5.7)
qplot(sample_quantiles, t_q) + geom_abline()

#for fun
c <- rep(c("black", "red", "green"), times=c(18, 18, 2))
n_b <- seq(1, 36, 2)
n_r <- seq(2,36, 2)
n_g <- c("0", "00")
n <- c(n_b, n_r, n_g)
roulette <- cbind(c, n)
sum(sample(roulette, 40)=="black")
sum(roulette=="black")

#assesment 6/ SAT testing
#An old version of the SAT college entrance exam had a -0.25 point 
#penalty for every incorrect answer and awarded 1 point for a correct 
#answer. The quantitative test consisted of 44 multiple-choice questions
#each with 5 answer choices. Suppose a student chooses answers by 
#guessing for all questions on the test.

p <- 1/5 #p for guessing right on 1 q
p+(1-p)*-.25 #expected value for guessing on one q
avg <- 44*(p+(1-p)*-.25) #ev for guessing on all questions
se <- sqrt(44)*(abs(-.25-1)*sqrt(p*(1-p))) #se for guessing on all q
1-pnorm(8, avg, se) #CLT p that score is >= 8
#p that guessing results in score 8 or higher
B <- 10000
set.seed(21)
SAT <- replicate(B, {
  score <- sample(c(1, -.25), 44, replace=T, prob = c(p, 1-p))
  sum(score)
})
mean(SAT>=8)

#reduce to 4 possibilities, no penalty
p2 <- 1/4
p2*44 #ev new test

#Consider a range of correct answer probabilities 
#p <- seq(0.25, 0.95, 0.05) representing a range of student skills.
#What is the lowest p such that the probability of scoring over 35 
#exceeds 80%?

p3 <- seq(.25, .95, .05)

SAT2 <- function(p){
  m <- 44*p
  s <- sqrt(44)*(abs(0-1)*sqrt(p*(1-p)))
  1-pnorm(35, m, s)               
}

simulated_test <- sapply(p3, SAT2)
simulated_test>.8
p3[13]

#assesment 7/Roulette
#A casino offers a House Special bet on roulette, which is a bet on 
#five pockets (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays 
#out 6 to 1. In other words, a losing bet yields -$1 and a successful
#bet yields $6. A gambler wants to know the chance of losing money if 
#he places 500 bets on the roulette House Special.

p <- 5/38
avg <- p*6 - 1*(1-p) #ev one bet
sd <- abs(-1-6)*sqrt(p*(1-p)) #se one bet
#expected value of avg payout 500 games
r <- replicate(500, {
  rg <- sample(c(6, -1), 500, replace = T, prob = c(p, 1-p))
  mean(rg)
})
mean(r)
sd(r) #standard error of average payout 500 games

avg500 <- 500*avg #ev 500 bets
sd500 <- sqrt(500)*sd
pnorm(0, avg500, sd500)


#assesment 8/The Big Short
# The death_prob data frame contains information about the estimated 
#probability of death within 1 year (prob) for different ages and sexes.
# Use death_prob to determine the death probability of a 50 year old 
#female, p.



data("death_prob")
head(death_prob)
p <- death_prob %>% filter(sex=="Female") %>% filter(age==50) %>% pull(prob)

# The loss in the event of the policy holder's death is -$150,000 and 
#the gain if the policy holder remains alive is the premium $1,150.
# What is the expected value of the company's net profit on one policy 
#for a 50 year old female?

ev <- p*-150000 + (1-p)*1150

# Calculate the standard error of the profit on one policy for a 
#50 year old female.

sd <- abs(1150+150000)*sqrt(p*(1-p))

# What is the expected value of the company's profit over all 1,000
#policies for 50 year old females?

ev1000 <- 1000*ev

# What is the standard error of the sum of the expected value over all
#1,000 policies for 50 year old females?

sd1000 <- sqrt(1000)*sd

# Use the Central Limit Theorem to calculate the probability that the 
#insurance company loses money on this set of 1,000 policies.

prob <- pnorm(0, ev1000, sd1000)

# Use death_prob to determine the probability of death within one year 
#for a 50 year old male.

pm <- death_prob %>% filter(sex=="Male", age==50) %>% pull(prob)

# Suppose the company wants its expected profits from 1,000 50 year old
#males with $150,000 life insurance policies to be $700,000. Use the 
#formula for expected value of the sum of draws with the following 
#values and solve for the premium b:
# where   E[S] = mu_S = 700000
#         n = 1000
#         p = death probability of 50 year old males
#         a = 150000 loss
#         b = premium to solve
# E[S] = n * (ap + b(1-p))
# --> b = ((E[S]/n) - ap)/(1-p)

mu_S <- 700000
n <- 1000
a <- -150000
b <- ((mu_S/n)-a*p)/(1-p)

# Using the new 50 year old male premium rate, calculate the standard 
#error of the sum of 1,000 premiums.

sd_m <- sqrt(1000)*(abs(b+150000)*sqrt(p*(1-p)))

# What is the probability of losing money on a series of 1,000 policies 
#to 50 year old males? Use the Central Limit Theorem.

probm <- pnorm(0, mu_S, sd_m)

# a lethal pandemic disease increases the probability of death within 
#1 year for a 50 year old to .015. Unable to predict the outbreak, the 
#company has sold 1,000 $150,000 life insurance policies for $1,150.
# What is the expected value of the company's profits over 1,000 
#policies?

pn <- .015
evn <- n*(pn*a+(1-pn)*1150)

# What is the standard error of the expected value of the company's 
#profits over 1,000 policies?

sdn <- sqrt(1000)*(abs(a-1150)*sqrt(pn*(1-pn)))

# What is the probability of the company losing money?

pnorm(0, evn, sdn)

# Suppose the company can afford to sustain one-time losses of 
#$1 million, but larger losses will force it to go out of business. 
#What is the probability of losing more than $1 million?

pnorm(-10^6, evn, sdn)

# Investigate death probabilities p <- seq(.01, .03, .001). What is the 
#lowest death probability for which the chance of losing money exceeds 
#90%?

pe <- seq(.01, .03, .001)
e <- function(p){
  ev <- n*(p*a + (1-p)*1150)
  sd <- sqrt(n)*(abs(1150-a)*sqrt(p*(1-p)))
  pnorm(0, ev, sd)
}
results <- sapply(pe, e)
results
pe[4]

# Investigate death probabilities p <- seq(.01, .03, .0025). What is 
#the lowest death probability for which the chance of losing over 
#$1 million exceeds 90%?

pe2 <- seq(.01, .03, .0025)
e2 <- function(p){
  ev <- n*(p*a + (1-p)*1150)
  sd <- sqrt(n)*(abs(1150-a)*sqrt(p*(1-p)))
  pnorm(-10^6, ev, sd)
}
exp <- sapply(pe2, e2)
exp
pe2[5]

#Define a sampling model for simulating the total profit over 1,000 
#loans with probability of claim p_loss = .015, loss of -$150,000 on a 
#claim, and profit of $1,150 when there is no claim. Set the seed to 25,
#then run the model once. What is the reported profit (or loss) in 
#millions (that is, divided by 10^6)?
b <- 1150
set.seed(25)
p_loss <- .015
samp <- sample(c(0, 1), n, replace = T, prob=c(p_loss, 1-p_loss))
earn <- sum(samp==1)*b
lose <- sum(samp==0)*a
profit <- (1/10^6)*(earn + lose)

#course
X <- sample(c(0,1), n, replace=TRUE, prob=c((1-p_loss),p_loss))
loss <- -150000*sum(X==1)/10^6 # in millions
profit <- 1150*sum(X==0)/10^6
loss+profit

# Set the seed to 27, then run a Monte Carlo simulation of your sampling
#model with 10,000 replicates to simulate the range of profits/losses 
#over 1,000 loans. What is the observed probability of losing $1 
#million or more?

set.seed(27)
M <- replicate(10000, {
  samp <- sample(c(0, 1), n, replace = T, prob=c((1-p_loss), p_loss))
  loss <- a*sum(samp==1)/10^6
  win <- b*sum(samp==0)/10^6
  loss+win
})
sum(M<=-1)/10000
 
# Suppose that there is a massive demand for life insurance due to the 
#pandemic, and the company wants to find a premium cost for which the 
#probability of losing money is under 5%, assuming the death rate stays
#stable at p = 0.015. Calculate the premium required for a 5% chance of
#losing money given n = 1000 loans probability of death p = 0.015, and 
#loss per claim l=-150000. Save this premium as x for use in further 
#questions.

p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))

#What is the expected profit per policy at this rate?

p*l + (1-p)*x

# What is the expected profit over 1,000 policies?

n*(p*l + (1-p)*x)

# Run a Monte Carlo simulation with B=10000 to determine the probability
# of losing money on 1,000 policies given the new premium x, loss on a 
# claim of $150,000, and probability of claim p=0.015. Set the seed to 
# 28 before running your simulation.
# What is the probability of losing money here?

set.seed(28)
B <- 10000
mc <- replicate(B, {
  earn <- sample(c(l, x), n, replace = T, prob = c(p, (1-p)))
  profit <- sum(earn)
})
mean(mc<0)
#or
alt <- replicate(B, {
  e <- sample(c(0, 1), n, replace=T, prob = c(p, (1-p)))
  lose <- (sum(e==0)*l)/10^6
  earn <- (sum(e==1)*x)/10^6
  lose + earn
})
mean(alt<0)
sum(alt<0)/10000

# The company cannot predict whether the pandemic death rate will stay
# stable. Set the seed to 29, then write a Monte Carlo simulation that
#for each of B=10000 iterations:
#  - randomly changes p by adding a value between -0.01 and 0.01 with
#    sample(seq(-0.01, 0.01, length = 100), 1)
#  - uses the new random p to generate a sample of n=1000 policies with premium x
#    and loss per claim l=-150000
#  - returns the profit over n policies (sum of random variable)
# The outcome should be a vector of B total profits

set.seed(29)

new_p <- p + sample(seq(-.01, .01, length = 100), 1)

outcome <- function(new_p){
  replicate(B, {
    samp <- sample(c(0, 1), n, replace = T, prob = c(new_p, (1-new_p)))
    earn <- sum(samp==0)*l/10^6
    lose <- sum(samp==1)*x/10^6
    earn + lose
  })
}
results <- sapply(new_p, outcome)

X <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length=100), 1)
  Y <- sample(c(x, l), n, replace=TRUE, prob=c(1-new_p, new_p))
  sum(Y)
})
sum(X)

# What is the expected value over 1,000 policies?

mean(X)

# What is the probability of losing money?

sum(X<0)/B

# probability of losing more than one million dollars?

mean(X< -10^6)
