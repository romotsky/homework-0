## brexit analysis
update.packages("dslabs")
update.packages()

# suggested libraries and options
library(tidyverse)
options(digits = 3)
# load brexit_polls object
library(dslabs)
data(brexit_polls)
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

N <- 1500
N * p
se <- sqrt(p*(1-p)/N)
X <- N * p
X_se <- N * sqrt(p*(1-p)/N)
X / N
X_bar <- (d+1)/2
d_se <- 2 * se
####
data(brexit_polls)
head(brexit_polls)
brexit_polls %>% summarize(mean(spread))
brexit_polls %>% summarize(sd(spread))
brexit_polls %>%
  mutate(x_hat = (spread + 1)/2 ) %>% summarize(mean = mean(x_hat), sd  = sd(x_hat))
####
brexit_polls[1,]
ci <- brexit_polls[1,] %>% mutate(se = sqrt(remain*(1-remain)/samplesize), lower = remain - se * qnorm(.975), upper = remain + se * qnorm(.975))
ci
###
june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01")
june_polls %>% summarize(n = n())
june_polls %>% mutate(se = 2 * sqrt(x_hat*(1-x_hat)/samplesize), lower = spread - se * qnorm(.975), upper = spread + se * qnorm(.975),
                      hits = lower <=0 & upper >= 0, wins = lower > 0, d_hits = lower <=d & upper >= d  ) %>% summarize(cover = mean(hits), win = mean(wins), in_d = mean(d_hits))
####
june_polls %>% mutate(se = 2 * sqrt(remain*(1-remain)/samplesize), lower = spread - se * qnorm(.975), upper = spread + se * qnorm(.975))  %>% 
  mutate(hits = lower <= d & upper >= d) %>% group_by(pollster) %>% summarise(h = mean(hits), n = n()) %>% arrange(desc(h))
####
june_polls %>% ggplot(aes(poll_type, spread)) + geom_boxplot()
####
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2, 
            se = 2* sqrt(p_hat*(1-p_hat)/N),
            lower = spread - se *qnorm(0.975),
            upper = spread + se *qnorm(0.975))
####
# suggested libraries and options
library(tidyverse)
options(digits = 3)
# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)
# final proportion voting "Remain"
p <- 0.481

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_hit %>% group_by(poll_type) %>% summarise(hits_t = sum(hit), hits_f = sum(!hit)) %>%
  select(-poll_type) %>% chisq.test()
####
twobytwo <- brexit_hit %>% group_by(poll_type) %>% summarise(hits_t = sum(hit), hits_f = sum(!hit))
odd_o <- twobytwo[[1,2]]/twobytwo[[1,3]]
odd_t <- twobytwo[[2,2]]/twobytwo[[2,3]]
####
brexit_polls %>%
  ggplot(aes(enddate, spread, color = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -.038))
####
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
head(brexit_long)
brexit_long %>% ggplot(aes(x=enddate, y=proportion, color = vote)) +
  geom_smooth(method = "loess",  span = 0.3) +
  geom_point()

#scratch
z_s <- seq(-5,5,by = .1)
z_s

dvalues <- dnorm(z_s)
dvalues
plot(z_s, dvalues)

dnorm(0)
dnorm(-5)
pnorm(z_s)

plot(z_s, pnorm(z_s))

qnorm(pnorm(z_s))

x <- c(2,3,4,NA)
!is.na(x)
library(tidyverse)
filter(is.na(x))
replicate()
sample()


Pr_a <- .2
pr_B<- .6
Pr_c <- .15
Pr_d <- .05
Pr_no <- 0.1

options(digits = 3)
# prob not found (0.1) given it's in B
Pr_no * Pr_b


# use maximum posterior probability from previous question: region A
pr_A <- 0.2
pr_missed <- 0.1    # prob plane in B but not found
pr_not_found_in_B <-  1 - pr_B + pr_B*pr_missed
pr_missed_not_present <- 1
pr_A_post <- pr_A * pr_missed_not_present / pr_not_found

# probability plane found on day 2
pr_found_in_A <- pr_A_post*(1-pr_missed)

# probability plane not found on day 1 but found on day 2
pr_not_found_in_B * pr_found_in_A
