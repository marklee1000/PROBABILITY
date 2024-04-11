MAYOL, JOSE RAPHAEL J.-FA8
================
2024-04-11

# Question 1

An analogue signal received at a detector, measured in microvolts, is
normally distributed with mean of 200 and variance of 256.

a.) What is the probability that the signal will exceed 224 𝜇V?

b.) What is the probability that it will be between 186 and 224 𝜇V?

c.) What is the micro voltage below which 25% of the signals will be?

d.) What is the probability that the signal will be less than 240 𝜇V,
given that it is larger than 210 𝜇V?

e.) Estimate the interquartile range.

f.) What is the probability that the signal will be less than 220 𝜇V,
given that it is larger than 210 𝜇V?

g.) If we know that a received signal is greater that 200 𝜇V, what is
the probability that is in fact greater than 220 𝜇V?

------------------------------------------------------------------------

For both Questions 1 & 2, pnorm() was used for estimating the
probability while qnorm() was used for approximating the data value.

For **(a)**, we will first need to set up our given. In this case, the
mean is 200 and the variance is 256. However, we need the standard
deviation. To obtain the standard deviation, we will get the square root
of our variance which will result to a value of 16. Finally, we must
find **P(X \> 224)**.

``` r
sdev1 <- sqrt(256)
# 1-a
prob_above_224 <- 1 - pnorm(224,200,sdev1)
cat("The probability that the signal will exceed 224 𝜇V is",prob_above_224)
```

    ## The probability that the signal will exceed 224 𝜇V is 0.0668072

For **(b)**, we must find **P(186 \< X \< 224)**.

``` r
# 1-b
prob_between_186_224 <- pnorm(224,200,sdev1) - pnorm(186,200,sdev1)
cat("The probability that the signal will be between 186 and 224 𝜇V is",prob_between_186_224)
```

    ## The probability that the signal will be between 186 and 224 𝜇V is 0.7424058

For **(c)**, we must find **X where P \< 0.25**.

``` r
# 1-c
microvoltage_25_quantile <- qnorm(0.25,200,sdev1)
cat("The micro voltage below which 25% of the signals will be is",microvoltage_25_quantile)
```

    ## The micro voltage below which 25% of the signals will be is 189.2082

For **(d)**, we need to use the Bayes’ Theorem, and as such we also need
to get the intersection of the two probabilities in question. In this
case, the *intersection* will be P(210 \< X \< 240). Finally, we must
find **P(X \< 240 \| X \> 210)**.

``` r
# 1-d
prob_above_210 <- 1 - pnorm(210,200,sdev1)
prob_below_240 <- pnorm(240,200,sdev1)
prob_below_210 <- pnorm(210,200,sdev1)
prob_between_210_240 <- prob_below_240 - prob_below_210
prob_below_240_given_above_210 <- prob_between_210_240 / prob_above_210
cat("The probability that the signal will be less than 240 𝜇V, given that
it is larger than 210 𝜇V is",prob_below_240_given_above_210)
```

    ## The probability that the signal will be less than 240 𝜇V, given that
    ## it is larger than 210 𝜇V is 0.9766541

For **(e)**, we must find **the range of X where 0.25 \< X \< 0.75**.

``` r
# 1-e
microvoltage_75_quantile <- qnorm(0.75,200,16)
cat("The interquartile range is (",microvoltage_25_quantile,",",microvoltage_75_quantile,")")
```

    ## The interquartile range is ( 189.2082 , 210.7918 )

For **(f)**, we also need to get the intersection of the two involved
probabilities so that we can use Bayes’ Theorem. In this case, the
*intersection* will be P(210 \< X \< 220). Finally, we must get **P(X \<
220 \| X \> 210)**.

``` r
# 1-f
prob_below_220 <- pnorm(220,200,sdev1)
prob_between_210_220 <- prob_below_220 - prob_below_210
prob_below_220_given_above_210 <- prob_between_210_220 / prob_above_210
cat("The probability that the signal will be less than 220 𝜇V, given that
it is larger than 210 𝜇V is",prob_below_220_given_above_210)
```

    ## The probability that the signal will be less than 220 𝜇V, given that
    ## it is larger than 210 𝜇V is 0.6027988

For **(g)**, we need to use Bayes’ Theorem. The *intersection* in this
case will simply be P(X \> 220). Finally, we must obtain **P(X \> 220 \|
X \> 200)**.

``` r
# 1-g
prob_above_200 <- 1 - pnorm(200,200,sdev1)
prob_above_220 <- 1 - pnorm(220,200,sdev1)
prob_above_220_given_above_200 <- prob_above_220 / prob_above_200
cat("The probability that the signal will be greater than 220 𝜇V, given that
it is greater that 200 𝜇V is",prob_above_220_given_above_200)
```

    ## The probability that the signal will be greater than 220 𝜇V, given that
    ## it is greater that 200 𝜇V is 0.2112995

# Question 2

A manufacturer of a particular type of computer system is interested in
improving its customer support services. As a first step, its marketing
department has been charged with the responsibility of summarizing the
extent of customer problems in terms of system failures. Over a period
of six months, customers were surveyed and the amount of downtime (in
minutes) due to system failures they had experienced during the previous
month was collected. The average downtime was found to be 25 minutes and
a variance of 144. If it can be assumed that downtime is normally
distributed:

a.) obtain bounds which will include 95% of the downtime of all the
customers;

b.) obtain the bound above which 10% of the downtime is included.

------------------------------------------------------------------------

For **(a)**, the bounds which will include 95% of the downtime of all
the customers can be obtained by getting **the range of X where 0.025 \<
P \< 0.975**.

``` r
# 2-a
sdev2 <- sqrt(144)
lower_bound_95 <- qnorm(0.5-(0.95/2),25,sdev2)
upper_bound_95 <- qnorm(0.5+(0.95/2),25,sdev2)
cat("Bounds for 95% of all downtime: (",lower_bound_95,",",upper_bound_95,")")
```

    ## Bounds for 95% of all downtime: ( 1.480432 , 48.51957 )

For **(b)**, we need to get the highest 10% of the downtime. In this
case, this is also **the range of X where 0.9 \< P \< 1**. Since the
normal distribution is continuous, then it is expected that the highest
bound, where P = 1, is infinite.

``` r
# 2-b
lower_bound_10 <- qnorm(0.9,25,sdev2)
upper_bound_10 <- qnorm(1,25,sdev2)
cat("Bound above which 10% of the downtime is included: (",lower_bound_10,",",upper_bound_10,")")
```

    ## Bound above which 10% of the downtime is included: ( 40.37862 , Inf )
