MAYOL, JOSE RAPHAEL J.-FA7
================
2024-02-29

# Question 1

In Example 16.3 with 𝜆 = 4 per minute, use R to obtain:

1)  P(T ≤ 0.25) = P(time between submissions is at most 15 seconds);
2)  P(T \> 0.5) = P(time between submissions is greater than 30
    seconds);
3)  P(0.25 \< T \< 1) = P(time between submissions is between 15 seconds
    and 1 minute)

Before we begin, we first need to set the parameter which is lambda (𝜆)
= 4 per minute.

``` r
lambda <- 4
```

For **letter (a)**, we can just use the integral from 0 to 0.25 of the
exponential cumulative distribution formula. In R, this can be done
quickly by the pexp() function. For **letter (b)**, we will get 1 then
subtract the integral from 0 to 0.5 of the formula. Lastly for **letter
(c)**, we will just take the integral from 0.25 to 1.

``` r
# (a) P(T <= 0.25)
prob_a <- pexp(0.25, rate = lambda)
# (b) P(T > 0.5)
prob_b <- 1 - pexp(0.5, rate = lambda)
# (c) P(0.25 < T < 1)
prob_c <- pexp(1, rate = lambda) - pexp(0.25, rate = lambda)

cat("(a) P(T <= 0.25) =", prob_a, "\n",
    "(b) P(T > 0.5) =", prob_b, "\n",
    "(c) P(0.25 < T < 1) =", prob_c, "\n"
    )
```

    ## (a) P(T <= 0.25) = 0.6321206 
    ##  (b) P(T > 0.5) = 0.1353353 
    ##  (c) P(0.25 < T < 1) = 0.3495638

As we can see, the probability for scenario (a) is around **63%**, then
**13.5%** for scenario (b), and lastly **34.96%** for scenario (c).

# Question 3

The average rate of job submissions in a computer center is 2 per
minute. If it can be assumed that the number of submissions per minute
has a Poisson distribution, calculate the probability that:

1)  more than two jobs will arrive in a minute;
2)  at least 30 seconds will elapse between any two jobs;
3)  less than 30 seconds will elapse between jobs;
4)  a job will arrive in the next 30 seconds, if no jobs have arrived in
    the last 30 seconds

Like Question 1, we will set again the parameter of lambda but this time
with a value of 2.

``` r
lambda3 <- 2
```

We will then use the pexp() function again for all probabilities in
Question 2 except for (a), where we will use ppois() since it is asking
about the number of events taking place within a time period.

``` r
# (a) Probability of more than two jobs arriving in a minute (P(X>2) or 1-P(X<=2))
prob_more_than_two <- 1 - ppois(2, lambda3)

# (b) Probability of at least 30 seconds between any two jobs (P(T>=0.5) or 1-P(X<0.5))
prob_at_least_30_sec <- 1 - pexp(0.5, rate = lambda3)

# (c) Probability of less than 30 seconds between jobs (P(X<0.5))
prob_less_than_30_sec <- pexp(0.5, rate = lambda3)

# (d) Probability of a job arriving in the next 30 seconds given no jobs in the last 30 seconds (P(X<0.5))
prob_job_in_next_30_sec <- pexp(0.5, rate = lambda3)

cat("Probability of more than two jobs arriving in a minute:", prob_more_than_two, "\n",
    "Probability of at least 30 seconds between any two jobs:", prob_at_least_30_sec, "\n",
    "Probability of less than 30 seconds between jobs:", prob_less_than_30_sec, "\n",
    "Probability of a job arriving in the next 30 seconds:", prob_job_in_next_30_sec, "\n")
```

    ## Probability of more than two jobs arriving in a minute: 0.3233236 
    ##  Probability of at least 30 seconds between any two jobs: 0.3678794 
    ##  Probability of less than 30 seconds between jobs: 0.6321206 
    ##  Probability of a job arriving in the next 30 seconds: 0.6321206

Hence, we now have the probabilities the different scenarios: **32.33%**
for (a), **36.79%** for (b), **63.21%** for (c), and also **63.21%** for
(d).

# Question 7

A website receives an average of 15 visits per hour, which arrive
following a Poisson distribution.

1)  Calculate the probability that at least 10 minutes will elapse
    without a visit.
2)  What is the probability that in any hour, there will be less than
    eight visits?
3)  Suppose that 15 minutes have elapsed since the last visit, what is
    the probability that a visit will occur in the next 15 minutes.
4)  Calculate the top quartile, and explain what it means.

First, we will set the parameters.

``` r
lambda7 <- 15  # Average visits per hour
```

To calculate the probabilities in **scenarios (a), (b), and (c)**, we
have to determine which function to use for each one. For (a), we have
to use pexp() since it is asking for the time between events. The same
could also be said for scenario (c), but not for (b) which is asking for
the number of events within a time period. In that case, we will use
ppois().

``` r
# (a) Probability that at least 10 minutes will elapse without a visit. (1-P(T<1/6))
prob_at_least_10_min_no_visit <- 1 - pexp(1/6, rate=lambda7)

# (b) Probability that in any hour, there will be less than eight visits (1-P(X<8))
prob_less_than_8_visits <- ppois(7, lambda7)

# (c) Probability that a visit will occur in the next 15 minutes supposing 15 minutes have elapsed since the last one ((P(X<15/60)))
prob_visit_in_next_15_min <- pexp(15/60, rate = lambda7)

cat("Probability of more than two jobs arriving in a minute:", prob_more_than_two, "\n",
    "Probability of at least 30 seconds between any two jobs:", prob_at_least_30_sec, "\n",
    "Probability of less than 30 seconds between jobs:", prob_less_than_30_sec, "\n",
    "Probability of a job arriving in the next 30 seconds:", prob_job_in_next_30_sec, "\n")
```

    ## Probability of more than two jobs arriving in a minute: 0.3233236 
    ##  Probability of at least 30 seconds between any two jobs: 0.3678794 
    ##  Probability of less than 30 seconds between jobs: 0.6321206 
    ##  Probability of a job arriving in the next 30 seconds: 0.6321206

For **(d)**, we will simply get the third quartile (Q3 or 75%) in Visits
Per Hour which we can simply do using the qpois() function.

``` r
top_quartile <- qpois(0.75, lambda7)
cat("Third Quartile (75%) of Visits Per Hour:", top_quartile, "\n")
```

    ## Third Quartile (75%) of Visits Per Hour: 18

From this information, we can deduce that in 75% of the time, the number
of visits in any given hour will not exceed **18**. This also means that
only 25% of all hours will have more than 18 visits.
