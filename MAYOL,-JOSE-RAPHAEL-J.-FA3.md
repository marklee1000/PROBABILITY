MAYOL, JOSE RAPHAEL J.-FA3
================
2024-02-25

# Question 2

A binary communication channel carries data as one of two sets of
signals denoted by 0 and 1. Owing to noise, a transmitted 0 is sometimes
received as a 1, and a transmitted 1 is sometimes received as a 0. For a
given channel, it can be assumed that a transmitted 0 is correctly
received with probability 0.95, and a transmitted 1 is correctly
received with probability 0.75. Also, 70% of all messages are
transmitted as a 0. If a signal is sent, determine the probability that:

a.) a 1 was received;<br> b.) a 1 was transmitted given that a 1 was
received.

<br><br> In the problem, there are two possible values that a single
datum can have: either 0 or 1. There is a catch however, a piece of data
that is transmitted as a certain value is not guaranteed to be received
the same! This means that a datum transmitted as a 1 has a chance to be
received as 0, and vice versa. <br>

**(a)** To calculate the probability that **a 1 was received**, we first
need to set up the given values. These are the probabilities that *a
transmitted 0 is correctly received* (95%), *a transmitted 1 is
correctly received* (75%), *a message is transmitted as a 0* (70%), and
*a message is transmitted as a 1* (30%). In R:

``` r
P_R0givenT0 <- 0.95
P_R1givenT1 <- 0.75
P_T0 <- 0.70
P_T1 <- 0.30
```

We can now get the probability that *a 1 was received* by using the *Law
of Total Probability*. In other words, we will add both *the product of
the probabilities that a 1 was received given a 1 was transmitted*
(P_R1givenT1) and *a message is transmitted as a 1* (P_T1) & also *the
product of the probability that a 1 was received given a 0 was
transmitted* (P_R1givenT0) *and a message is transmitted as a 0* (P_T0).

We already know almost all of these values, but we still do not have
*P_R1givenT0*. To get *P_R1givenT0*, we have to take the complement of
*P_R0givenT0* which we can simply get by subtracting *P_R0givenT0* from
1.

``` r
P_R1 <- (P_R1givenT1 * P_T1) + ((1 - P_R0givenT0) * P_T0)
cat("Probability that a 1 was received:",P_R1,"or",P_R1 * 100,"%\n")
```

    ## Probability that a 1 was received: 0.26 or 26 %

By the Law of Total Probability, we have already obtained our answer for
(a). The probability that a message was received as 1 is **0.26** or
**26%**.

<br>

**(b)** For the probability that **a 1 was transmitted given that a 1
was received**, we can use the *Bayes’ Rule for Two Events*. In this
case, we will get *the product of the probabilities that a 1 was
received given a 1 was transmitted* (P_R1givenT1) and *a message is
transmitted as a 1* (P_T1) then divide it by the probability that *a 1
was received* (P_R1).

``` r
P_T1givenR1 <- (P_T1 * P_R1givenT1)/P_R1 # P that a 1 was transmitted given that a 1 was received
cat("Probability that a 1 was transmitted given that a 1 was received:",P_T1givenR1,"or",P_T1givenR1 * 100,"%\n")
```

    ## Probability that a 1 was transmitted given that a 1 was received: 0.8653846 or 86.53846 %

By the *Bayes’ Rule*, the probability that a 1 was transmitted given
that a 1 was received is around **0.865** or **86.5%**.

# Question 7

There are three employees working at an IT company: Jane, Amy, and Ava,
doing 10%, 30%, and 60% of the programming, respectively. 8% of Jane’s
work, 5% of Amy’s work, and just 1% of Ava‘s work is in error. What is
the overall percentage of error? If a program is found with an error,
who is the most likely person to have written it?

<br><br> Right away, we already have the following data:

``` r
P_Jane <- 0.10
P_Amy <- 0.30
P_Ava <- 0.60
```

<br>

First, we must get the **overall percentage of error**. We can easily
use the *Law of Total Probability* and add all of the error
probabilities of each employee, which we can get by individually
multiplying the work distributions (P_Employee) by their respective
error rates.

``` r
P_Jane_Error <- P_Jane * 0.08
P_Amy_Error <- P_Amy * 0.05
P_Ava_Error <- P_Ava * 0.01

P_TotalError <- P_Jane_Error + P_Amy_Error + P_Ava_Error
cat("Overall percentage of error:",P_TotalError * 100,"%\n")
```

    ## Overall percentage of error: 2.9 %

Hence, by the *Law of Total Probability*, the overall percentage of
error is **0.029** or **2.9%**.

<br>

Now, we need to find **the most likely person to have written an error**
in the chance that an error is committed. We already have the error
values stored from the recent computations. Thus, we can simply retrieve
the highest number among the three.

``` r
errors <- c(P_Jane_Error, P_Amy_Error, P_Ava_Error)
names(errors) <- c("Jane","Amy","Ava")
prone_error <- names(errors)[which.max(errors)]
cat("Most likely person to have written an error:",prone_error,"with a likelihood of",max(errors) * 100,"%")
```

    ## Most likely person to have written an error: Amy with a likelihood of 1.5 %

We can see that **Amy** is the most likely person to commit an error,
with a likelihood of **0.015** or **1.5%**.
