MAYOL, JOSE RAPHAEL J.-FA2
================
2024-02-18

# Question 1

<br> **1.)** Use R to illustrate that the probability of getting:

a.) a head is 0.5 if a fair coin is tossed repeatedly;<br> b.) a red
card is 0.5 if cards are drawn repeatedly with replacement from a
well-shuffled deck;<br> c.) an even number is 0.5 if a fair die is
rolled repeatedly.

<br><br> For this item, we are given three different scenarios: a coin
toss, a card draw, and a die roll. Now from a purely theoretical
standpoint, the probability of getting a head from a fair coin toss is
0.5 (1 out of 2 “head” or “tail” outcomes). Likewise, the probability of
drawing a red card from a standard deck with replacement is 0.5 (26 red
cards in a 52-card deck). The probability is also 0.5 in getting an even
number out of a fair die roll (getting either 2, 4, or 6 in a 6-faced
die). With R, we can simulate these scenarios in an experimental manner.
**NOTE:** The results of an experimental simulation will ALMOST ALWAYS
be different from a theoretical one, but the tendencies will stay the
same as shown in the succeeding text.

<br> Let us start with **scenario (a)**, the coin toss.

<br> Before starting anything that warrants the creation of random
numbers, we will first need to start with this line of code:

``` r
set.seed(123)
```

We will be doing this for every scenario onwards. This will ensure that
every time a random function is called, the exact same set of random
numbers will also be called. This ensures the reproducibility of the
generated results. <br> For these simulations, we will be reproducing
every scenario for 10,000 times each. We will use the sample() function
in R which takes a random sample from a given dataset or vector, with or
without replacement. Then, we will be taking the proportion of all “H”
results in the simulation, effectively giving us the probability of
getting “head” from a coin toss.

``` r
set.seed(123)
num_tosses1 <- 10000
coin_tosses1 <- sample(c("H", "T"), num_tosses1, replace = TRUE)
prob_head1 <- mean(coin_tosses1 == "H")
```

<br> For **scenario (b)**, we can use the same principle as we did in
(a). Although this time, we will use the rep() function to create the
deck with 26 black and 26 red cards.

``` r
set.seed(123)
num_draws1 <- 10000
deck <- rep(c("red", "black"), each = 26)
drawn_cards1 <- sample(deck, num_draws1, replace = TRUE)
prob_red1 <- mean(drawn_cards1 == "red")
```

<br> The same principle as the other two will also be used for
**scenario (c)**. The ‘%%’ operator will be used to check for evenness.

``` r
set.seed(123)
num_rolls1 <- 10000
die_rolls1 <- sample(1:6, num_rolls1, replace = TRUE)
prob_even1 <- mean(die_rolls1 %% 2 == 0)
```

<br> Now, we can present the generated probabilities through a table for
better viewing experience.

``` r
viewing1 = matrix(c(prob_head1,prob_red1,prob_even1), ncol=3, byrow=TRUE)
colnames(viewing1) = c('Head','Red','Even')
rownames(viewing1) <- c('Probability')
viewing_table1 = as.table(viewing1)
viewing_table1
```

    ##               Head    Red   Even
    ## Probability 0.5017 0.5036 0.4951

<br> We can see that the probabilities are very close to 50, but not
exactly the same value. Let us try again but with a higher simulation
repetition of 100,000.

``` r
# (a) Getting a head from a fair coin toss
set.seed(123)
num_tosses2 <- 100000
coin_tosses2 <- sample(c("H", "T"), num_tosses2, replace = TRUE)
prob_head2 <- mean(coin_tosses2 == "H")

# (b) Drawing a red card from a well-shuffled 52-card deck
set.seed(123)
num_draws2 <- 100000
deck <- rep(c("red", "black"), each = 26)
drawn_cards2 <- sample(deck, num_draws2, replace = TRUE)
prob_red2 <- mean(drawn_cards2 == "red")

# (c) Getting an even result from a fair die toss
set.seed(123)
num_rolls2 <- 100000
die_rolls2 <- sample(1:6, num_rolls2, replace = TRUE)
prob_even2 <- mean(die_rolls2 %% 2 == 0)

# viewing table 2
viewing2 = matrix(c(prob_head2,prob_red2,prob_even2), ncol=3, byrow=TRUE)
colnames(viewing2) = c('Head','Red','Even')
rownames(viewing2) <- c('Probability')
viewing_table2 = as.table(viewing2)
viewing_table2
```

    ##                Head     Red    Even
    ## Probability 0.50161 0.50155 0.49885

<br> We can see that the results are still not exactly 0.5, but now they
are closer to the theoretical value than before. Thus, we have shown how
we can use R to simulate statistical situations like these three.

<br>

# Question 3

<br> **3.)** An experiment consists of rolling a die. Use R to simulate
this experiment 600 times and obtain the relative frequency of each
possible outcome. Hence, estimate the probability of getting each of 1,
2, 3, 4, 5, and 6.

<br><br> To do this problem, we can use the exact same method as
Question 1 Scenario (c). But this time, we will only do 600 simulation
runs to get the probability of each outcome. As explained in the
previous question, we will use set.seed(123) for reproducibility
purposes.

``` r
set.seed(123)
num_simulations <- 600
die_rolls <- sample(1:6, num_simulations, replace = TRUE)

relative_frequency <- table(die_rolls) / num_simulations
print(relative_frequency)
```

    ## die_rolls
    ##         1         2         3         4         5         6 
    ## 0.1766667 0.1700000 0.1600000 0.1533333 0.1666667 0.1733333

In this table, we can see the probabilities of getting each result from
the die roll situation. We can also observe that all of the values in
the table add up to 1, which further solidifies the validity of the
result of our simulation. Theoretically, the probability of getting a
specific result from a six-faced fair die is actually 1/6 or
approximately 0.1667, being 1 out of 6 scores. The probabilities
generated from the simulation are not the same as expected, but they
follow the trend that is supposed to happen based on the theoretical
value.
