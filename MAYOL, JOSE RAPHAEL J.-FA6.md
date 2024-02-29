---
title: "MAYOL, JOSE RAPHAEL J.-FA6"
output: html_document
date: "2024-02-27"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# I. Geometric Distribution
Provide an R code for the geometric distribution. The geometric distribution is a probability distribution that models the number of trials required to achieve the first success in a sequence of Bernoulli trials, where each trial has a constant probability of success.
*
<br><br>
In this problem, we are asked to analyze and obtain basic statistic measures for a randomly generated geometric distribution with a 20% probability of success.
<br>
**1. Set the probability of success:  p <- 0.2**
<br>
```{r}
p <- 0.2
```
<br>
**2. Generate 1000 random variables from the geometric distribution.**
This can be simply done with the built-in rgeom() function which creates a vector of random variables that are already distributed geometrically.
```{r}
x <- rgeom(1000, p)
```
<br>
**3. Calculate some basic statistics:**
```{r}
mean_x <- mean(x)
var_x <- var(x)
sd_x <- sd(x)
```
The generated values signify the mean, variance, and standard deviation of our geometric distribution. Moreover, the mean of the distribution is also the average number of trials taken to achieve the first success. This also tells us that the number of trials required to achieve first success IS the mean, but there is a catch. When talking about "trials", we are referring to integer values. This means that quantities such as 3.9 or 4.2 are not valid values for "trials" since a float-value trial does not exist in real life. Thus, the number of trials required to achieve first success is actually the **ceiling value of the mean**.
<br><br>
**4. Print the results in item 3**
```{r}
# Print the results
cat("Number of trials required to achieve first success: ", ceiling(mean_x), "\n",
    "Mean (in 2 decimal places): ", round(mean_x, 2), "\n",
    "Variance (in 2 decimal places): ", round(var_x, 2), "\n",
    "Standard deviation (in 2 decimal places): ", round(sd_x, 2), "\n")
```
<br>
**5. Plot the histogram of the results.**
```{r}
hist(x, breaks = 20, main = "Geometric Distribution", xlab = "Number of trials", ylab = "Frequency")
```
<br>
From the histogram, we now have an idea regarding the behavior of the data. Since the shape is heavily skewed to the right, we can infer that the distribution is heavily skewed positively, in other words, a skewness above 1.5. This also means that the first success is achieved relatively early for the most of the simulations.
<br>
To support our hypothesis, we can also go even further and compute for the **skewness** and **kurtosis** of our randomly created geometric distribution:
```{r}
skewness_geometric <- sum((x - mean_x)^3) / (length(x) * sd_x^3)
kurtosis_geometric <- sum((x - mean_x)^4) / (length(x) * sd_x^4) - 3
cat("Skewness of geometric distribution: ", skewness_geometric, "\n",
    "Kurtosis of geometric distribution: ", kurtosis_geometric, "\n")
```
We can see that the skewness value is extremely positive, thus proving our hypothesis of a high skewness (heavily skewed to the right). Moving on, the kurtosis value for a normal distribution is 3. For reference, the kurtosis for our distribution is higher than that. This implies that the graph is more peaked than normal.
<br>

# II. Hypergeometric Distribution
Consider a plant manufacturing IC chips of which 10% are expected to be defective. The chips are packed in boxes for export. Before transportation, a sample is drawn from each box. Estimate the probability that the sample contains more than 10% defectives, when:

1. A sample of 10 is selected from a box of 40;
2. A sample of 10 is selected from a box of 5000.
<br><br>

To determine the probability in this hypergeometric distribution, we can simply use the dhyper() function. But first, we need to set the parameters in each situation.
```{r}
n1 <- 10  # Sample size for scenario 1
N1 <- 40  # Total number of chips in scenario 1
K1 <- N1 * 0.1  # Number of defective chips in scenario 1

n2 <- 10   # Sample size for scenario 2
N2 <- 5000 # Total number of chips in scenario 2
K2 <- N2 * 0.1  # Number of defective chips in scenario 2
```
<br>
Here, we indicated the sample sizes, total number of chips, and the number of defective chips for both scenarios 1 and 2. The next step will be to calculate the probability of each scenario happening using the dhyper() function.
```{r}
# Calculate probabilities
prob_defectives_scenario1 <- sum(dhyper(2:n1, m = K1, n = N1 - K1, k = n1, log = FALSE))
prob_defectives_scenario2 <- sum(dhyper(2:n2, m = K2, n = N2 - K2, k = n2, log = FALSE))

# Display results
cat("Probability that the sample contains more than 10% defectives when:\n",
    "A sample of 10 is selected from a box of 40: ", prob_defectives_scenario1, "\n",
    "A sample of 10 is selected from a box of 5000: ", prob_defectives_scenario2, "\n")
```
<br>
As we can see, the probabilities of scenarios 1 and 2 occurring are **both around 26%**. Even with the largely different population size, the chances of pulling more than 10% defectives in a sample of 10 are still the same since 10% of the whole population remain defective. To further prove the result of this generated probabilities, we can use the definition of probability.
<br><br>
We will start with **Scenario 1**. Our population consists of 40 chips, and since 10% are defective, then this means that 36 are good while 4 are defects. To get the probability of getting more than 10% defectives (or >1) in the sample, we need to get the probability of getting less than or equal to 1 defects or P(X<=1) and subtract it from 1.
<br><br>
To get P(X<=1), we can get both P(X=0) and P(X=1) then add them together.
<br><br>
P(X=0) is equivalent to (36 C 10)/(40 C 10); where (36 C 10) is the number of combinations where you can get exactly 0 defectives & (40 C 10) is the total number of possible combinations.
<br><br>
For P(X=1), the value is equivalent to (36 C 9)(4 C 1)/(40 C 10); where (36 C 9)(4 C 1) is the number of combinations to get exactly 9 good chips and 1 defective.
<br><br>
Combining both P(X=0) and P(X=1), we can get P(X<=1) which can be represented by [(36 C 10) + (36 C 9)(4 C 1)]/(40 C 10).
<br><br>
Finally, we can get the probability of getting more than 10% defectives or P(X>1) by subtracting P(X<=1) from 1. This is also equivalent to 1 - [(36 C 10) + (36 C 9)(4 C 1)]/(40 C 10), which when evaluated results to 0.25588 or about **26%**, just like the value generated from the codes beforehand.
<br><br><br>
For **Scenario 2** we can just use the same principles. However, we have to take note that the population size is 5000, 10% or 500 are defectives, and 4500 are still good chips. Then, we have to determine the value of P(X>1) just like before.
<br><br>
P(X>1) = 1 - [P(X=0) + P(X=1)]/(5000 C 10)
<br><br>
P(X>1) = 1 - [(4500 C 10) + (4500 C 9)(500 C 1)]/(5000 C 10)
<br><br>
P(X>1) = 0.26386...
<br><br>
Again, the result is around **26%**, the same as the probability we generated from the code. This proves that both the probabilities of getting more than 10% defectives from a sample of 10 in Scenarios 1 and 2 are **both 26%**.