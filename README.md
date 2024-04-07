# Analysis for the Study "The Presence of Atmospheric Microplastics on Quezon City, Philippines"
### Monteron, De Lara, Peregrino, Ferrer, Primavera (2024)

## Background
To study the atmospheric MPs in Quezon City, we used total active samplers
with a specified volume rate (67.96 cubic meters per hour) where the 
particles, including MPs deposit on the filter. We then aim to use this information
assess the concentrations of MP from the total number of MPs found in the entire filter divided by the total volume of the air sampled. However due to logistical problems, we do not have the access to the entire filter which corresponds to the
entire number of MPs with the volume of air sampled. Hence, we have to use
estimate techniques.

## Mathematical Formalism
### Single Filter
Let $A$ be the area of a given filter. We will then sample a section of the filter
with an area $a$ such that $A = ak$ where $k$ is the number of equal-sized sections that the filter can be decomposed into.

We will then obtain the number of MPs in $a$ and call that $x$. Under the assumption that the MPs are uniformly deposited on
the entire filter, any such $a$ is an accurate estimator for the number of MPs
on any similar-sized $a$. In another words, if $X$ is the true number of MPs a given filter, the
best estimator for $X/k$ (subsections with size $a$), under the uniform deposition assumption is distributed by
$$
X/k \sim \mathrm{Poisson}(x)
$$

To take account of the inherent uncertainty of our $X/k$ estimate, we will construct two-tailed $\alpha$-confidence intervals around our obtained measure of $x$ as follows

$$\mathbb{P}(\phi < X/k < \varphi) = 1 - \alpha$$

Since we now have our $\alpha$-intervals around $X/k$ we can now use the obtained estimates, $k\phi$ and $k\varphi$ as endpoints of our interval and $kx$ for the mean, as the estimates for $X$. Since we now have estimates for $X$ we can also obtain concentration estimates by dividing our estimates by the total volume associated with the specified filter. 

### Multiple Filters
Now, consider a case where we have multiple with filters with each having MPs $X_1, X_2, ..., X_n$. Suppose we have obtained a corresponding $x_1, x_2, ..., x_n$ from each filter and each $x_i$ has the same area. Then,

$$
\frac{1}{k}\sum_{i} X_i \sim \mathrm{Poisson}\left(\sum_{i}x_i\right)
$$

We can also construct confidence intervals as follows

$$\mathbb{P}\left(\phi < \frac{1}{k}\sum_{i} X_i < \varphi\right) = 1 - \alpha$$

From this, we can obtain a pooled estimate for the number of MPs using the same estimation process outlined above.