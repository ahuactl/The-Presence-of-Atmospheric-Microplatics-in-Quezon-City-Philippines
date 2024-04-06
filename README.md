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
We let $X$ be the true number of MPs in a filter. If we assume that the particles 
are evenly deposited on the entire filter, we expect that for any subsection $a$,
the number of particles is given by

$$
a \sim \mathrm{Poisson}(\lambda)
$$

where $\lambda$ is the true density of microplastic deposition of the filter.
If there are $k$ equal sized subsections, then we could plasubily estimate $X$ as
$k$ times $a$ under the assumption of equal deposition in the entire filter. 

