
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CarefullyCausal

<!-- badges: start -->
<!-- badges: end -->

The goal of CarefullyCausal is to provide the user a practical guide
when doing causal analyses. Particularly, CarefullyCausal provides the
user the estimand, a table of causal estimates, a discussion on the
causal assumptions, relevant diagnostics and
interpretations/explanations. The key aspects of a causal analyses are
printed and discussed in detail to help the user in evaluating whether
the estimated effects can be interpreted as being causal. Currently,
CarefullyCausal can be used in a setting with a fixed-exposure, meaning
that the exposure does not vary over time.  

Some key features:  

- **Setting**: Fixed-exposure
- **Outcome of interest**: can be dichotomous or continuous
- **Exposure of interest**: can be dichotomous, multi-value (max 4
  levels) or continuous
- **Effect measures**: can be in log(odds), risk ratio or odds ratio

## Installation

You can install the development version of CarefullyCausal as follows:

``` r
# To download R packages from Github or other sources we need the "devtools" package
install.packages("devtools")
library("devtools")

# Now we can download the CarefullyCausal package
install_github("mauricekorf/CarefullyCausal")
library(CarefullyCausal)
```

## Example

To develop some intuition with the CarefullyCausal function and to
highlight some important features, an example is step-by-step
illustrated using the NHEFS[^1] data set.

### Research Question

Suppose that we are interested in the causal relation between quitting
smoking and weight change (in Kilograms). Specifically,we would like to
know the effect of quitting smoking on someone’s weight change. In order
to evaluate this causal relation, we assume that there are some
important confounders we need to adjust for, which includes:  
sex, race, age, education level, smoke intensity (number of cigarettes
per day), how long someone has been smoking ( in years), start weight
(in Kilograms), how much someone exercises (**0:** much exercise, **1:**
moderate exercise, **2:** no exercise) and how active a person is on a
usual day (**0:** very active, **1:** moderately active, **2:**
inactive)

<br> Given that we assume that these are the only confounders and that
no collider bias or selection bias is induced, we would obtain the
following simple Directed Acyclic Graph (DAG):
<p align="center">
<img src="man/figures/README-RQ-1.png" width="60%" style="display: block; margin: auto;" />
</p>
<center>
<i>Figure 1: </i>The selected variables are for illustration purposes
only. The DAG shows by no means the true causal structure
</center>

### Data

In order to answer our research question, we will use a subset of the
NHEFS data set from Hernán MA & Robins JM (2020) in the *Causal
inference: What if* book[^2]. To conveniently import this data set we
will use the package *causaldata*.

``` r
# Download the required package, containing various (causal) data sets
install.packages("causaldata")
library(causaldata)

# Load in the data, we will use the complete cases variant
df = nhefs_complete
```

<br>

We will now select the variables that we deemed to be relevant, as shown
in Figure 1 (DAG). We show the first six rows of the data set below, to
get an idea of the data set.

``` r
# Select variables
df = nhefs_complete[,c("wt82_71","qsmk","race","sex","education","smokeintensity", "smokeyrs","wt71","exercise","active", "age")]
```

<table>
<thead>
<tr>
<th style="text-align:right;">
wt82_71
</th>
<th style="text-align:right;">
qsmk
</th>
<th style="text-align:left;">
race
</th>
<th style="text-align:left;">
sex
</th>
<th style="text-align:left;">
education
</th>
<th style="text-align:right;">
smokeintensity
</th>
<th style="text-align:right;">
smokeyrs
</th>
<th style="text-align:right;">
wt71
</th>
<th style="text-align:left;">
exercise
</th>
<th style="text-align:left;">
active
</th>
<th style="text-align:right;">
age
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
-10.093960
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
79.04
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
42
</td>
</tr>
<tr>
<td style="text-align:right;">
2.604970
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
58.63
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
36
</td>
</tr>
<tr>
<td style="text-align:right;">
9.414486
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
56.81
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:right;">
56
</td>
</tr>
<tr>
<td style="text-align:right;">
4.990117
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
59.42
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
68
</td>
</tr>
<tr>
<td style="text-align:right;">
4.989251
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
0
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
87.09
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
40
</td>
</tr>
<tr>
<td style="text-align:right;">
4.419060
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
99.00
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
43
</td>
</tr>
</tbody>
</table>

<br>

To further inspect the variable coding and the corresponding definition
we will create a table including the class of the variable.
<center>
<table>
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:left;">
class
</th>
<th style="text-align:left;">
description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
wt82_71
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
The weight change between 1971 and 1982 in Kg
</td>
</tr>
<tr>
<td style="text-align:left;">
qsmk
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Quit smoking, 1: yes, 0: no
</td>
</tr>
<tr>
<td style="text-align:left;">
race
</td>
<td style="text-align:left;">
factor
</td>
<td style="text-align:left;">
1: black or other, 0: white,
</td>
</tr>
<tr>
<td style="text-align:left;">
sex
</td>
<td style="text-align:left;">
factor
</td>
<td style="text-align:left;">
1: female, 0: male
</td>
</tr>
<tr>
<td style="text-align:left;">
education
</td>
<td style="text-align:left;">
factor
</td>
<td style="text-align:left;">
1: 8th grade, 2: HS dropout, 3: HS, 4: college dropout, 5: college or
higher
</td>
</tr>
<tr>
<td style="text-align:left;">
smokeintensity
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Number of cigarettes smoked per day in 1971
</td>
</tr>
<tr>
<td style="text-align:left;">
smokeyrs
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Years of smoking
</td>
</tr>
<tr>
<td style="text-align:left;">
wt71
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Start weight (Kg) in 1971
</td>
</tr>
<tr>
<td style="text-align:left;">
exercise
</td>
<td style="text-align:left;">
factor
</td>
<td style="text-align:left;">
In recreation in 1971 how mu h exercising, 0: much, 1: moderate, 2:
little or none
</td>
</tr>
<tr>
<td style="text-align:left;">
active
</td>
<td style="text-align:left;">
factor
</td>
<td style="text-align:left;">
On usual day how active in 1971, 0: very active, 1: moderately active,
2: inactive
</td>
</tr>
<tr>
<td style="text-align:left;">
age
</td>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
Age in 1971
</td>
</tr>
</tbody>
</table>
</center>

[^1]: National Health and Nutrition Examination Survey Data I
    Epidemiologic Follow-up Study,
    <https://wwwn.cdc.gov/nchs/nhanes/nhefs/>

[^2]: Hernán MA, Robins JM (2020). Causal Inference: What If. Boca
    Raton: Chapman &
    Hall/CRC,<https://www.hsph.harvard.edu/miguel-hernan/causal-inference-book/>
