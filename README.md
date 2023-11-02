
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

<br>

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

<br>

### Analysis (Applying CarefullyCausal)

We will now shift our focus to actually using the CarefullyCausal
function. The minimal call requires us to specify the following
arguments: *formula, data, family and exposure*. Note that you can
always consult the help file within R to see the documentation of
CarefullyCausal and to learn about all available arguments including an
explanation. You can access this by simply typing in *CarefullyCausal*
in the help tab in R. Nonetheless, we summarise the key arguments we
need for the minimal call.

- *Formula*, this has the same form as for example when using *glm*
  which is $y\sim x + w$. Here $y$ denotes the outcome of interest
  variable, $x$ denotes the exposure and $w$ is a covariate we want to
  adjust for
- *Data*, specify the data frame (data set)
- *Family*, this is the same as in *glm* where it describes the error
  distribution and link function. Here we have two choices: *“gaussian”*
  or *“binomial”*. This depends on the nature of the outcome of
  interest. In our example the outcome is continuous and thus we use the
  *“gaussian”* argument (default setting)
- *Exposure*, we explicitly define our exposure variable as a character
  string. In this example, it will be *“qsmk”*

``` r
# Transform exposure in a factor variable and transform into dataframe
df$qsmk = as.factor(df$qsmk) 
df = as.data.frame(df)

# Run the CarefullyCausal function and save it in the object "output"
output <- CarefullyCausal(wt82_71 ~ qsmk + race + sex + education + smokeintensity + smokeyrs + wt71 + exercise
                          + active + age,
                          data = df,
                          exposure = "qsmk",
                          family = "gaussian")
# Print the output
output
#> 
#> Estimand: 
#> Conditional 
#> E[wt82_71^qsmk=1|race, sex, education, smokeintensity, smokeyrs, wt71, exercise, active, age]  -  E[wt82_71^qsmk=0|race, sex, education, smokeintensity, smokeyrs, wt71, exercise, active, age]
#> 
#> Marginal 
#> E[wt82_71^qsmk=1]  -  E[wt82_71^qsmk=0]
#> *Please see output at $Estimand_interpretation for details 
#>  
#> 
#> Treatment effect: 
#>                          Estimate Std. Error S-value 95%.CI.lower 95%.CI.upper
#> qsmk1 outcome regression    3.381      0.441  44.858        2.517        4.246
#> qsmk1 IPTW                  3.318      0.494  35.198        2.351        4.286
#> qsmk1 S-standardization     3.381      0.455     Inf        2.438        4.222
#> qsmk1 T-standardization     3.448      0.462     Inf        2.547        4.359
#> qsmk1 TMLE                  3.370      0.494     Inf        2.401        4.339
#> 
#> Reference exposure level: 0 
#> 
#> 
#> Please evaluate whether the difference beteen the lowest estimate: 3.3183 and highest: 3.4482 is of substance, 
#> given the nature of the data. If so, evaluate the different modelling assumptions.
#> 
#> 
#> To interpret these effects as causal, the following key assumptions must be satisfied: 
#> 
#> [1] Conditional exchangeability: implies that adjusting for "race, sex, education, smokeintensity, smokeyrs, wt71, exercise, active, age" is enough to completely eliminate 
#> all confounding and selection bias. See the covariate balance table ($Assumptions$exchangeability$covariate_balance) 
#> in the saved output and the corresponding explanations ($Assumptions$exchangeability$explanation). 
#> 
#> [2] Positivity: is satisfied when both exposed and unexposed individuals are observed within every stratum of variables adjusted for ( race, sex, education, smokeintensity, smokeyrs, wt71, exercise, active, age ). This can be evaluated using the propensity plots saved in the output at $Assumptions$positivity$plots (or identically use the ps.plot() function), the table below ($Assumptions$positivity$ps_table) and the corresponding explanation found at $Assumptions$positivity$explanation. Note: PS=propensity score 
#>  
#>                        PS range for 1
#> observed exposure: 0   0.0338, 0.6520
#> observed exposure: 1   0.0685, 0.7709
#> 
#> [3] Consistency: implies that exposure 'qsmk' must be sufficiently well-defined so that any variation within 
#> the definition of the exposure would not result in a different outcome. See $Assumption$consistency 
#> for a more in-depth explanation and examples. 
#> 
#> [4] No measurement error: assumes that all variables were measured without substantial error, such that
#> no substantial measurement bias is present. However, if the presence of substantial measurement bias is plausible, 
#> then the estimated effects should be carefully reconsidered as being causal effects. See $Assumptions$no_measurement_error 
#> for a further discussion 
#> 
#> [5] Well-specified models: assumes that any models used are well-specified meaning that they include all
#> relevant non-linearities and/or statistical interactions
```

<br>

[^1]: National Health and Nutrition Examination Survey Data I
    Epidemiologic Follow-up Study,
    <https://wwwn.cdc.gov/nchs/nhanes/nhefs/>

[^2]: Hernán MA, Robins JM (2020). Causal Inference: What If. Boca
    Raton: Chapman &
    Hall/CRC,<https://www.hsph.harvard.edu/miguel-hernan/causal-inference-book/>
