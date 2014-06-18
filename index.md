---
title       : Wealth Accumulation Simulator
subtitle    : Or, how I learned to love random numbers
author      : Edward O'Donnell
job         : Village Idiot
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Simulation of wealth Accumulation
<p>This presentation will demonstrate how to use a simulation program
in R to project the growth of capital assets. The program runs on RStudio's Shiny Server available at <a href="https://eddyod.shinyapps.io/wealth/">Wealth Accumulation Simulation at Shinyapps</a>. This simulation is an adaptation of the <a href="http://glimmer.rstudio.com/systematicin/retirement.withdrawal/">retirement app</a> from
<a href="http://systematicinvestor.wordpress.com/">Systematic Investor</a>.
</p>
<p>The simulation starts with some parameters that are adjusted by the user. These are as follows:</p>
1. Set number of years to accumulate capital.
2. Set starting amount of wealth.
3. Set annual rate of return.
4. Set risk level of rate of return (standard deviation).
5. Set inflation rate.
6. Set inflation rate volatility.
7. Set monthly deposits.
8. Set number of simulations.

--- .class #id 

## Simulation Process

<p>After any of the parameters are set, the shiny reactive inputs will rerun the simulation. The user can also use the submit button to rerun the simulation. The simulation takes all the parameters and runs through a loop which has a length of years * 12. All the calculations are done with monthly data. During each iteration, a random number is drawn from a normal distribution with mean = rate of return and the standard deviation = to the risk the user inputed. The following R code is used:</p>

```r
n_obs = 12
monthly.mean.return = .075
monthly.ret.std.dev = .15
capital <- rnorm(n_obs, mean = monthly.mean.return, sd = monthly.ret.std.dev)
head(capital)
```

```
## [1]  0.26319 -0.08427  0.23187 -0.02620  0.04211  0.03143
```
<p>This number is also added to the monthly deposit and the inflation rate is subtracted. Each time the simulation is run, two graphs and a summary table are produced.
</p>

---

## Multiple line chart of the simulation

<p>Multiple line chart with each line representing one simulation</p>
<img src="linechart.png"/>

---

## Histogram of final period values
<p>This chart is a histogram with density plot of the final values from each simulation. </p>
<img src="histogram.png"/>



