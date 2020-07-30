# Location
### From stay points to spatial behavior
```sh
rog.R
entropy.R 
# calculate Activity Space and Entropy separately

regression.R
# take output from rog.R and entropy.R
# output a data frame contains 4 spatial behaviors
# used for later regression and 2-level growth model 
```
### Reliability
```sh
correlation.R
# take perDay.rds data, and output twin correlations

split_Half_3:30.Rmd
# take output from regression.R
# and output split-half reliability index
```

### Linear Mixed Model with Random Effects of Time
```sh
analysis.R
# remove 7 individuals with less than 2 days of data (based on split-half reliability)
# MML model and extract random and fixed effects
# biometric variance decomposition
# code for plotting heritability over time 
```