# Modeling Personality and Daily Spatial Behavior: A Longitudinal Study in Adolescent Twin
full presentation is in `personality_and_movement.pdf`  

Key Question to answer: Is our movement routine across time and space impacted by gene or environment?  
Of course - our movement has been heavily constrained during pandemic, and I can predict this perfectly :)  
We leveraged four daily spatial behavior to quantify a movement pattern which are:
1. number of places visited
2. distance traveled
3. activity space
4. entropy (or predictability)

We found that:
1. Spatial Behavioral is stable over time (2-4 years)
2. Personality correlates with daily spatial behavior
3. Heritability of daily spatial behaviors increases substantially between age 16 and 18



# Technical Guide for this Repository
# Location
### Visualization
```sh
mapAll_layered.py
#created interactive maps for twins with title "MZ/OS/DZ_IDA_IDB.html", 
#using two local module circleMarkLayer.py and TwinLayer.py
```

### Summary Statistic
```sh
summary.R
#takes an .rds dataset (latitude, longitude, and time) as input
#outputs an .rds data.table that later could be visualized
#in summary_errorRemoving.Rmd (Rmarkdown file)
#providing information on accuracy, time track interval, step length, 
#speed, length of observation, number of points, daily mileage 
#from within and between individual perspectives.
```

### Data Cleaning
```
inside folder cleaning
Code executable in R and Rmarkdown
```

### Stay Point Visualization
```
inside folder .idea
Code executable in Python
```

### Analysis
```
insde Analsyis folder
Code executable in R and Rmarkdown
formula to derive 4 spatial variables
Split-half reliability plots
Twin reliability plots
formal analysis such as regression
```


