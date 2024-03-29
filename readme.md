

# PrecipTDS: Temporal downscaling of precipitation

*PrecipTDS* package generates a higher resolution temporal scale (15 & 30-min)
from 1-h precipitation using a modified stochastic disaggregation method.


# Installation:

The package can be installed from
[github](https://github.com/bijoychandraAU/PreciTDS)

``` r
list.of.packages <- c("devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(devtools)
install_github("bijoychandraAU/PrecipTDS")
```

## Example 1: Temporal Downscaling of 1-hour to 15-min.

The projected precipitation for 1 year (2030) recorded at 1-h scale is disaggregated to 15-min precipitation using the observed (1981-2000) 15-min precipitation.  

### 1.1. Observed 15-min (O15) precipitation.

```{r observed_15min, eval=TRUE}
library(PrecipTDS)
data(obs_15,package = "PrecipTDS")
head(obs_15,5)
```

### 1.2. Model 1-hour (DS60) precipitation.

```{r model, eval=TRUE}
data(model,package = "PrecipTDS")
head(model,5)
```

### 1.3. Disaggregated 15-min (DS15) precipitation from DS60.

```{r Output_15min, eval=TRUE}
DS15=PrecipTDS15(obs=obs_15,mod=model)
head(DS15,5)
```

## Example 2: Temporal Downscaling of 1-hour to 30-min.

The projected precipitation for 1 year (2030) recorded at 1-h scale is disaggregated to 30-min precipitation using the observed (1981-2000) 30-min precipitation.  

### 2.1. Observed 30-min (O30) precipitation.

```{r observed_30min, eval=TRUE}
#library(PrecipTDS)
data(obs_30,package = "PrecipTDS")
head(obs_30,5)
```

### 2.2. Model 1-hour (DS60) precipitation.

```{r model_1h, eval=TRUE}
data(model,package = "PrecipTDS")
head(model,5)
```

### 2.3. Disaggregated 30-min (DS30) precipitation from DS60.

```{r Output_30min, eval=TRUE}
DS30=PrecipTDS15(obs=obs_30,mod=model)
head(DS30,5)
```


# References:
 - Takhellambam, B. S., Srivastava, P., Lamba, J., McGehee, R. P., Kumar, H., & Tian, D. (2022). Temporal disaggregation of hourly precipitation under changing climate over the Southeast United States. Sci Data 9: 211. [https://doi.org/10.1038/s41597-022-01304-7](https://doi.org/10.1038/s41597-022-01304-7) 
 
 - Choi, J., Socolofsky, S. A. & Olivera, F. (2008).Hourly disaggregation of daily rainfall in Texas  using measured hourly precipitation at other locations. Journal of Hydrologic 575 Engineering 13, 476–487.[https://doi.org/10.1061/(ASCE)1084-0699(2008)13:6(476)](https://doi.org/10.1061/(ASCE)1084-0699(2008)13:6(476))
 
 - Socolofsky, S., Adams, E. E. & Entekhabi, D. (2001). Disaggregation of daily rainfall for  continuous watershed modeling. Journal of Hydrologic Engineering 6, 300–309.[https://doi.org/10.1061/(ASCE)1084-0699(2001)6:4(300)](https://doi.org/10.1061/(ASCE)1084-0699(2001)6:4(300))
 
 - Mirhosseini, G., Srivastava, P. & Stefanova, L. (2013).The impact of climate change on rainfall  Intensity–Duration–Frequency (IDF) curves in Alabama. Reg Environ Change 13, 25–33 519.[https://doi.org/10.1007/s10113-012-0375-5](https://doi.org/10.1007/s10113-012-0375-5).


# Authors:

-   Bijoychandra S. Takhellambam
-   Puneet Srivastava
-   Jasmeet Lamba
-   Hemendra Kumar
-   Roberto Molinari
