
# Preci_diasg: Temporal downscaling of precipitation.

*PreciTDS* package generates a higher resolution temporal scale (15-min)
from 1-h precipitation using a modified stochastic disaggregation
method.

# Installation:

The package can be installed from
[github](https://github.com/bijoychandraAU/PreciTDS)

``` r
list.of.packages <- c("devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(devtools)
install_github("bijoychandraAU/PreciTDS")
```

# Introduction:

*PreciTDS* package required both observed (15-min) and model (1-h)
precipitation data. Both precipitation can be either continuous or
discontinuous time series format. They have to be either dataframe or
tibble format.

## Examples

The projected precipitation for Jan-2030 that recorded at 1-h scale is
disaggregated to 15-min precipitation using the observed 15-min
precipitation (1977-2013).

``` r
library(PreciTDS)
Preci_diasg(obs=observed,mod=model)
```

# References:

-   Choi, J., Socolofsky, S. A. & Olivera, F. [Hourly disaggregation of
    daily rainfall in Texas using measured hourly precipitation at other
    locations](https://doi.org/10.1061/(ASCE)1084-0699(2008)13:6(476)).
    Journal of Hydrologic 575 Engineering 13, 476–487 (2008).

-   Socolofsky, S., Adams, E. E. & Entekhabi, D. [Disaggregation of
    daily rainfall for continuous watershed
    modeling](https://doi.org/10.1061/(ASCE)1084-0699(2001)6:4(300)).
    Journal of Hydrologic Engineering 6, 300–309 (2001).

-   Mirhosseini, G., Srivastava, P. & Stefanova, L.[The impact of
    climate change on rainfall Intensity–Duration–Frequency (IDF) curves
    in Alabama](https://doi.org/10.1007/s10113-012-0375-5). Reg Environ
    Change 13, 25–33 519 (2013).

# Authors:

-   [Bijoychandra Takhellambam](https://github.com/bijoychandraAU)
-   Puneet Srivastava
-   Jasmeet Lamba
-   Ryan P. McGehee
-   Hemendra Kumar
-   Di Tian
