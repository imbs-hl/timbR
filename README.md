[![Travis Build Status](https://travis-ci.org/imbs-hl/timbR.svg?branch=master)](https://travis-ci.org/github/imbs-hl/timbR)
[![Coverage Status](https://coveralls.io/repos/github/imbs-hl/timbR/badge.svg?branch=master)](https://coveralls.io/github/imbs-hl/timbR?branch=master)
<!---![CRAN Downloads month](http://cranlogs.r-pkg.org/badges/ranger?color=brightgreen)
![CRAN Downloads overall](http://cranlogs.r-pkg.org/badges/grand-total/ranger?color=brightgreen) --->
## timbR: Tree interpretation methods based on ranger
Björn-Hergen Laabs

### Introduction
timbR is a collection of methods for the interpretation of random forests trained by ranger. 

In version 1.0 most representaive trees can be selected based on four different tree based distance measures.



### Installation
#### R version
To install the timbR R package, run

```R
install.packages("devtools")
library(devtools)
devtools::install_github("imbs-hl/timbR")
```

If you find any bugs, or if you experience any crashes, please report to us.

Please cite our paper if you use ranger.

### References
* Laabs, B.-H., Westenberger, A., König, I. R. (2020) Identification of representative trees in random forests based on a new tree-based distance measure. Unpublished
* Bannerjee, M., Ding, Y., Noone, A.-M. (2012) Identifying representative trees from ensembles. Stat in Med 31:1601-16.
