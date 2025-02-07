[![Travis Build Status](https://travis-ci.org/imbs-hl/timbR.svg?branch=master)](https://travis-ci.org/github/imbs-hl/timbR)
[![Coverage Status](https://coveralls.io/repos/github/imbs-hl/timbR/badge.svg?branch=master)](https://coveralls.io/github/imbs-hl/timbR?branch=master)
<!---![CRAN Downloads month](http://cranlogs.r-pkg.org/badges/ranger?color=brightgreen)
![CRAN Downloads overall](http://cranlogs.r-pkg.org/badges/grand-total/ranger?color=brightgreen) --->
## timbR: Tree interpretation methods based on ranger
Lea Louisa Kronziel, Björn-Hergen Laabs

### Introduction
timbR is a collection of methods for the interpretation of random forests trained by ranger. 

In version 1.0 most representaive trees can be selected based on different tree based distance measures.
In version 2.0 artificial representaive trees can be created based on different tree based distance measures.

### Installation
#### R version
To install the timbR R package, run

```R
install.packages("devtools")
library(devtools)
devtools::install_github("imbs-hl/timbR")
```

If you find any bugs, or if you experience any crashes, please report to us.

Please cite our paper if you use timbR.

### References
* Laabs, B.-H., Westenberger, A., & König, I. R. (2024). Identification of representative trees in random forests based on a new tree-based distance measure. Advances in Data Analysis and Classification, 18:363–380. 
* Bannerjee, M., Ding, Y., Noone, A.-M. (2012) Identifying representative trees from ensembles. Stat in Med 31:1601-16.
* Laabs, B.-H., Kronziel, L. L., König, I. R., & Szymczak, S. (2024). Construction of artificial most representative trees by minimizing tree-based distance measures. In L. Longo, S. Lapuschkin, & C. Seifert (Eds.), Explainable Artificial Intelligence:290–310.
