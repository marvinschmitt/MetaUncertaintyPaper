
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The Meta-Uncertainty Framework

<!-- badges: start -->
<!-- badges: end -->
<center>
<img src="inst/meta-uncertainty-figure-1.png" style="width:85.0%" />
</center>

This repository contains the code for the paper *Meta-Uncertainty in
Bayesian Model Comparison*: <https://arxiv.org/abs/2210.07278>

Note that the R code is structured as a package, thus requiring a local
installation with subsequent loading via
`library(MetaUncertaintyPaper)`.

## Installation Instructions

### R environment

The R environment is captured with `renv`. Install the `renv` package
and load the environment with

    renv::restore()

### Python environment

The package requirements of the Python environment (except BayesFlow,
see below) are captured in the `requirements.txt` file. Recreate the
environment using

    pip install -r requirements.txt

The amortized model comparison network (BayesFlow) in Experiment 3 uses
[BayesFlow](https://github.com/stefanradev93/BayesFlow) at commit
`c4208418ad19b6648be216cfe013c8f5317a652c`:
<https://github.com/stefanradev93/BayesFlow/tree/c4208418ad19b6648be216cfe013c8f5317a652c>.

Should you fail to install this BayesFlow version or encounter
unexpected errors, you can load the trained neural networks’ weights
from the folder `python/checkpoints_exp3/` and avoid re-training the
network.
