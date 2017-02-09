##########################################################
##
## Stability Selection Based on paper: https://stat.ethz.ch/~nicolai/stability.pdf
##
##########################################################
library(stabs);library(glmnet)

(stab.glmnet <- stabsel(x = input, y = output,
                        fitfun = glmnet.lasso, cutoff = 0.75,
                        PFER = 1))