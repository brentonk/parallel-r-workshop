## ----load----------------------------------------------------------------
load(url("http://bkenkel.com/data/clinton-jop.rda"))

## ----describe-house------------------------------------------------------
str(house106)

## ----describe-votes------------------------------------------------------
str(votes106)

## ----first-logit---------------------------------------------------------
## Substitute given vote into member data
house106$vote <- votes106[, 1]

## Run logit of vote choice on member attributes
logit_fit <- glm(vote ~ republican + prefs_same + prefs_nonsame,
                 data = house106,
                 family = binomial(link = "logit"))

## ----first-p-------------------------------------------------------------
summary(logit_fit)

p_vals <- summary(logit_fit)$coef[, 4]
p_vals[2]

## ----packages, eval=FALSE------------------------------------------------
## install.packages(c("microbenchmark", "foreach", "doParallel"))

## ----function------------------------------------------------------------
pval_by_vote <- function(vote, coefficient) {
    ## Substitute given vote into member data
    house106$vote <- votes106[, vote]

    ## Run logit of vote choice on member attributes
    logit_fit <- glm(vote ~ republican + prefs_same + prefs_nonsame,
                     data = house106,
                     family = binomial(link = "logit"))

    ## Retrieve p-value
    p_vals <- summary(logit_fit)$coef[, 4]
    return(p_vals[coefficient])
}

## ----function-test-------------------------------------------------------
pval_by_vote(vote = 1, coefficient = 2)

## ----for-setup-----------------------------------------------------------
p <- rep(NA, 5)

## ----for-----------------------------------------------------------------
for (i in 1:5) {
    p[i] <- pval_by_vote(vote = i, coefficient = 2)
}

## ----for-results---------------------------------------------------------
p
sum(p < 0.05)

## ----for-function--------------------------------------------------------
pvals_for <- function(votes, coefficient) {
    p <- rep(NA, length(votes))
    for (i in 1:length(votes)) {
        p[i] <- pval_by_vote(vote = votes[i], coefficient = coefficient)
    }
    return(p)
}

## ----for-function-results------------------------------------------------
pvals_for(1:5, 2)

## ----system-time-applied-------------------------------------------------
system.time(pvals_for(1:5, 2))

## ----microbenchmark, cache=TRUE------------------------------------------
library("microbenchmark")
microbenchmark(five = pvals_for(1:5, 2),
               ten = pvals_for(1:10, 2))

## ----foreach-------------------------------------------------------------
library("foreach")

## ----foreach-syntax------------------------------------------------------
p1 <- foreach (i = 1:5) %do% {
    pval_by_vote(vote = i, coefficient = 2)
}
p1

## ----foreach-combine-----------------------------------------------------
p2 <- foreach (i = 1:5, .combine = "c") %do% {
    pval_by_vote(vote = i, coefficient = 2)
}
p2

## ----foreach-function----------------------------------------------------
pvals_do <- function(votes, coefficient) {
    foreach (i = votes, .combine = "c") %do% {
        pval_by_vote(vote = i, coefficient = coefficient)
    }
}
pvals_do(1:5, 2)

## ----for-vs-do, cache=TRUE-----------------------------------------------
microbenchmark(for_loop = pvals_for(1:5, 2),
               do = pvals_do(1:5, 2))

## ----dopar-syntax--------------------------------------------------------
foreach (i = 1:5, .combine = "c") %dopar% {
    pval_by_vote(vote = i, coefficient = 2)
}

## ----library-dopar-------------------------------------------------------
library("doParallel")

## ----detect-cores--------------------------------------------------------
## Results will vary across machines
detectCores()

## ----dopar-setup---------------------------------------------------------
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

## ----cluster-export------------------------------------------------------
clusterExport(cl, varlist = ls())

## ----dopar-syntax-redux--------------------------------------------------
foreach (i = 1:5, .combine = "c") %dopar% {
    pval_by_vote(vote = i, coefficient = 2)
}

## ----dopar-function------------------------------------------------------
pvals_dopar <- function(votes, coefficient) {
    foreach (i = votes, .combine = "c") %dopar% {
        pval_by_vote(vote = i, coefficient = coefficient)
    }
}

## ----small-test, cache=TRUE----------------------------------------------
microbenchmark(for_loop = pvals_for(1:5, 2),
               do = pvals_do(1:5, 2),
               dopar = pvals_dopar(1:5, 2))

## ----medium-test, cache=TRUE---------------------------------------------
cols <- 1:ncol(votes106)
system.time(p_do <- pvals_do(cols, 2))
system.time(p_dopar <- pvals_dopar(cols, 2))

## ----check-equality------------------------------------------------------
all.equal(p_do, p_dopar)

## ----get-answer----------------------------------------------------------
sum(p_dopar < 0.05)

## ----big-test, cache=TRUE------------------------------------------------
system.time(for (j in 1:4) pvals_for(cols, j))
system.time(foreach (j = 1:4) %dopar% pvals_for(cols, j))

## ----stop-cluster--------------------------------------------------------
stopCluster(cl)

## ----doMC, eval=FALSE----------------------------------------------------
## library("doMC")
## registerDoMC(2)  # Number of cores to use
## 
## foreach (i = 1:5, .combine = "c") %dopar% {
##     pval_by_vote(vote = i, coefficient = 2)
## }

## ----cache=FALSE, echo=FALSE---------------------------------------------
read_chunk("accre/run.r")

## ----accre-r, eval=FALSE-------------------------------------------------
## load(url("http://bkenkel.com/data/clinton-jop.rda"))
## 
## library("foreach")
## library("doMPI")
## 
## cl <- startMPIcluster()
## registerDoMPI(cl)
## 
## pval_by_vote <- function(vote, coefficient) {
##     ## Substitute given vote into member data
##     house106$vote <- votes106[, vote]
## 
##     ## Run logit of vote choice on member attributes
##     logit_fit <- glm(vote ~ republican + prefs_same + prefs_nonsame,
##                      data = house106,
##                      family = binomial(link = "logit"))
## 
##     ## Retrieve p-value
##     p_vals <- summary(logit_fit)$coef[, 4]
##     return(p_vals[coefficient])
## }
## 
## ## Multiple-coefficient analysis (a few big jobs)
## print(system.time(
##     results0 <- foreach (j = 1:4) %do% {
##         foreach (i = 1:ncol(votes106), .combine = "c") %do% {
##             pval_by_vote(vote = i, coefficient = j)
##         }
##     }
## ))
## 
## print(system.time(
##     results1 <- foreach (j = 1:4) %dopar% {
##         foreach (i = 1:ncol(votes106), .combine = "c") %do% {
##             pval_by_vote(vote = i, coefficient = j)
##         }
##     }
## ))
## 
## save(results0, results1, file = "clinton-2006-results.rda")
## 
## closeCluster(cl)
## mpi.quit()

