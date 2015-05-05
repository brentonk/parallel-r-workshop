## ---- accre-r
load(url("http://bkenkel.com/data/clinton-jop.rda"))

library("foreach")
library("doMPI")

cl <- startMPIcluster()
registerDoMPI(cl)

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

## Multiple-coefficient analysis (a few big jobs)
print(system.time(
    results0 <- foreach (j = 1:4) %do% {
        foreach (i = 1:ncol(votes106), .combine = "c") %do% {
            pval_by_vote(vote = i, coefficient = j)
        }
    }
))

print(system.time(
    results1 <- foreach (j = 1:4) %dopar% {
        foreach (i = 1:ncol(votes106), .combine = "c") %do% {
            pval_by_vote(vote = i, coefficient = j)
        }
    }
))

save(results0, results1, file = "clinton-2006-results.rda")

closeCluster(cl)
mpi.quit()
