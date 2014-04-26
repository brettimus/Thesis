# 2/22/14

library(bnlearn)

# Initialization
V <- c("V1","V2","V3","V4")
M.init <- empty.graph(V)
BNNA <- matrix(0L, ncol=4, nrow=4, dimnames = list(V,V))
BNNA["V1","V2"] <- 1
BNNA["V1","V3"] <- 1
BNNA["V1","V4"] <- 1
amat(M.init) <- BNNA

# Helps calculate the prior probability for the "prior" function
# Sort of broken right now
prior.helper <- function(f,d,N) {
    k <- (2*log(N)*sqrt( (N^2 - N + 2)/2 ) - 4*sqrt(N) + 4)^(-1)
    2*k*log( (f+1) / f)*(sqrt(d+1) - sqrt(d))
}

# Returns the prior probability of a model
prior <- function(M) {
    N <- nrow(amat(M))
    M.num.feat <- sum(amat(M)[1,])  # Number of arcs from the class node
    M.num.arcs <- nrow(M$arcs)      # Total number of arcs
    prior.helper(M.num.feat,M.num.arcs,N)
}

# Likelihood of Data Given Model
L <- function(DATA,M) {
    score(M,DATA,type="loglik") # loglik is multinomial log-likelihood score
                                # we use it since our data are discrete
}


##############################
# Proposal
##############################
add.arc <- function(M,arc) {
    amat(M)[arc[1],arc[2]] = 1
}

# Invokes our acceptace rule
proposal.helper <- function(M,M.p) {
    prior.M <- prior(M)
    prior.M.p <- prior(M.p)

    like.M <- L(M,DATA)
    like.M.p <- L(M.p,DATA)

    a <- min(1, (prior.M.p * like.M.p)/(prior.M * like.M))

    # accept M.p with probability a
    sample(c(M.p,M),1,prob=c(a,1-a))
}
# Randomly proposes a new model given the input
propose <- function(M) {

    M.p <- empty.graph(V)
    amat(M.p) = amat(M)

    p <- sample(V,2,replace=F)

    # Check if an arc exists
    if (amat(M)[p[1],p[2]] == 1) {
      # If it exists, we propose its removal
      amat(M.p)[p[1],p[2]] = 0
      proposal.helper(M,M.p)

    } else {
      # If not, check the proposed arc preserves BN properites
      tryCatch(add.arc(M.p,p), error=function(e) proposal.helper(M))
      proposal.helper(M,M.p)
    }

    #
}

# The MCMC Algorithm
mcmc <- function(DATA,M.init,burn=1000,iter=1500) {
    i <- 0
    M <- M.init
    samps <- c()
    # Burn-in period
    while (i < burn) {
        M <- propose(M)
        i <- i + 1
    }
    while (i < iter) {
        M <- propose(M)
        # Record every tenth sample
        if ( (i+1) %% 10 == 0 ) {
            samps <- append(samps, M)
        }
        i <- i + 1
    }
}
