data {
    // Dimensionality
    int<lower = 0> d;

    // Data needed to estimate the probabilities according to a binomial 
    // distribution
    array[d, d] int<lower = 0> n_switches;
    array[d] int<lower = 0> col_totals;

    // Priors provided by the user
    real<lower = 0> alpha;
    real<lower = 0> beta;
}

parameters {
    // Likelihood parameters
    array[d, d] real<lower = 0, upper = 1> theta;
}

model {
    // Loop over each column
    for(i in 1:d) {
        // Likelihood
        target += binomial_lpmf(n_switches[, i] | col_totals[i], theta[, i]);

        // Priors
        target += beta_lpdf(theta[, i] | alpha, beta);
    }
}

generated quantities {
    // Left open for now, but could be useful later on
}
