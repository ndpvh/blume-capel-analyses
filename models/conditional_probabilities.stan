data {
    // Dimensionality d and number of conditions n
    int<lower = 1> d;
    int<lower = 1> n;

    // Data needed to estimate the probabilities according to a binomial 
    // distribution per condition, namely the number of switches observed in 
    // the data per condition and the total number of observations per column 
    // (allowing us to construct conditional probabilities)
    array[d, d, n] int<lower = 0> n_switches;
    array[d, n] int<lower = 0> col_totals;

    // Data needed to estimate the difference in probabilities per condition, 
    // namely the number of comparisons you would like to make (m) and the 
    // indices of the above arrays that correspond to these differences
    int<lower = 0> m;
    array[m, 2] int<lower = 1> diff_indices;

    // Priors provided by the user
    real<lower = 0> alpha;
    real<lower = 0> beta;
}

parameters {
    // Likelihood parameters
    array[d, d, n] real<lower = 0, upper = 1> theta;
}

model {
    // Loop over each column and each condition
    for(i in 1:n) {
        for(j in 1:d) {
            // Likelihood
            target += binomial_lpmf(n_switches[, j, i] | col_totals[j, i], theta[, j, i]);

            // Priors
            target += beta_lpdf(theta[, j, i] | alpha, beta); 
        }
    }
}

generated quantities {
    // Difference in probability per condition. Create an array that will 
    // carry this difference and then compute them
    array[d, d, m] real<lower = -1, upper = 1> theta_diff;
    int<lower = 1> idx_1;
    int<lower = 1> idx_2;

    for(i in 1:m) {
        // Assign the indices denoting the conditions
        idx_1 = diff_indices[i, 1];
        idx_2 = diff_indices[i, 2];

        // Loop over all values of theta to compute the differences
        for(j in 1:d) {
            for(k in 1:d) {
                theta_diff[j, k, i] = theta[j, k, idx_1] - theta[j, k, idx_2];
            }
        }  
    }
}
