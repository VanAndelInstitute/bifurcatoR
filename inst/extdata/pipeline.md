Basic documentation of current (i.e. as of 2/14/22) simulation framework. To be updated as code develops/is modified.

# Generate Data

### `configure_params`
Based on specific parameters (see below), create an array of simulated data to be used fitted on various methods. Alternatively, we can generate the data as it is being fitted to a method of interest but I figured it might just be easier to simulate the data beforehand, especially if we are interested in multiple configurations.

Right now, we are only interested in the Normal distribution, but `configure_params` has been set up to include:
* Beta
* Cauchy
* Normal
* Uniform
* Weibull

The other parameters include:
* Balanced Design (`balanced`): whether to sample `X=0` and `X=1` (for strain effect) with equal probability from the Binomial distribution.
* Number of Groups (`nGroups`): `n_groups = 2` for strain effect; `n_groups = 3` for allele effect (TBD).
* Number of Subjects (`nSubjects`): self-explanatory.

The output of `configure_params` is a list of arguments that correspond to the function of interest, e.g. `rnorm` takes in the `mean` parameter whereas `rweibull` takes in the `scale` parameter.

### `X_sim`

When generating the `X` variable via `X_sim`, balanced samples in `X` are obtained using `sample` to get exactly a 50:50 split but unbalanced design can either take in a `prob` argument such that we have `rbinom(nSubjects, 1, prob)` or default to a 25:75 split (hard-coded within the function).

### `y_sim`

Given the output from `X_sim`, generate data that correspond to the null, mean-effect, variance-effect, and mean-variance-effect models.

* *Null Model*: `rnorm(nSubjects, mean = 1, sd = 0)` (default parameters)
* *Mean Model*: `X * mean_effect_size + rnorm(nSubjects)`
* *Variance Model*: For `X=0`, `rnorm(nSubjects)`; for `X=1`, `rnorm(nSubjects, sd = variance_effect_size)`
* *Mean-Variance Model*: For `X=0`, `rnorm(nSubjects, sd = variance_effect_size)`; for `X=1`, `mean_effect_size + rnorm(nSubjects, sd = variance_effect_size)`

Additionally, the data above were normalized by the Median Absolute Deviation (MAD):
