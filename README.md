
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ppm

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/pmcharrison/ppm.svg?branch=master)](https://travis-ci.org/pmcharrison/ppm)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/pmcharrison/ppm?branch=master&svg=true)](https://ci.appveyor.com/project/pmcharrison/ppm)
[![Coverage
status](https://coveralls.io/repos/github/pmcharrison/ppm/badge.svg)](https://coveralls.io/r/pmcharrison/ppm?branch=master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2620414.svg)](https://doi.org/10.5281/zenodo.2620414)

The `ppm` package implements variants of the Prediction by Partial
Matching (PPM) algorithm as originally introduced by Cleary and Witten
(1984), subsequently developed by Bunton (1996), and reviewed by Pearce
(2005).

## Installation

To install from GitHub:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("pmcharrison/ppm")
```

## Example usage

### Original PPM

``` r
library(ppm)
seq <- data.frame(
  pos = 1:6,
  time = 1:6,
  val = c(1, 2, 1, 2, 1, 1)
)
mod <- new_ppm_simple(alphabet_size = 3)
res <- model_seq(mod, seq$val, return_distribution = FALSE)

plot(seq$pos, res$information_content,
     xlab = "Position",
     ylab = "Information content")
```

<img src="man/figures/README-unnamed-chunk-1-1.png" width="50%" style="display: block; margin: auto;" />

### PPM-Decay

``` r
mod_2 <- new_ppm_decay(alphabet_size = 3)
res_2 <- model_seq(mod_2, seq$val, time = seq$time,
                   return_distribution = FALSE)

plot(seq$pos, res_2$information_content,
     xlab = "Position",
     ylab = "Information content")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="50%" style="display: block; margin: auto;" />

## References

<div id="refs" class="references">

<div id="ref-Bunton1996">

Bunton, Suzanne. 1996. “On-line stochastic processes in data
compression.” PhD dissertation, Seattle, WA: University of Washington.

</div>

<div id="ref-Cleary1984">

Cleary, John G., and Ian H. Witten. 1984. “Data compression using
adaptive coding and partial string matching.” *IEEE Transactions on
Communications* 32 (4): 396–402.
<https://doi.org/10.1109/TCOM.1984.1096090>.

</div>

<div id="ref-Pearce2005">

Pearce, Marcus T. 2005. “The construction and evaluation of statistical
models of melodic structure in music perception and composition.” PhD
thesis, London, UK: City University.

</div>

</div>
