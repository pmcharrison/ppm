
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ppm

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/pmcharrison/ppm.svg?branch=master)](https://travis-ci.org/pmcharrison/ppm)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/pmcharrison/ppm?branch=master&svg=true)](https://ci.appveyor.com/project/pmcharrison/ppm)
[![Coverage
status](https://coveralls.io/repos/github/pmcharrison/ppm/badge.svg)](https://coveralls.io/r/pmcharrison/ppm?branch=master)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2620414.svg)](https://doi.org/10.5281/zenodo.2620414)

Prediction by Partial Matching (PPM) is a flexible and robust algorithm
for generating incremental probabilistic predictions for symbolic
sequences (Cleary and Witten 1984). Many variants of PPM exist in the
literature. This package, `ppm`, implements the PPM variant developed by
Bunton (1996) and subsequently used by Pearce (2005) in the Information
Dynamics Of Music (IDyOM) model. It also implements the PPM-Decay
algorithm of Harrison et al. (2020), which applies a flexible
memory-decay kernel to PPM such that historic events are downweighted
compared to recent events. Please cite the `ppm` package by referring to
this latter paper:

> Harrison, Peter M. C., Roberta Bianco, Maria Chait, and Marcus T.
> Pearce. 2020. “PPM-Decay: A Computational Model of Auditory Prediction
> with Memory Decay.” *bioRxiv*.
> <https://doi.org/10.1101/2020.01.09.900266>.

You might also want to mention the particular version of the package you
used. To check the installed version, you can run the following code in
your R console:

``` r
asNamespace("ppm")$`.__NAMESPACE__.`$spec[["version"]]
```

In addition to the GitHub repository, the source code for the `ppm`
package is permanently archived on Zenodo, and is available at the
following DOI: <https://doi.org/10.5281/zenodo.2620414>. This DOI will
always point to the latest version of the `ppm` package, but you can
also find version-specific DOIs on the Zenodo page.

## Installation

To install from GitHub:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("pmcharrison/ppm")
```

## PPM

The standard PPM model is a variable-order Markov order model that
blends together the predictions of multiple *n*-gram models. An *n*-gram
model generates predictions by tabulating the occurrences of
subsequences of *n* tokens – *n*-grams – in the training dataset. These
*n*-gram models are combined using a technique called *smoothing*.

Many versions of PPM exist corresponding to different smoothing and
order selection techniques. The implementation in this package uses
*interpolated smoothing* as introduced by Bunton (1996), along with a
selection of different *escape methods* which determine the weighting
parameters for the *n*-gram models. *Update exclusion* is also
supported. The implementation also supports the order selection
technique from PPM*, which was designed to help PPM extend to
arbitrarily long *n\*-gram orders. Note however that the present
implementation is not optimized for large order bounds; with an infinite
order bound, model training has computational complexity quadratic in
the length of the input, whereas alternative implementations can achieve
linear complexity in the length of the input.

You can create a PPM model object using the `new_ppm_simple` function:

``` r
library(ppm)
mod <- new_ppm_simple(alphabet_size = 5)
```

See `?new_ppm_simple` for information on configuring the model.

Note how we provided an `alphabet_size` parameter to the model. This
compulsory parameter refers to the size of the alphabet over which the
model will be trained and will generate predictions. For example, if we
were modelling the letters A-Z, we might set this parameter to 26,
corresponding to the 26 letters in the English alphabet.

Input sequences are coded as integers within this alphabet. For example,
we might define a sequence as follows:

``` r
seq_1 <- c(1, 3, 3, 1, 2, 1, 3)
```

The `factor` function from base R can be useful for making this encoding
from a series of textual tokens. Factor objects are represented as
integer vectors under the hood, but they come with an associated mapping
between the integers and their labels, which makes them easier to
manipulate within R. For example:

``` r
print(letters)
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
seq_2 <- factor(c("a", "b", "r", "a", "c", "a", "d", "a", "b", "r", "a"),
                levels = c("a", "b", "c", "d", "r"))
print(seq_2)
#>  [1] a b r a c a d a b r a
#> Levels: a b c d r
print(as.integer(seq_2))
#>  [1] 1 2 5 1 3 1 4 1 2 5 1
```

The *ppm* package treats factor objects like their underlying integer
representations. When modelling factor objects, it’s recommended to pass
the underlying factor representation to the PPM model upon
initialisation. In this case, the `alphabet_size` parameter need not be
provided.

``` r
mod <- new_ppm_simple(alphabet_levels = c("a", "b", "c", "d", "r"))
```

You feed sequences to the PPM model using the `model_seq` function. By
default, the model processes these sequences incrementally, one symbol
at a time. It simultaneously learns from these incoming symbols, and
generates predictions for future symbols based on what came before. For
example:

``` r
res <- model_seq(mod, seq_2)
print(res)
#> # A tibble: 11 x 5
#>    symbol model_order information_content entropy distribution
#>    <fct>        <int>               <dbl>   <dbl> <list>      
#>  1 a               -1                2.32    2.32 <dbl [5]>   
#>  2 b                0                3.32    1.77 <dbl [5]>   
#>  3 r                0                3.17    2.11 <dbl [5]>   
#>  4 a                0                2       2.25 <dbl [5]>   
#>  5 c                1                3.91    1.86 <dbl [5]>   
#>  6 a                0                1.91    2.29 <dbl [5]>   
#>  7 d                1                3.58    2.18 <dbl [5]>   
#>  8 a                0                2       2.31 <dbl [5]>   
#>  9 b                1                2.20    2.30 <dbl [5]>   
#> 10 r                1                1.32    2.16 <dbl [5]>   
#> 11 a                1                1.22    2.13 <dbl [5]>
```

The output of `model_seq` is a tibble with several columns. Each row
corresponds to a different element of the input sequence, organised in
order of presentation. The row describes what happened when the model
tried to predict this element, conditioned on the preceding elements in
the sequence. Each row has the following fields:

-   `symbol` - The integer encoding of the corresponding symbol in the
    sequence.
-   `model_order` - The highest-order *n*-gram model used for generating
    predictions.
-   `information_content` - The negative log probability, or
    *information content*, of the observed symbol according to the
    model.
-   `entropy` - The entropy of the model’s predictive distribution when
    predicting that element of the sequence.
-   `distribution`- The predictive probability distribution when
    predicting that element of the sequence.

Often we are particularly interested in the `information_content` field,
which gives us an index of the model’s surprisal at different parts of
the input sequence.

``` r
plot(res$information_content,
     xlab = "Position",
     ylab = "Information content (bits)",
     type = "l", 
     ylim = c(0, 5))
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="50%" style="display: block; margin: auto;" />

The `model_seq` function changes the input PPM model object, even the
absence of the assignment operator `<-`. Typically, the PPM model will
have been updated with the contents of the training sequence. If we
present the same sequence again, it should prove to be much more
predictable (red line):

``` r
res_2 <- model_seq(mod, seq_2)
plot(res$information_content,
     xlab = "Position",
     ylab = "Information content (bits)",
     type = "l", 
     ylim = c(0, 5))
points(res_2$information_content,
       type = "l", col = "red")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="50%" style="display: block; margin: auto;" />

By setting `generate = TRUE`, we can also instruct the model to generate
new samples based on the statistics that it has learned so far. In this
case the first argument to `model_seq` should be an integer
corresponding to the desired length of the generated sequence.

``` r
res_3 <- model_seq(mod, seq = 20, generate = TRUE)
res_3$symbol
#>  [1] a b r a c a d a b r a c a d a b r a c a
#> Levels: a b c d r
```

## PPM-Decay

The original PPM algorithm has a perfect memory, and weights all
historic observations equally when generating predictions. The PPM-Decay
modifies this behaviour, introducing a customisable memory decay kernel
that determines the weight of historic observations as a function of the
time and number of events observed since the original event. For
example, a decay kernel for modelling auditory prediction might resemble
the following:

<img src="man/figures/example-decay-kernel.png" width="50%" style="display: block; margin: auto;" />

In its most general form (illustrated above), the decay kernel comprises
three phases:

-   A buffer phase (yellow);
-   A short-term memory phase (red);
-   A long-term memory phase (blue).

The parameters for these different phases, in particular durations and
relative weights, are customisable. Each phase can be disabled
separately to produce simpler families of decay kernels. For example,
the default parameters define a one-stage exponential decay kernel;
adding a buffer phase and retrieval noise produces the two-stage decay
kernel in Harrison et al. (2020).

The `new_ppm_decay` function is used to create a new PPM-Decay model. It
works in a similar way to `new_ppm_simple`, described above for PPM
models. The function has many customisable parameters (see
`?new_ppm_decay`) that support the specification of various kinds of
decay kernels, including single-stage exponential decays, two-stage
exponential decays, exponential decays with non-zero asymptotes, and
kernels combining a flat buffer period with subsequent exponential
decays.

``` r
mod_decay <- new_ppm_decay(alphabet_size = 5, ltm_half_life = 2)
```

When modelling a sequence with a PPM-Decay model, you need to specify
both the sequence itself and a numeric vector corresponding to the
timepoints of the symbol observations.

``` r
seq_2_time <- seq_along(seq_2)
print(seq_2_time)
#>  [1]  1  2  3  4  5  6  7  8  9 10 11

res_4 <- model_seq(mod_decay, 
                   seq_2,
                   time = seq_2_time)

plot(res_4$information_content,
     xlab = "Position",
     ylab = "Information content (bits)",
     type = "l", 
     ylim = c(0, 5))
points(res_4$information_content,
       type = "l", col = "blue")
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="50%" style="display: block; margin: auto;" />

Here the original PPM output is plotted in black, the PPM-Decay model in
blue.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Bunton1996" class="csl-entry">

Bunton, Suzanne. 1996. “<span class="nocase">On-line stochastic
processes in data compression</span>.” PhD dissertation, Seattle, WA:
University of Washington.

</div>

<div id="ref-Cleary1984" class="csl-entry">

Cleary, John G., and Ian H. Witten. 1984. “<span class="nocase">Data
compression using adaptive coding and partial string matching</span>.”
*IEEE Transactions on Communications* 32 (4): 396–402.
<https://doi.org/10.1109/TCOM.1984.1096090>.

</div>

<div id="ref-Harrison2020" class="csl-entry">

Harrison, Peter M. C., Roberta Bianco, Maria Chait, and Marcus T.
Pearce. 2020. “PPM-Decay: A Computational Model of Auditory Prediction
with Memory Decay.” *bioRxiv*.
<https://doi.org/10.1101/2020.01.09.900266>.

</div>

<div id="ref-Pearce2005" class="csl-entry">

Pearce, Marcus T. 2005. “<span class="nocase">The construction and
evaluation of statistical models of melodic structure in music
perception and composition</span>.” PhD thesis, London, UK: City
University.

</div>

</div>
