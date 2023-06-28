
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Polygons plot

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- [![Bioc release status](http://www.bioconductor.org/shields/build/release/bioc/polygonsplot.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/polygonsplot)  -->
<!-- [![Bioc devel status](http://www.bioconductor.org/shields/build/devel/bioc/polygonsplot.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/polygonsplot)  -->
<!-- [![Bioc downloads rank](https://bioconductor.org/shields/downloads/release/polygonsplot.svg)](http://bioconductor.org/packages/stats/bioc/polygonsplot/)  -->
<!-- [![Bioc support](https://bioconductor.org/shields/posts/polygonsplot.svg)](https://support.bioconductor.org/tag/polygonsplot)  -->
<!-- [![Bioc history](https://bioconductor.org/shields/years-in-bioc/polygonsplot.svg)](https://bioconductor.org/packages/release/bioc/html/polygonsplot.html#since)  -->
<!-- [![Bioc last commit](https://bioconductor.org/shields/lastcommit/devel/bioc/polygonsplot.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/polygonsplot/)  -->
<!-- [![Bioc dependencies](https://bioconductor.org/shields/dependencies/release/polygonsplot.svg)](https://bioconductor.org/packages/release/bioc/html/polygonsplot.html#since) -->

<!-- badges: end -->

A **Polygon plot** (singular, Polygons plot, plural) is designed to
visualize multivariate ranges as opposite to single data points. The
concept of `polygons plot` was developed by [Donato
Giovannelli](https://www.donatogiovannelli.com/) in late 2018 while
working on our review [Living at the Extremes: Extremophiles and the
Limits of Life in a Planetary
Context](https://doi.org/10.3389/fmicb.2019.00780), published in 2019 in
Frontiers in Microbiology.

<details>
<summary>
Story Details
</summary>

While working on the review, he started collecting data regarding the
range (minimum and maximum) for physico-chemical parameters of Earth’s
life such as pH, temperature, salinity and pressure. He wanted to
visualize the ranges of these variables in a single plot, but I could
not find a simple way to do it. After some research and a lot of
doodling, the polygons plot was born!

<img src="https://www.frontiersin.org/files/Articles/447668/fmicb-10-00780-HTML-r2/image_m/fmicb-10-00780-g002.jpg" class="img-responsive" alt="Merino_et_al_2019_polygon_plot">

</details>

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install `polygonsplot` from
[Bioconductor](http://bioconductor.org/) using the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("polygonsplot")
```

The development version of **polygonsplot** can be installed from
[GitHub](https://github.com/giovannellilab/polygonsplot) with:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
    
BiocManager::install("giovannellilab/polygonsplot")

## Example

This is a basic example which shows you how to solve a common problem:
```

``` r
library("polygonsplot")
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub!

## Citation

Below is the citation output from using `citation('polygonsplot')` in R.
Please run this yourself to check for any updates on how to cite
**polygonsplot**.

``` r
# print(citation('polygonsplot'), bibtex = TRUE)
```

Please note that the `polygonsplot` was only made possible thanks to
many other R and bioinformatics software authors, which are cited either
in the vignettes and/or the paper(s) describing this package.

## Code of Conduct

Please note that the `polygonsplot` project is released with a
[Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Development tools

- Continuous code testing is possible thanks to [GitHub
  actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
  through *[usethis](https://CRAN.R-project.org/package=usethis)*,
  *[remotes](https://CRAN.R-project.org/package=remotes)*, and
  *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)* customized
  to use [Bioconductor’s docker
  containers](https://www.bioconductor.org/help/docker/) and
  *[BiocCheck](https://bioconductor.org/packages/3.17/BiocCheck)*.
- Code coverage assessment is possible thanks to
  [codecov](https://codecov.io/gh) and
  *[covr](https://CRAN.R-project.org/package=covr)*.
- The code is styled automatically thanks to
  *[styler](https://CRAN.R-project.org/package=styler)*.
- The documentation is formatted thanks to
  *[devtools](https://CRAN.R-project.org/package=devtools)* and
  *[roxygen2](https://CRAN.R-project.org/package=roxygen2)*.

For more details, check the `dev` directory.

This package was developed using
*[biocthis](https://bioconductor.org/packages/3.17/biocthis)*.
