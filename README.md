
<!-- README.md is generated from README.Rmd. Please edit that file -->

# polygonPlot

<!-- badges: start -->

[![R-CMD-check](https://github.com/giovannellilab/polygonPlot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/giovannellilab/polygonPlot/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
<!-- badges: end -->

A **Polygon plot** (singular, Polygons plot, plural) is designed to
visualize multivariate ranges as opposite to single data points. The
concept of `polygons plot` was developed by [Donato
Giovannelli](https://www.donatogiovannelli.com/) in late 2018 while
working on our review [Living at the Extremes: Extremophiles and the
Limits of Life in a Planetary
Context](https://doi.org/10.3389/fmicb.2019.00780), published in 2019 in
Frontiers in Microbiology.

A complete explanation of how polygon plot works can be found at
<https://giovannellilab.github.io/polygonPlot/>.

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

## Installation

You can install the development version of polygonPlot from
[GitHub](https://github.com/giovannellilab/polygonPlot) with:

``` r
# install.packages("devtools")
devtools::install_github("giovannellilab/polygonPlot")
```

## Example usage

This is a basic example which shows you how to solve a common problem:

``` r
library(polygonPlot)

df <- read.csv(system.file(file.path("extdata", "example.csv"), package="polygonPlot"))
df
#>        info data1 data2 data3 data4 data5 data6
#> 1  axis_min  -300    -4     0     0    NA  -0.5
#> 2  axis_max   500    14  1000    50    NA   0.5
#> 3      data   300    -2    10    20     5   0.1
#> 4             320    12    50    40    10   0.3
#> 5             340    NA    90    NA    NA    NA
#> 6             360    NA   130    NA    NA    NA
#> 7             380    NA   170    NA    NA    NA
#> 8             400    NA   210    NA    NA    NA
#> 9             420    NA    NA    NA    NA    NA
#> 10            440    NA    NA    NA    NA    NA
#> 11            460    NA    NA    NA    NA    NA
```

``` r
plot <- polygonplot(df, shape = 4, fillcolor = "#57cc99", linecolor = "#38a3a5",
                    labels_axis = c("Earth", "Mercury", "Venus", "Mars"))
plot
```

<img src="man/figures/README-plot-1.png" width="100%" />

## Detailed Description

### Libraries

``` r
library(polygonPlot)
library(ggplot2)
```

### Input Parameters

#### Required

The `polygonplot` function requires two mandatory parameters:

- **`df`**: the input `data.frame` that should be composed as follows:
  - The first column should contain the strings `axis_min`, `axis_max`
    and `data` in that row order.
  - All the columns, starting from the second, must be dedicated to the
    variables of interest. In particular, the first two rows should
    contain the values for the `axis_min` and `axis_max` respectively.
    - If left empty, the axis_min/max will be automatically calculated
      based on data given and extension will be decided based on the
      Axes range extension (`extra` parameter).
  - Starting from the third row, each variable column must contain all
    the data values.
  - As default, column names will be used as labels of the axis
    (starting from the second one). Change them in order to change the
    labels, but don’t leave them empty.
- **`shape`**: the `integer` value indicating the shape of the polygon.
  Available numbers:
  - `3` for *Triangle*
  - `4` for *Square*
  - `5` for *Pentagon*
  - `6` for *Hexagon*

Examples of the input `data.frame`

``` r
df <- read.csv(system.file(file.path("extdata", "example.csv"), package="polygonPlot"))
df
#>        info data1 data2 data3 data4 data5 data6
#> 1  axis_min  -300    -4     0     0    NA  -0.5
#> 2  axis_max   500    14  1000    50    NA   0.5
#> 3      data   300    -2    10    20     5   0.1
#> 4             320    12    50    40    10   0.3
#> 5             340    NA    90    NA    NA    NA
#> 6             360    NA   130    NA    NA    NA
#> 7             380    NA   170    NA    NA    NA
#> 8             400    NA   210    NA    NA    NA
#> 9             420    NA    NA    NA    NA    NA
#> 10            440    NA    NA    NA    NA    NA
#> 11            460    NA    NA    NA    NA    NA
```

a minimal version

``` r
df_min <- read.csv(system.file(file.path("extdata", "minimal_example.csv"), 
                               package="polygonPlot"))
df_min
#>       info data1 data2 data3 data4 data5 data6
#> 1 axis_min  -300    -4     0     0    NA  -0.5
#> 2 axis_max   500    14  1000    50    NA   0.5
#> 3     data   300    -2    10    20     5   0.1
#> 4            460    12   270    40    10   0.3
```

#### Optional

- **`extra`**: axis range extension
- **`fillcolor`**: fill color of the polygon
- **`alpha`**: alpha value of the fill color
- **`linecolor`**: line color of the polygon border
- **`linetype`**: line type of the polygon border
- **`lwd`**: line width of the polygon border
- **`labels_axis`**: vector with the desired labels of the axis
- **`title`**: title of the plot
- **`fix_aspect_ratio`**: boolean flag to fix the aspect ratio of the
  plot as `1`. It is strongly recommended to leave it as default value
  `TRUE`.
  - **NOTE**: If you are going to change the theme of the returned
    ggplot object, remember to put in the `theme` function the following
    code `aspect.ratio = 1` in order to keep the text and the relative
    ticks aligned on the axis.

See the section `About the aspect.ratio` for a practical example.

### Return

- **`ggplot2`** object.

### Polygons

#### Shape: 3 - Triangle

``` r
plot_triangle <- polygonplot(df, shape = 3, fillcolor = "#e56b6f", linecolor = "#b56576",
                             labels_axis = c("earth", "moon", "sun"))
plot_triangle
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

#### Shape: 4 - Square

``` r
plot_square <- polygonplot(df, shape = 4, fillcolor = "#57cc99", linecolor = "#38a3a5")
plot_square
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

#### Shape: 5 - Pentagon

``` r
plot_pentagon <- polygonplot(df, shape = 5, fillcolor = "#c89f9c", linecolor = "#b36a5e")
plot_pentagon
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

#### Shape: 6 - Hexagon

``` r
plot_hexagon <- polygonplot(df, shape = 6, fillcolor = "#0077b6", linecolor = "#023e8a")
plot_hexagon
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

### About the `aspect.ratio`

As described above, the `polygonplot` function return a `ggplot2`
object. If you are going to change the theme of the returned ggplot
object, it is strongly recommended to specify in the `theme` function
the `aspect.ratio = 1` in order to keep the text and the relative ticks
aligned on the axis.

``` r
plot_pentagon <- polygonplot(df, shape = 5, fillcolor = "#c89f9c", linecolor = "#b36a5e")
plot_pentagon + theme_bw() + theme(aspect.ratio = 1)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
