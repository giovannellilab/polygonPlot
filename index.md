## Here comes the Polygons Plot

In case you have not had enough plot and chart types, here come a new one: the Polygon plot. A Polygon plot [_singular_, Polygons plot, _plural_] is designed to visualize multivariate ranges as opposite to single data points. I have developed the concept for the polygons plot in late 2018 while working on our review [Living at the Extremes: Extremophiles and the Limits of Life in a Planetary Context](https://www.frontiersin.org/articles/10.3389/fmicb.2019.00780/full), published in 2019 in Frontiers in Microbiology.

While working on the review, I started collecting data regarding the range (minimum and maximum) for physico-chemical parameters of Earth's life such as pH, temperature, salinity and pressure. I wanted to visualize the ranges of these variables in a single plot, but I could not find a simple way to do it. After some research and a lot of doodling, the polygons plot was born!

<img src="images/merino_frontiers.jpg" class="img-responsive" alt="Merino_et_al_2019_polygon_plot">

>The first polygons plot appeared in [Merino et al. 2019 Frontiers in Microbiology](https://www.frontiersin.org/articles/10.3389/fmicb.2019.00780/full). If you find the plot interesting or useful, consider citing the Merino et al. 2019 paper. More information about how to cite the Polygon plot are available at the section [how to cite](#how-to-cite-the-polygon-plot)

### Polygon plot features

Polygon plot have many features that makes them unique and convenient to show multivariate ranges and a number of statistical properties.

- It is possible to plot from 3 to 6 variable in a single 2D plot (potentially more, depending of big your final plot will be)
- The showed range can be the interval minimum-maximum, interquartile range or any other interval
- You can add the mean (arithmetic, geometric, etc...) as a single line running through the plot
- You can easily use the shape of the polygon plot to categorize you observations into discrete groups having a similar multivariate distribution of the ranges for the selected variables
- Soon(ish) you'll be able to plot the boxplot or density plot on the outside of each axis
- You can use the perimeter of the axis projection or the area of the polygon to sort you polygons plot array or query your visualization based on extension of the range of variables visualized

>**A word of caution:** While it defines a space on a plane that can be used for algebraic operations to work on a series of plots (called a polygons plot array), it does not define the state space of the possible combinations of the visualized variables. Visualizing the polygon plot as the state space of possible instances might seem like an intuitive thing to do, but it is inherently **_wrong_**! You are have been warned!

We are working on a R package to be able to make Polygons plot easily and add some of the functionalities we have imagined discussing these plots with Shaunna Morrison, Fang Huang and Anirudh Prabhu. For now, we have a _beta_ Shiny app built by [Fang Huang](https://people.csiro.au/H/F/f-huang) than can be used right now to create Polygons plot and download them as **.png** or **.svg**. You can find the _beta_ of the app here: [fanghuang.shinyapps.io/DonatoPlot/](https://fanghuang.shinyapps.io/DonatoPlot/).


>Fun fact! My friends refer to these plots as "Donato's plots"!

Keep reading to find out how to use and read polygons plot, how they were invented and how they stack against other popular choices for plotting multivariate data. Or just jump ahead to the section you are most interest in!

- [The inception](#the-inception)
- [Reading the Polygon plot](#reading-the-polygon-plot)
- [Using the Polygon plot: the Shiny app](#using-the-plot-the-shiny-app)
- [Gallery of Polygons](#gallery-of-polygons)
- [Polygons plot array](#polygons-plot-array)
- [List of future features](#list-of-future-features)
- [How to cite the Polygon plot](#how-to-cite-the-polygon-plot)
- [Questions, support and contribute!](#questions-support-and-contribute)

### The inception
While working on the review I started trying to put on the same plot multiple ranges of variables. The first choice was to use multiple [boxplots](https://en.wikipedia.org/wiki/Box_plot). But while they are very robust and well known, it was difficult to portray the kind of information I was looking for. Boxplot had to be grouped by variable, making across variable comparison difficult, or by group, making across group comparison difficult. A lot of boxplots were required to put all the variable together, with way to much ink on the page. Messy. I did not like it.

Searching the web I found [forest plot](https://en.wikipedia.org/wiki/Forest_plot), which in essence are a simplified form of bloxplot were the range is represented by a single line. The result is a series of parallel line (thus reducing the amount of ink, but also the amount of information reported) essentially suffering the same problems of the boxplot.

<img src="forest_plot.png" class="img-responsive" alt="forest-plot-example">
> My planetary and life data used for the Merino et al. 2019 paper visualized as a Forest plot.

The type of comparison I was looking for was similar to the information displayed by a [spider chart](https://en.wikipedia.org/wiki/Radar_chart) (called also polar chart, radar chart or star plot among many other names). While not one of my favorite, spider chart are useful to visualize multivariate data in a single plot, and give more intuitive comparison of the distribution of the data among different variables between different plots. The problem arises when for each variable you don't want just to plot a single value, but you want to show the data range (like minimum and maximum), or a combination of range and average.

<img src="spider_chart.png" class="img-responsive" alt="spider-plot-example">
> The same data as above plotted as spider plots plot. Ranges are difficult to visualize and sometimes impossible to plot, depending on the variable's order.

I started doodling through a series of option, and devising a series of rules. Soon I started converging on the idea that polygons were the best solution to visualize ranges for n-variables.

<img src="inception1.jpg" class="img-responsive" alt="doodling-the-polygon-plot" width="50" height="50">
<img src="inception2.jpg" class="img-responsive" alt="doodling-the-polygon-plot" width="50" height="50">
<img src="inception3.jpg" class="img-responsive" alt="doodling-the-polygon-plot" width="50" height="50">
<img src="inception4.jpg" class="img-responsive" alt="doodling-the-polygon-plot" width="50" height="50">

### Reading the Polygon plot

### Using the Polygon plots: the Shiny app

### Gallery of Polygons

### Polygons plot array

### List of future features

### How to cite the Polygon plot

### Questions, support and contribute!
