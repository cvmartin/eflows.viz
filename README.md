
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eflows.viz <img src="man/figures/eflows_viz_logo.png" align="right" width="140" />

## Overview

eflows.viz is a visualization package for eflows. eflows generates
`e_frame` R6 objects as a result of its calculations. These objects can
be processed by additional functions to extend the data pipeline, or in
the case of eflows.viz, provide visualization and dashboarding.

## Visualizations

  - basic: `viz` + basic operation (`fore`, `back`, `sim`…) +
    explanative suffix. An specific case of suffix is `sum_*`, used for
    summaries. Example: `viz_fore_input`, `viz_fore_output`,
    `viz_back_sum_obj`

  - groups: `gviz` + explanative suffix (normally something simple, like
    `fore` or `back`). The groups bundle several visualizations in a
    list, normally with the `y` axis normalized so they look nice side
    by side.

  - vizget: when passes a `gviz` as argument, it deploys a shiny gadget
    that allows easy comparison.

## Installation

``` r
devtools::install_github("cvmartin/eflows.viz")
```
