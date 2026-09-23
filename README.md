
# saferMake <a href="https://safer-r.github.io/saferMake">[<img src="man/figures/new_saferMake.png" align="right" height="140" />](https://safer-r.github.io/saferMake/)</a>

<br />

<!-- badges: start -->

[![CRAN status](https://img.shields.io/cran/v/saferMake.svg)](https://cran.r-project.org/package=saferMake)
[![R Version](https://img.shields.io/badge/R-%3E%3D%204.6.0-blue?logo=R)](https://cran.r-project.org/)
[![Downloads](https://cranlogs.r-pkg.org/badges/saferMake)](https://r-pkg.org/pkg/saferMake)<br />
[![Rworkflows](https://github.com/safer-r/saferMake/actions/workflows/rworkflows.yml/badge.svg)](https://github.com/safer-r/saferMake/actions/workflows/rworkflows.yml)
[![Codecov](https://codecov.io/github/safer-r/saferMake/coverage.svg?branch=master)](https://app.codecov.io/github/safer-r/saferMake?branch=master)
[![safer-R Status](https://img.shields.io/badge/Safer--R%20status-backbone%20v19.3-brightgreen)](https://github.com/safer-r/.github/blob/main/profile/backbone.R)<br />
[![](https://img.shields.io/badge/license-GPL3.0-lightgrey.svg)](https://opensource.org/license/gpl-3.0)
<!-- badges: end -->

<br />

## Table of content

   - [Description](#description)
   - [Content](#content)
   - [Versions](#versions)
   - [Installation](#installation)
   - [How to run](#how-to-run)
   - [Docker](#docker-image)
   - [Licence](#licence)
   - [Citations](#citations)
   - [Credits](#credits)
   - [acknowledgments](#acknowledgments)

<br />


## Description

Shiny app to convert a R function to a safer-r function, according to the [safer-R project](https://github.com/safer-r) specifications.

<br />

## Content
<br />

Read this [webpage](https://safer-r.github.io/saferMake) for more details.

<br />

<br />

## Versions

The different *saferMake* releases are tagged [here](https://github.com/safer-r/saferMake/tags).

<br />

## Installation

```r
remotes::install_github("https://github.com/safer-r/saferMake") # later install.packages("saferMake")
```

Older versions can be installed like this:

```r
v <- "v1.0" # desired tag version
remotes::install_github(paste0("https://github.com/safer-r/saferMake", "/tree/", v))
```

 ## How to run

In a R session, after package installation :

```r
saferMake::make()
```

Downloading the repo :

```r
shiny::runApp("C:/Users/gmillot/Documents/Git_projects/safer-r/saferMake/inst/app/")
```

## Docker image


<br />

## Licence

This package can be redistributed and/or modified under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
Distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose.
See the GNU General Public License for more details at https://www.gnu.org/licenses or in the Licence.txt attached file.

<br />

## Citation

If you are using functions of *saferMake*, please cite: 

> Millot GA (2026). _The R saferMake package_.
> <https://github.com/safer-r/saferMake/>.

<br />

## Credits

[Gael A. Millot](https://github.com/gael-millot), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

<br />

## Acknowledgments

The developers & maintainers of the mentioned softwares and packages, including:

- [R](https://www.r-project.org/)
- [Git](https://git-scm.com/)
- [GitHub](https://github.com/)
- [rworkflows](https://github.com/neurogenomics/rworkflows)
- [tidyverse](https://ggplot2.tidyverse.org/)
- [posit](https://posit.co/)

