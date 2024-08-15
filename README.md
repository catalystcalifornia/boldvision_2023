# Bold Vision

<img src="https://www.boldvisionla.org/wp-content/uploads/2022/09/Bold-Vision-Light-Logo.png" alt="Bold Vision Logo">


<li><a href="">GitHub Pages Static Website Link</a></li>

<br>

<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a></li>
    <li><a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#data-methodology">Data Methodology</a>
      <ul>
        <li><a href="#data-dictionary">Data Dictionary</a></li>
      </ul>
    </li>
    <li><a href="#contributors">Contributors</a></li>
    <li><a href="#contact-us">Contact Us</a></li>
    <li><a href="#about-catalyst-california">About Catalyst California</a>
      <ul>
        <li><a href="#our-vision">Our Vision</a></li>
        <li><a href="#our-mission">Our Mission</a></li>
      </ul>
    </li>
    <li><a href="#citation">Citation</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#partners">Partners</a></li>
  </ol>
</details>

## About The Project

[Bold Vision](https://www.boldvisionla.org/) is a multi-sector effort to build a 10-year-plus initiative that aims to fundamentally improve the lives of a generation of BIPOC children and youth, creating lasting change in our communities by establishing new paths towards success for young people across L.A. County. [Catalyst California](https://www.catalystcalifornia.org/) has served as the lead community engagement, policy development, and research consultant for the initiative alongisde a Community Council of 13 people from color-led youth serving organizations. Member organizations of the Community Council include: SJLI (Social Justice Learning Institute), Pacoima Beautiful, Legacy La, Khmer Girls in Action, ICS (Innercity Struggle), EPIC (Empowering Pacific Islander Communities), East Yard Communities for Environmental Justice, CoCo (Community Coalition), CHIRLA (Coalition for Humane Immigrant Rights), CNVP (California Native Vote Project), Brotherhood Crusade, Advancing Communities Together, Active SGV.
<p align="right">(<a href="#top">back to top</a>)</p>


### Built with

<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/R_logo.svg/1086px-R_logo.svg.png?20160212050515" alt="R" height="32px"/> &nbsp; <img  src="https://upload.wikimedia.org/wikipedia/commons/d/d0/RStudio_logo_flat.svg" alt="RStudio" height="32px"/> &nbsp; <img  src="https://upload.wikimedia.org/wikipedia/commons/thumb/e/e0/Git-logo.svg/768px-Git-logo.svg.png?20160811101906" alt="RStudio" height="32px"/>

<p align="right">(<a href="#top">back to top</a>)</p>

## Getting Started

This is an example of how you may give instructions on setting up your project locally.
To get a local copy up and running follow these simple example steps.

### Prerequisites

We completed the data cleaning, analysis, and visualization using the following software. 
* [R](https://cran.rstudio.com/)
* [RStudio](https://posit.co/download/rstudio-desktop)

We used several R packages to analyze data and perform different functions, including the following.
* data.table 
* dplyr
* sf
* tidyr
* usethis

```
list.of.packages <- c("usethis","dplyr","data.table", "sf", tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

devtools::install_github("r-lib/usethis")

library(usethis)
library(dplyr)
library(data.table)
library(sf)
library(tidyr)
```

<p align="right">(<a href="#top">back to top</a>)</p>


### Installation

1. Add installation instructions here such as what programs, packages, and APIs to use and how to access or download them
2. Step 2
3. Step 3
4. Step 4

<p align="right">(<a href="#top">back to top</a>)</p>


## Data Methodology

Use this space to show useful examples of how a project can be used (e.g. iframes, citation, etc). Additional screenshots, code examples and demos work well in this space. You may also link to more resources.

### Data Dictionary
| field                         | type    | description                                                                                                                                                                                      |
| ----------------------------- | ------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `county`                      | string  | The name of the county.                                                                                                                                                                          |
| `total_pop`                   | integer | The total population in the county.                                                                                                                                                              |           
| `total_rate`                  | integer | The total rate for the entire population for the given [indicator](https://www.racecounts.org/issue/crime-and-justice)(ex: Total % of Adults Who Reported Feeling Safe in their Neighborhood).   | 
| `nh_white_rate`               | integer | The total rate for White people exclusive of  those that are of Hispanic origin for the given indicator (ex: Total % of White Adults Who Reported Feeling Safe in their Neighborhood.)                                                    |
| `black_diff`                  | integer | The difference in rate between Non-Hispanic Black and the group with the 'Best Rate' for the given indicator.                                                                                    |

<p align="right">(<a href="#top">back to top</a>)</p>

## Contributors

* [Alexandra Baker](https://github.com/bakeralexan)
* [Leila Forouzan](https://github.com/lforouzan)
* [Hillary Khan](https://github.com/hillaryk-ap)
* [Maria Khan](https://github.com/mariatkhan)
* [Elycia Mulholland-Graves](https://github.com/elyciamg)
* [Chris Ringewald](https://github.com/cringewald)
* [David Segovia](https://github.com/davidseg1997)
* [Jennifer Zhang](https://github.com/jzhang514)

<p align="right">(<a href="#top">back to top</a>)</p>

## Contact Us

[Chris Ringewald](https://www.linkedin.com/in/chris-ringewald-6766369/) - cringewald[at]catalystcalifornia.org  <br>


[Leila Forouzan](https://www.linkedin.com/in/leilaforouzan/) - lforouzan[at]catalystcalifornia.org

<p align="right">(<a href="#top">back to top</a>)</p>

## About Catalyst California

### Our Vision
A world where systems are designed for justice and support equitable access to resources and opportunities for all Californians to thrive.

### Our Mission
[Catalyst California](https://www.catalystcalifornia.org/) advocates for racial justice by building power and transforming public systems. We partner with communities of color, conduct innovative research, develop policies for actionable change, and shift money and power back into our communities. 

[Click here to view Catalyst California's Projects on GitHub](https://github.com/catalystcalifornia)

<p align="right">(<a href="#top">back to top</a>)</p>

## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>

## Partners

Use this space for any additional acknowledgments (project partners, etc)

* [Partner org](URL)
* [Partner org](URL)
* [Partner org](URL)

<p align="right">(<a href="#top">back to top</a>)</p>
