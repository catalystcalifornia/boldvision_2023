# Bold Vision 2024

<img src="https://www.boldvisionla.org/wp-content/uploads/2022/09/Bold-Vision-Light-Logo.png" alt="Bold Vision Logo">

<br>

<details>
  <summary>Table of Contents</summary>
  <ol>
    <li> <a href="#about-the-project">About the Project</a></li>
    <li> <a href="#acknowledgement-and-partners">Acknowledgement and Partners</a>
    <li> <a href="#about-the-data">About the Data</a>
    <li> <a href="#getting-started">Getting Started</a>
    <li> <a href="#contributors">Contributors</a>
    <li> <a href="#contact-us">Contact Us</a>
    <li> <a href="#citation">Citation </a> 
    <li> <a href="#about-catalyst-california">About Catalyst California</a> 
    <li> <a href="#license">License </a> 
  
  </ol>
</details>

# About the Project

[Bold Vision](https://www.boldvisionla.org/) is a multi-sector effort to build a 10-year-plus initiative that aims to fundamentally improve the lives of a generation of BIPOC children and youth, creating lasting change in our communities by establishing new paths towards success for young people across L.A. County. Thanks to decades of power-building, Los Angeles is reaching a tipping point, with the time coming near when organized youth power can dictate that public systems transform themselves into ones of support, not suppression. This tipping point is an opportunity for youth of color and their adult allies to eliminate these inequities. The task before us is to follow the lead of youth of color, and commit to the investments, the advocacy, and the power building needed to finally uproot it from our public systems. 

Data is a vital tool to measure how youth of color are thriving in Los Angeles or lacking the resources and guidance in order to become thriving adults. Bold Vision released it's first data-driven report measuring youth thriving in Los Angeles in May of 2021 that can be viewed at the bottom of this webpage [here](https://www.boldvisionla.org/issue/). This year in 2024, the indicator report has been updated with the latest data available and a couple of new indicators across the five domains of interest: Positive Youth Development, Healthy Built Environment, Systems Impact, Youth Power and Community Organizing and Power Building. See the updated indicator report [here](INSERT LINK HERE). 

# Acknowledgement and Partners

[Catalyst California](https://www.catalystcalifornia.org/) has served as the lead community engagement, policy development, and research consultant for the initiative alongisde a Community Council of 13 people from color-led youth serving organizations. The collobration and insights frm this council is monumental in shaping and developing Bold Vision with intentionality and rigor. Member organizations of the Community Council include: 
* [SJLI (Social Justice Learning Institute)](https://sjli.org/)
* [Pacoima Beautiful](https://www.pacoimabeautiful.org/)
* [Legacy La](https://www.legacyla.org/)
* [Khmer Girls in Action](https://kgalb.org/)
* [ICS (Innercity Struggle)](https://kgalb.org/)
* [EPIC (Empowering Pacific Islander Communities)](https://www.empoweredpi.org/)
* [East Yard Communities for Environmental Justice](https://eycej.org/)
* [CoCo (Community Coalition)](https://cocosouthla.org/)
* [CHIRLA (Coalition for Humane Immigrant Rights)](https://www.chirla.org/)
* [CNVP (California Native Vote Project)](https://canativevote.org/)
* [Brotherhood Crusade](https://www.brotherhoodcrusade.org/)
* [Advancing Communities Together](https://act4avyouth.org/)
* [Active SGV](https://www.activesgv.org/)

<p align="right">(<a href="#top">back to top</a>)</p>

# About the Data 

## Indicators Overview
The Bold Vision report measures 19 indicators from five different domains to wholistically gather the state of youth in Los Angeles County. The selection of Bold Vision’s indicators was guided by an intentional and iterative process with research experts, community-based organizations, and youth prior to the launch report. We analyze these indicators and their respective domains to track how all youth are supported in reaching their fullest potential and the degree of disparity between groups in L.A. County. Each indicator is processed by race group and regional group. Additionaly, an index of disparity calculation allows us to see how youth in varying groups compare against one another, this highlights the disparities amidst youth. See below each domain and the indicator associated: 

| Domain                         | Description    | Indicators                                                                                                                                                                                      |
| ----------------------------- | ------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Systems Impact | In any youth’s life, there are events that can throw them off track. But the systems that respond to crisis points for youth of color must center their well-being. | Systems Impact Index of Disparity by Subgroup and SPA, Graduation Rates of Systems-Impacted Youth, Youth Arrests, Youth Diverted from Arrests, Youth affected by Immigration Systems (Undocumented), Youth in Foster Care or Probation |
| Healthy Built Environment | For youth of color to thrive, they need to live, learn, and play in thriving resource rich environments. | Healthy Built Environment Index of Disparity by Subgroup and SPA, Lack of Green Space, Pollution Exposure around Sensitive Land Uses, Traffic Fatalities/Injuries, Rent Burden, Access to Fresh Fruits and Vegetables |           
| Positive Youth Development | Positive youth development encompasses several distinct spheres: education, leadership development, employment in a career, and the physical and mental health supports youth of color need to develop into thriving adults. | Positive Youth Development Index of Disparity by Subgroup and SPA, Asthma Rates, Good/Excellent Health Status, Access to Needed Medical Care, Connected Youth, Early Childhood Education Access | 
| Youth Power | Youth of color are the ones most familiar with the challenges, hopes, and opportunities that confront them – and so the greater the voice they have in public decision-making, the closer we will be to solutions. | Youth Power Index of Disparity by Subgroup and SPA, Youth Receiving a Living Wage, Access to Grassroots/Base-Building Organizations, Political Engagement and Advocacy, Youth Voter Turnout |
| Community Organizing and Power Building | Over the past decades Los Angeles’s youth of color and adult allies have built a strong ecosystem of community organizations in recognition that the right research and the right policy by themselves are not sufficient to achieve change: indeed, the only true pre-requisite for transformation is organized community power. | Average Budget of Grassroots/Base-Building Organizations, Tenure of Grassroots/Base-Building Organizations |

## Methodolody 
To learn more about the detailed methodology of our Bold Vision indicators, the data limitations of this report and how the index of disparity is calculated- please see the methodology documentation here: 

# Getting Started

## Prerequisites

We completed the data cleaning, analysis, and visualization using the following software. 
* [R](https://cran.rstudio.com/)
* [RStudio](https://posit.co/download/rstudio-desktop)

We used several R packages to analyze data and perform different functions, including the following.
* dplyr
* sf
* tidyr
* usethis
* leaflet
* RPostgreSQL
* readxl
* stringr
* rpostgis
* ggplot

```
list.of.packages <- c("usethis","dplyr","data.table", "sf", tidyr","RPostgreSQL","readxl","stringr","sf", "ggplot2", "flextable", "ggchicklet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

devtools::install_github("r-lib/usethis")

library(usethis)
library(dplyr)
library(sf)
library(tidyr)
library(RPostgreSQL)
library(readxl)
library(stringr)
library(sf)
library(ggplot2)
library(flextable)
library(ggchicklet)

```

<p align="right">(<a href="#top">back to top</a>)</p>


# Contributors

* [Alexandra Baker](https://github.com/bakeralexan)
* [Hillary Khan](https://github.com/hillaryk-ap)
* [Maria T. Khan](https://github.com/mariatkhan)
* [Elycia Mulholland-Graves](https://github.com/elyciamg)
* [Chris Ringewald](https://github.com/cringewald)

<p align="right">(<a href="#top">back to top</a>)</p>

# Contact Us

[Elycia Mulholland-Graves](https://www.linkedin.com/in/elycia-mulholland-graves-54578258/) - egraves[at]catalystcalifornia.org  <br>

[Maria T. Khan](https://www.linkedin.com/in/mariatkhan/) - mkhan[at]catalystcalifornia.org 

<p align="right">(<a href="#top">back to top</a>)</p>

# Citation
To cite Bold Vision, please use the following:

Catalyst California; BOLD VISION, boldivisionla.org, [current year].

# About Catalyst California

## Our Vision
A world where systems are designed for justice and support equitable access to resources and opportunities for all Californians to thrive.

## Our Mission
[Catalyst California](https://www.catalystcalifornia.org/) advocates for racial justice by building power and transforming public systems. We partner with communities of color, conduct innovative research, develop policies for actionable change, and shift money and power back into our communities. 

[Click here to view Catalyst California's Projects on GitHub](https://github.com/catalystcalifornia)

<p align="right">(<a href="#top">back to top</a>)</p>

# License

Distributed under the General Public Use and Creative Commons Licenses. See `LICENSE.txt` and `CC_LICENSE.md` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>
