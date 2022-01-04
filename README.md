# A series of quality assurance and validation checks to assess UNHCR's statistics

This package connects directly to the internal UNHCR Platform used to consolidate Forcible Population Displacement Statistics.

It performs a series of consistency check through functions on specific areas: 
 * Aggregation type
 * Consistency between datasets
 * Consistency over time
 * Data submissions
 * Demographic
 * Missing data
 * Quality
 * Sub-national data


Each function is based on country, year and whether it is a mid=-year or end-year report.
Each function output:
 * a summary output of the check
 * potentially a cleaned version based on the check
 

A wrapper function conveniently bring all those checks to create a summary quality assurance report by country. 



## Contribution are welcome

Contribution - specifically creation of new vignettes are welcome - please fork and submit pull request.

```{r}
devtools::document()
pkgdown::build_site()
```
