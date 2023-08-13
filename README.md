
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hcris-cost-reports

This repository includes code to download and process the CMS hospital
cost report data, known as HCRIS (Healthcare Cost Report Information
System). The data includes reports for hospitals that completed the 2010
version of the cost report form, from approximately 2010 through the
present.

The code (`R/hcris-script.R`) generates two forms of report data and
select variables: records from hospitals representing their own fiscal
year and records from hospitals converted to synthetic calendar years.
The synthetic data method borrows from [Adam Sacarny’s Stata
code](https://github.com/asacarny/hospital-cost-reports) to complete
this task (credit to Dr. Sacarny but any errors are mine alone).

This data in this repository is scheduled to update quarterly using
Github Actions. The resulting data (`data/costreports.rds`) is then used
in a [`shiny`
application](https://josh-fangmeier.shinyapps.io/hospital-profile/) to
generate profile views of each reporting hospital.
