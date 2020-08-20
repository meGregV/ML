
# MLens project in R

There are four main components:
## *'MLens.modeling.R'* - main R script that pulls the data, runs analysis and outputs results.
*opts.RDS* file already contains tuning parameters stored for *recosystem* training run and 
will save you computational time if the entire repository is cloned (or could use "Download ZIP" option under big green "CODE" button).
Additionally, the script will save edx and validation data sets to working directory as .RDS files upon
initial run to avoid lengthy web data pulls on reruns.
    
## *'MLens.EDA.R'* - secondary R script containing EDA and visualization, stand along from the main script.

## *'MLens.Rmd'* -  Rmarkdown file that would knit everything together into pdf.
* *'my_header.tex'* - **won't knit without it** as it part of YAML header
* *'references.bib'*  - a bibliography file used to built REFERENCE section at the end
*  *'5 point_interval_rating.png'* - an image used by rmd
    
## *MLens.pdf* - Output pdf document (code is omitted for clarity)


