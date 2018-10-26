# set working directory
setwd("H://Mijn documenten/Coursera Training/Reproducible Research/project1")

# load rmarkdown
library(rmarkdown)

# create md-file
render(input = 'PA1_template.rmd',
                  output_file = 'PA1_template.md',
                  output_format=   md_document())
