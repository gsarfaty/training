#glamr package: https://github.com/USAID-OHA-SI/glamr developed
# by USAID/SI

#install from github if not already installed
install.packages("devtools")
devtools::install_github("USAID-OHA-SI/glamr", force=TRUE)
devtools::install_github("USAID-OHA-SI/glitr")
devtools::install_github("USAID-OHA-SI/gisr")
devtools::install_github("ICPI/ICPIutilities")


#load packages
library(glamr)
library(here)
library(usethis)



ls(package:glamr)

# Can use the create_project command to initialize a new R project
project_path <- here()

create_project(path = project_path, open = TRUE, rstudio = TRUE)



#set up SI folders, gitignore, readme
si_setup()

setup_gitignore()