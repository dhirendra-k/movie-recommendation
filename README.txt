Access the shiny web app at the link below
URL# https://dhirendra-k.shinyapps.io/MovieRecommender/


To run locally, see below (need to update if we want to keep this part)

Try the commands below in the Rstudio console. I haven't included all packages that may need to be installed

> install.packages('shiny')
> install.packages('devtools')
> devtools::install_github("stefanwilhelm/ShinyRatingInput")
> shiny::runApp()

If ShinyRatingInput fails to download due to "cannot remove prior installation of package rlang"
then restart R session and try download again.



Note that it takes a while to download ratings and users. Either implement ui feature to display a download message
or make note of this in submission (takes a minute before functions can run and movie images appear)