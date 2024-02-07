library(targets)
renv::restore()
# error with amt 0.1.7
install.packages("amt")

# still giving an error, try using different URL?

renv::restore(repos="https://cloud.r-project.org")

# nope still doesn't work


## Troubleshooting
getOption("download.file.method")
renv:::renv_download_file_method()
library(renv)
options(renv.download.override = utils::download.file)

tar_make()
