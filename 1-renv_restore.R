# 13 Feb 2024
# Jack Hendrix
# iSSA workflow

# Set up ----
# Targets is a package that organizes all of your code into discrete chunks that relate to one another via an overall _targets.R code
# this makes it easier to change something earlier in the process (e.g. bring in new data, use a different landcover, etc.) without breaking everything in subsequent scripts
# the _targets.R file defines which data are brought in, what functions are done to those, and then all models/figures/etc. are discrete outputs that can be called upon

# renv is a package that creates a snapshot of all the package versions used when creating the project
# this means that anyone who downloads the code can use renv::restore() to update their library of packages to the same ones by the original author, and avoid errors that arise from having different versions of any given package
# as long as you have the same up-to-date version of R (4.3) and RTools as I do (https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html), you should be able to just run a couple lines of code and everything should work smoothly

# renv ----

renv::activate()
# your computer will have a global library of packages used in R, but this line sets the library for this project to the local library stored inside the renv folder, so it will use the packages associated with this specific project rather than your existing version

renv::restore()
# this checks that all of the packages you need, which are listed in the renv.lock file, are contained in your project library. The Git repository should have an empty library, so it will have to go through and download all of those packages. This might take a while - it took about 15min on my laptop

# if there are any errors in this process and it fails to download any packages, you can try to run
install.packages("packagename")
# to install that one manually, and then try renv::restore() again
