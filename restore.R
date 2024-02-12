# Had to mess around t get all the correct package versions so tar_make() will run correctly

library(targets)
renv::restore()
# error with amt

# activate() ensures that the project library is used when restoring, not the global library (i think??)
renv::activate()

## Troubleshooting

library(renv)

# try initializing first?
renv::init()

renv::restore()

tar_make()

# had to manually install amt for it to work
