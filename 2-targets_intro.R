# 13 Feb 2024
# Jack Hendrix
# targets() analysis

## How-to targets() ----

# Further reading, if you like:
# https://books.ropensci.org/targets/
# https://ropensci.org/blog/2021/02/03/targets/


# Assuming that renv() worked correctly, you should be able to run through the template iSSA analysis of fishers in New York state with the following two lines of code:
library(targets)
tar_make()

## How does targets work? ----

# _targets.R is sort of an organizational script that uses functions to do all of the analysis that normally you would have to do manually
# it refers to functions stored in the R subfolder
# tar_make() actually runs the code through all of the functions contained in the R folder
## which means if you write any additional scripts to explore or test things out (like this one), you should save them in the main project folder (like where this file is saved). If you put them in the R subfolder, it will cause errors.

## The 'grammar' of targets() ----

# In the _targets.R file, starting on line 62, it begins reading in the data that will be used, and on line 98 it begins applying functions to the data to tidy/prepare/build models using them

# targets works with a system of inputs, functions, and outputs. These inputs and outputs are called targets (confusing, I know), and named in the parentheses following tar_target

# For example, looking at line 158 in _targets.R
# dist_parameters is the target being created here, the output - that's what we're getting out of this chunk
# calc_distribution_parameters is the function being applied, which is stored in the R folder as its own script
# tracks_random is the input to this function, which was defined earlier and created as a target of a previous function
# you take tracks_random, apply calc_distribution_parameters to it, and you get dist_parameters. You can then take dist_parameters and apply other functions to it to get new targets.


## Visualizing connections between targets ----

# You can visualize the workflow using
tar_visnetwork()
# which will show you how targets relate to one another, via which functions, and if there are any breaks in the workflow


## Loading and viewing targets ----

# tar_make() will run the code, but it doesn't automatically show you the output of the code. So it will run models and build figures, but until you call for those targets directly, they're sort of hidden in the background

# If you want to see a specific part of the workflow but not necessarily load it as an object into your environment, use tar_read()
tar_read(plot_boxplot)
# this will pull up the boxplot from line 211 of _targets.R, which is based on the code in plot_boxplot_horiz.R

# if you want to load an object into your local environment, use tar_load()
tar_load(dist_parameters)

# roughly speaking, tar_read() is useful for figures, tar_load() is useful for data frames


## Computational efficiency ----

# targets is "smart" in that it does not re-run code if nothing is changed
# E.g. if you change the colours of the figure in the plot_boxplot_horiz.R script, but don't touch the rest of the code, when you run tar_make() it will only update that target
# aka, it doesn't bother rerunning all the identical code, which saves a ton of computational time and effort - very nice for complex iSSA models.


# If you want to just run certain parts of the workflow to get a specific output, e.g. you just want the model_forest from line 190 of _targets.R, you can tar_make() a specific target with
tar_make(model_forest)

# this will only run the parts of the workflow that are necessary precursors to model_forest, and ignore the rest, even if you've changed those scripts as well

# AKA, if you only change one script, and everything else is untouched, then tar_make() and tar_make(specific_object) will do the exact same thing


## Troubleshooting errors ----

# calling for specific targets can be useful if you know part of your code is broken, but you still want to work on other parts

# KEY: if there are any errors anywhere in the workflow, then tar_make() will error out and nothing will work
# even if it's the very final target that's got an error, if you run tar_make(), it won't let you get any of the preceding objects either

# but if you know of an object earlier in the workflow than that broken one, you can use tar_make(earlier_object), and it will only run the code up until that point
# and so it will not give an error, so you can still work on the initial steps
