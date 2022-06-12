
# load_all() from devtools works as if you build install build and call the package
# Then it can find functions from other scripts within package or DESPRIPTION file

# library(devtools)
# devtools::load_all()

# Dependencies on other packages that will be loaded in
# 'DESCRIPTION' under 'Depends'
# Add via use_package("package_name")

db <- load.data('Test_with_mistakes_small','csv')
View(db)
