utils::sessionInfo()
styler::style_pkg(dry = "off")
devtools::load_all()
# Configure package documentation
usethis::use_roxygen_md()
# roxygen2::roxygenize()
# Set up package documentation
usethis::use_package_doc()
devtools::document() # Wrapper for roxygen2::roxygenize()
# Instalar localmente
# devtools::check()
# devtools::build()
# install.packages("../RElab_0.0.0.9000.tar.gz", repos = NULL, type = "source")
devtools::install(upgrade = "never")

pkgdown::build_site()
library(fs)
source_folder <- "docs"
destination_folder <- "../../datalab-icman.github.io/_site/datalabR"
# Use dir_copy for recursive directory copying
fs::dir_copy(path = source_folder,
             new_path = destination_folder,
             overwrite = TRUE)
message("Folder copied using fs::dir_copy.")



