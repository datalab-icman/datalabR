#' @title A simple custom wrapper to create, configure and tune a workflowr project
#'
#' @description This function automates the creation of a workflowr project.
#' It initializes the project, configures GitLab/GitHub, creates and overwrites
#' specific R Markdown files with predefined templates, replaces placeholders,
#' and publishes the initial site.
#'
#' @param project_name Character. The name of the project and repository.
#' @param existing Character. Checks if the package already exists.
#' @param username Character. Your username on the GitLab/GitHub instance.
#' @param creator Character. The name of the author or entity creating the project.
#' @param domain Character. The domain of the GitLab/GitHub instance (e.g., "git.csic.es").
#' @param repo Character. The type of the repository to commit. Must be one of
#' c("GitHub", "GitLab")
#' @param attribution Character. Attribution text for the license file.
#' @param lic Character. The license to apply. Must be one of "CCBY-NC-SA4.0" (default)
#' or "gnu-gpl-v3".
#'
#' @return Invisibly returns the project path upon successful creation.
#' @export
#'
#' @examples
#' \dontrun{
#' wflow_datalabr(
#'   project_name = "my_awesome_project",
#'   username = "Data Lab ICMAN",
#'   creator = "Data Lab ICMAN",
#'   domain = "git.csic.es",
#'   repo = "GitLab",
#'   attribution = "Data Lab ICMAN",
#'   lic = "CCBY-NC-SA4.0"
#' )
#' }
wflow_datalabr <- function(project_name,
                           existing = FALSE,
                           username,
                           creator,
                           domain = NULL,
                           repo = c("GitHub", "GitLab"),
                           attribution,
                           lic = c("CCBY-NC-SA4.0", "gnu-gpl-v3")) {
  # --- 0. Validations and Setup ---

  # Check if workflowr is installed
  if (!requireNamespace("workflowr", quietly = TRUE)) {
    stop("\u26d4\ufe0f Package 'workflowr' is required but not installed. Please install it with install.packages('workflowr').")
  }

  # Helper function for text replacement
  replace_in_file <- function(file_path, pattern, replacement) {
    if (!file.exists(file_path)) {
      warning(sprintf("\u26a0\ufe0f  File not found, skipping replacement: %s", file_path))
      return(invisible(NULL))
    }
    content <- readLines(file_path, warn = FALSE)
    new_content <- gsub(pattern, replacement, content, fixed = TRUE)
    writeLines(new_content, file_path)
  }

  # Calculate derived variables
  href <- paste0("https://", domain, "/", username, "/", project_name)

  # --- 1. Create Workflowr Project ---
  cat("\n---\n")
  cat("STEP 1: CREATING WORKFLOWR PROJECT\n")
  cat("---\n")

  cat(sprintf("\u2699\ufe0f  Creating new workflowr project: '%s'...\n", project_name))
  # Set the working directory to the project folder upon creation
  original_wd <- getwd()
  on.exit(setwd(original_wd)) # Ensure we return to original wd on exit/error

  workflowr::wflow_start(project_name, change_wd = TRUE, existing = existing)
  cat(sprintf("\u2705 Project '%s' created successfully.\n", project_name))

  # Ensure the selected repository is valid
  repo <- match.arg(repo)

  if (repo == "GitHub") {
    cat("\u2699\ufe0f  Configuring GitHub remote repository...\n")
    workflowr::wflow_git_config(user.name = username, user.email = domain, overwrite = TRUE)
    cat("\u2705 GitHub remote configured.\n")
  }

  if (repo == "GitLab") {
    cat("\u2699\ufe0f  Configuring GitLab remote repository...\n")
    workflowr::wflow_use_gitlab(username = username, repository = project_name, domain = domain)
    cat("\u2705 GitLab remote configured.\n")
  }

  # --- 2. Create/Modify Files ---
  cat("\n---\n")
  cat("STEP 2: CREATING AND WRITING PROJECT FILES\n")
  cat("---\n")

  # Content for _site.yml
  site_yml_content <- "name: PROJECT_NAME
output_dir: ../public
navbar:
  title: PROJECT_NAME
  left:
    - text: Home
      href: index.html
    - text: About
      href: about.html
    - text: Methods
      href: Methods.html
    - text: Results
      href: Results.html
    - text: License Agreement
      href: license.html
  right:
    - icon: fab fa-gitlab
      text: Source code
      href: HREF
output:
  workflowr::wflow_html:
    toc: yes
    toc_float: yes
    theme: cosmo
    highlight: textmate"

  # Content for Methods.Rmd and Results.Rmd
  rmd_template_content <- '---
title: "TITLE"
author: CREATOR
date: "TODAYSDATE"
output: workflowr::wflow_html
editor_options:
  chunk_output_type: console
---

## Introduction

```{r}

```'

  # Ensure the selected license is valid
  lic <- match.arg(lic)

  # Content for license.Rmd based on 'lic' argument
  if (lic == "CCBY-NC-SA4.0") {
    license_url <- "https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode.txt"
    license_rmd_content <- '---
title: "License Agreement"
output:
  workflowr::wflow_html:
    toc: false
editor_options:
  chunk_output_type: console
---

<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/">
<span property="dct:title">The PROJECT_NAME</span> project was created by <span property="cc:attributionName">ATTRIBUTION</span> and is licensed under <a href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">Attribution-NonCommercial-ShareAlike 4.0 International<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg"></a></p>

```{r, results=\'asis\', echo=FALSE}
# URL of the plain text file
file_url <- "https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode.txt"

# Read the lines of the text file from the URL
# A tryCatch block is used to handle potential errors if the URL is unavailable
text_content <- tryCatch({
readLines(url(file_url), warn = FALSE)
}, error = function(e) {
"Error trying to read the URL. Please check the connection and the address."
})

# Print the content as plain text
# Using cat() with results=\'asis\' is the key to pasting the text
cat(text_content, sep = \'\\n\')
```'
  } else { # gnu-gpl-v3
    license_url <- "https://www.gnu.org/licenses/gpl-3.0.txt"
    license_rmd_content <- '---
title: "License"
output:
  workflowr::wflow_html:
    toc: false
editor_options:
  chunk_output_type: console
---

<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/">
<span property="dct:title">The PROJECT_NAME</span> project was created by <span property="cc:attributionName">ATTRIBUTION</span> and is licensed under a <a href="https://www.gnu.org/licenses/gpl-3.0.html" target="_blank" rel="license noopener noreferrer" style="display:inline-block;"> GNU General Public License, v3 or later<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"
src="https://www.gnu.org/graphics/gplv3-or-later.svg"></a></p>

```{r, results=\'asis\', echo=FALSE}
# URL of the plain text file
file_url <- "https://www.gnu.org/licenses/gpl-3.0.txt"

# Read the lines of the text file from the URL
# A tryCatch block is used to handle potential errors if the URL is unavailable
text_content <- tryCatch({
readLines(url(file_url), warn = FALSE)
}, error = function(e) {
"Error trying to read the URL. Please check the connection and the address."
})

# Print the content as plain text
# Using cat() with results=\'asis\' is the key to pasting the text
cat(text_content, sep = \'\\n\')
```'
  }

  # Write files
  cat("\u2699\ufe0f  Writing/overwriting configuration and Rmd files...\n")
  writeLines(site_yml_content, "analysis/_site.yml")

  results_content <- gsub("TITLE", "Results", rmd_template_content, fixed = TRUE)
  writeLines(results_content, "analysis/Results.Rmd")

  methods_content <- gsub("TITLE", "Methods", rmd_template_content, fixed = TRUE)
  writeLines(methods_content, "analysis/Methods.Rmd")

  writeLines(license_rmd_content, "analysis/license.Rmd")
  cat("\u2705 Project files created successfully.\n\n")

  # Download and save the full license text to the root LICENSE file
  cat(sprintf("\u2699\ufe0f  Downloading license text from %s...\n", license_url))
  license_full_text <- tryCatch(
    {
      readLines(url(license_url), warn = FALSE)
    },
    error = function(e) {
      "Failed to download license text. Please check your internet connection."
    }
  )
  writeLines(license_full_text, "LICENSE")
  cat("\u2705 Full license text saved to 'LICENSE' file.\n")

  # --- 3. Replace Placeholders ---
  cat("\n---\n")
  cat("STEP 3: REPLACING PLACEHOLDERS IN FILES\n")
  cat("---\n")

  disclaimer_text <- '## **DISCLAIMER**

#### End-to-end management, analysis, and computational modeling of multi-source marine and ecological datasets.

---

The **Data Lab ICMAN** is dedicated to providing high-quality, objective, and rigorously performed consulting, advisory, and data analysis services in the fields of statistical and mathematical methods. Our commitment is to ensure the utmost quality and objectivity in the execution of the services for which we are contracted, guaranteeing the reproducibility across operating systems and streamlining the traceability of the full version tree of produced material.

### Service quality and objectivity guarantee:

We guarantee that all methodologies, analyses, and reports delivered are prepared with professional diligence, based on accepted statistical and mathematical principles, and reflect the objective findings derived from the data and scope provided by the client.

---

### Limitation of liability and client responsibility:

1.  **Use of Results:** The client acknowledges and agrees that the results, advice, analyses, reports, and recommendations provided by **Data Lab ICMAN** are based solely on the information and data supplied by the client and are intended only to inform and guide decision-making.
2.  **Disclaimer of Warranties on Outcomes:** **Data Lab ICMAN** makes no warranty, express or implied, regarding the commercial viability, legal consequences, or ultimate outcome resulting from the client\'s use of our services or reports. We do not guarantee specific business, financial, or other results.
3.  **Client\'s Exclusive Responsibility:** The client shall bear sole and exclusive responsibility for the application, interpretation, implementation, and any consequences arising from the use or misuse of the data, analyses, advice, or results provided. This includes, but is not limited to, all decisions, actions, policies, or communications made by the client or any third party relying on the service outputs.
4.  **Indemnification:** The client agrees to indemnify and hold harmless **Data Lab ICMAN** and its employees, agents, and affiliates from and against any and all claims, liabilities, losses, damages, costs, and expenses arising out of or in connection with the client\'s misuse, unlawful use, or unauthorized reliance on the deliverables.
5.  **Scope of Service:** Our liability shall be limited strictly to the fee paid for the specific service in question, in the event of demonstrable professional negligence directly related to the execution of the agreed-upon scope of work.

---

### Intellectual property and usage license:

1.  All intellectual property rights (including copyright) in the methodologies, analyses, reports, and statistical models created by **Data Lab ICMAN** remain the property of the Entity or its licensors.
2.  Upon full payment, the Client receives a specific, limited license to use the final deliverables (the "**Licensed Material**"), the terms of which are stipulated in a separate **[License Agreement](license.html)** accompanying the materials.
3.  The Client\'s use of the Licensed Material is strictly conditioned upon and limited by the terms of the accompanying License Agreement. **The Client expressly agrees that its right to use the statistical results, data analysis, reports, and any other Intellectual Property furnished by Data Lab ICMAN is subject to, and dependent upon, the Client\'s strict adherence to the terms and conditions of the specified usage license accompanying the final product.** Failure to comply with the limitations, restrictions, or permitted uses set forth in the license shall be considered an unauthorized use and a material breach of the service agreement.

---

**By engaging in our paid services, the client fully accepts the terms and conditions set forth in this Disclaimer.**
'
  replace_in_file("analysis/index.Rmd", "Welcome to my research website.", disclaimer_text)
  cat("    \u2705 Patched 'index.Rmd' with the new DISCLAIMER text.\n")
  # --- FIN DE LA MODIFICACIÓN ---

  replace_in_file("analysis/_site.yml", "PROJECT_NAME", project_name)
  replace_in_file("analysis/_site.yml", "HREF", href)
  cat("    \u2705 Patched '_site.yml' with project name and href.\n")

  replace_in_file("analysis/license.Rmd", "PROJECT_NAME", project_name)
  replace_in_file("analysis/license.Rmd", "ATTRIBUTION", attribution)
  cat("    \u2705 Patched 'license.Rmd' with project name and attribution.\n")

  today_date <- format(Sys.Date(), "%Y-%m-%d")
  replace_in_file("analysis/Methods.Rmd", "CREATOR", creator)
  replace_in_file("analysis/Methods.Rmd", "TODAYSDATE", today_date)
  cat("    \u2705 Patched 'Methods.Rmd' with creator and date.\n")

  replace_in_file("analysis/Results.Rmd", "CREATOR", creator)
  replace_in_file("analysis/Results.Rmd", "TODAYSDATE", today_date)
  cat("    \u2705 Patched 'Results.Rmd' with creator and date.\n")

  # --- 4. Publish Initial Files ---
  cat("\n---\n")
  cat("STEP 4: PUBLISHING INITIAL PROJECT STATE\n")
  cat("---\n")

  files_to_publish <- c(
    "analysis/index.Rmd",
    "analysis/about.Rmd",
    "analysis/license.Rmd",
    "analysis/Methods.Rmd",
    "analysis/Results.Rmd"
  )

  workflowr::wflow_publish(files = files_to_publish, message = "Publish the initial files")

  cat("\n---\n")
  cat(sprintf("\U0001f389 All tasks completed successfully for project '%s'.\n", project_name))
  cat("---\n")

  workflowr::wflow_status()

  return(invisible(getwd()))
}
