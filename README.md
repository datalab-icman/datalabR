
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- <br> -->

<!-- <div style="display: flex; align-items: center; gap: 40px;"> -->

<!--   <a href="https://datalab-icman.github.io/datalab-icman"> -->

<!--     <img src="man/figures/datalabr_hex.png" alt="datalabr hex logo" width="150"> -->

<!--   </a> -->

<!--   <h1 style="margin: 0; border: none; text-decoration: none; box-shadow: none;">**datalabr**</h1> -->

<!-- </div> -->

<!-- <br> -->

# datalabR <a href="https://datalab-icman.github.io/datalabR"> <img src="man/figures/datalabr_hex.png" align="right" alt="datalabR hex logo" width="120" /> </a>

<!-- badges: start -->

<!-- badges: end -->

<br>

Provides a suite of useful tools from the Data Lab ICMAN.

## Installation

You can install the development version:

``` r
if (!require("pak")) install.packages("pak")
pak::pak("datalab-icman/datalabR")
```

## Example

``` r
library(datalabR)
```

Show some Unicode symbols:

``` r
show_symbol_legend()
#> 
#> --- Package Console Symbol Legend ---
#> 
#> STATUS MESSAGES:
#>  ✅ Success/Approved: Task completed successfully.
#>  ❌ Error/Failure: Task could not be completed due to an issue.
#>  ⚠️ Warning: Task completed, but with potential issues or anomalies.
#>  ⛔️ Forbidden/Stopped: Action is not valid or has been restricted.
#> 
#> PROGRESS & ACTIONS:
#>  ⚙️ Configuration/Setup: Starting a configuration or pre-processing step.
#>  🚀 Rocket Launch/Quick Start: Initiating a critical process or deployment.
#>  ⏳ In Progress/Waiting: Task is currently running and may take time.
#>  ⌛️ Time Completed: The waiting period or process has finished.
#>  🔍 Search/Investigation: Executing a query or data exploration function.
#>  📁 Directory/File: Creating or accessing a file path/folder.
#>  💾 Save/Write: Data has been successfully written to disk.
#> 
#> INFORMATIONAL:
#>  ℹ️ Information: General informational or debugging message.
#>  💡 Hint/Idea: A useful tip or additional note for the user.
#>  🎉 Celebration: Milestone reached or installation complete!
#> 
#> --------------------------------------
```
