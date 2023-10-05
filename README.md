# IDI Tools as R packages
Rework R tools for IDI analysis into packages

## Overview
Integrated data is a powerful tool for research. But this power comes with complexity: there is a steep learning curve for new analysts working with integrated data. During IDI research at the Social Wellbeing Agency, I developed a set of R scripts with a range of functions to simplify, standardise, and accelerate analysis.

These tools were made available to the research community via GitHub ([here](https://github.com/nz-social-wellbeing-agency) ) and within the secure data lab environment. However, the script-based format was unideal. Given that the tools are written in R, they would be better arranged as R packages.

This repository reworks the original scripts into R packages. As part of doing so, it generalises away from the SQL Server-specific configuration that is found in the secure data lab environment. This should make these tools relevant beyond the IDI, while only introducing minimal changes for existing users.

## Origin and license

Consistent with the original source code, this code is licensed under GPL-3.0. You can find the original source code [here](https://github.com/nz-social-wellbeing-agency/dataset_assembly_tool) . This repository branches from commit `4c0f87f`, dated February 2023.

## The packages

The original tooling has been divided into four component packages:

### dbplyr.helpers

Working between R and SQL, via dbplyr, lets you use each in the contexts where it is strongest: SQL for larger datasets, R for statistical analysis. These helpers allow us to complete certain SQL tasks from within R.

### check.dataset

Exploration or review of datasets occurs at all stages of an analysis. Regardless of whether a dataset is remote in SQL or local in R, it is helpful to have consistent ways to assess and confirm its correctness.

### assembly.tool

Analytic and research projects often begin with a period of data wrangling. As the number of data sources increases, so does the complexity of preparation. We developed the assembly tool to provide a consistent and fast method for assembling datasets for our analytic projects.

### conf.output

Within the IDI, all output must be summarised and confidentialised prior to its release. For this purpose we developed methods to produce our most common types of summaries and to confidentialise them. To give increased confidence there are also separate functions to check the correctness of the applied confidentiality.

## Installation

The repository contains pre-compiled versions of all four packages ready for installation. To install these packages we recommend the following steps:

1. Check if the package is already installed using `'package_name' %in% installed.packages()`.
2. Download all four package files from this repository. These are the files with the `.tar.gz` extension.
3. Install each package from file using `install.packages(path/to/package/package_name.tar.gz, repos=NULL, type="source")`.

Due to dependencies between these four packages, you will need to install them in the order: dbplyr.helpers > check.dataset > assembly.tool > conf.output.

## Use

Converting from the original scripts to packages has resulting in changes to how the tools are loaded, connect to the database, and tested on SQL server. Key differences are noted below.

### Load

When these tools were available as scripts, they would be loaded into the R workspace using the `source` command. Now that these tools are installed as packages, they are instead access via the `library` command, same as any other package (e.g. `library(dbplyr.helpers)`).

You can also access functionality from these tools without the library command using `::` notation (e.g. `dbplyr.helpers::___`) followed by the name of the function you want to use. Within RStudio, typing `package.name::` in the console should bring up a list of all the functions available in that package.

As a package, all the functions have help documentation. You can access the documentation using the standard `?` notation (e.g. `?run_time_inform_user` or `?dbplyr.helpers::run_time_inform_user`).

### Connect to database

The original scripts included a function `create_database_connection` to connect to the SQL database. However, databases differ and connections change over time. This means it is a better approach for users to create the database connection outside these tools according to the instruction of their database administrator.

We instead provide some guidance on what a database connection may look like. This guidance can be accessed from within R via the functions `dbplyr.helpers::display_connection_guidance()` and `assembly.tool::provide_assembly_tool_example`.

In general there are two ways to make a connection: via a connection string or by argument. The components of each approach are the same:

- Using a connection string
  ```
  con_str = "DRIVER=ODBC Driver 17 for SQL Server; Trusted_Connection=Yes; SERVER={server},{port}; DATABASE={database};"
  db_conn = DBI::dbConnect(odbc::odbc(), .connection_string = con_str)
  ```
  
- By argument
  ```
  db_conn = DBI::dbConnect(
    odbc::odbc(),
    driver = "ODBC Driver 18 for SQL Server",
    Trusted_Connection = "Yes",
	TrustServerCertificate = "Yes",
    server = "server",
    database = "database"
  )
  ```

### SQL Server testing

The IDI environment stores data in an SQL Server and the original version of this code interacted directly with the SQL Server. However SQL Server is not part of R. This has required a couple of changes:

1. The tools now work with SQLite, as this flavour of SQL is available within R.
2. The tests that depend on SQL Server are no longer run automatically during development. These can instead be accessed via the `test_with_sql_server` functions. Users are encouraged to run these if they need to confirm that packages are performing as expected in their SQL Server environment.

These changes mainly effect the dbplyr.helpers and assembly.tool packages. Given syntax differences between the different flavours of SQL, there is no guarantee these tools will work as expected if connected to a database that is neither SQLite nor SQL Server.

## Reference material

The original reference material for the Dataset Assembly Tool remains relevant for the rearrangement into packages. These references cover the design principles, architecture, and workings of the tools, as well as a worked examples of their use. The previous description of installation should be replaced with the instructions above.

A copy of the reference documents are included in this repository. They can also be accessed online:
* [Assembly tool primer and guide](https://swa.govt.nz/assets/Publications/guidance/Introduction-to-the-Dataset-Assembly-tool-primer-and-guide.pdf)
* [Assembly tool intro and training](https://swa.govt.nz/assets/Publications/guidance/Dataset-Assembly-Tool-introduction-and-training-presentation.pdf)
* [Assembly tool training presentation](https://vimeo.com/490565559)
* [Assembly tool demonstration](https://vimeo.com/561152732/435a570079)
* [Summarise and confidentialise training](https://swa.govt.nz/assets/Publications/guidance/summarise-and-confidentialise-tools-training-guide-v2.pdf)
* [Self-checking tools training](https://swa.govt.nz/assets/Publications/guidance/self-checking-tools-training-guide.pdf)
* [IDI exemplar project](https://swa.govt.nz/assets/Publications/guidance/IDI-Exemplar-project-guide.pdf)
