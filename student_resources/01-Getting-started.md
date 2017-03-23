Getting started
================
Seth Mottaghinejad
2017-03-17

**R** is a very popular programming language whose rich set of features and packages make it ideally suited for data analysis and modeling. Traditionally, R works by loading (copying) every object as a memory object, including tabular data which in R is called a `data.frame`. This means that large data can quickly surpass the amount of available space in the memory. This is especially a problem when multiple users are working on the same R server, where free memory can quickly turn into a scarce resource. Over time, many R packages have been introduced that attempt to overcome this limitation. Some propose a way to more efficiently load and process the data, which would in turn allow us to work with larger data sizes. This approach however can only take us so far, since efficiency eventually hits a wall, and also sometimes assumes a more sophisticated knowledge about programming that not all R users have.

Microsoft R Server (MSR) on the other hand takes a different approach to handling big data. MRS's `RevoScaleR` package stores the dataset on disk (hard drive) and loads it only a **chunk** at a time (where each chunk is a certain number of rows) for processing (reading or writing). Once a chunk is processed, it then moves to the next chunk of the data. By default, the **chunk size** is set to 500K rows, but we can change it to a lower number when dealing with *wider* datasets (hundreds or thousands of columns), and a larger number when dealing with *longer* data sets (only a handful of columns).

So data in `RevoScaleR` is *external* (because it's stored on disk) and *distributed* (because we process it chunk-wise). This means we are no longer bound by memory when dealing with data: Our data can be as large as we have space on the hard-disk to store it. As we will see, we `RevoScaleR` can work with flat files directly, and it can also convert them to a more efficient and native format called XDF. Since at every point in time we only load one chunk of the data as a memory object (an R `list` object to be specific), we never overexert the system's memory. As we will see, all this chunk-wise processing of data is happening behind the scenes and abstracted away from the end-user.

| environment | data sources         | most R packages | `RevoScaleR` |
|-------------|----------------------|-----------------|--------------|
| local       | `data.frame`         | ✓               | ✓            |
| local       | flat files (CSV)     |                 | ✓            |
| local       | XDF                  |                 | ✓            |
| remote      | SQL Server table     |                 | ✓            |
| remote      | HDFS (Hive, parquet) |                 | ✓            |
| remote      | HDFS (XDFD)          |                 | ✓            |

Moreover, `RevoScaleR` data-processing and analytics functions run both locally (on a single R server) and remotely, which makes our R code almost effortlessly portable from a local development environment to a remote production environment. It is important to emphasize that we are not just talking about the data being remote, but the computation itself happening remotely. In both SQL Server and Spark, `RevoScaleR` functions can not only access the data stored in those environments, but also execute remotely (on the SQL Server or Spark cluster) and preserve the priciple of [data locality](https://www.microsoft.com/en-us/research/publication/maximizing-data-locality-in-distributed-systems/).

On the other hand, most open-source R algorithms for data processing and analysis (including most third-party packages) rely on the whole dataset to be loaded into the R session as a `data.frame` object, which means they do not work *directly* with distributed data. But as we will see,

-   Most data-processing steps (cleaning data, creating new columns or modifying existing ones) can still *indirectly* (and relatively easily) be used by `RevoScaleR` to process data, so that we can still leverage any R code we developed. What we mean by *indirectly* will become clear as we cover a wide range of examples.
-   `RevoScaleR` comes with its own set of distributed analytics algorithms that work on a distributed data set in addition to a `data.frame`. For example, the `RevoScaleR::rxLinMod` function replicates what `stats::lm` does, but because `rxLinMod` is a distributed algorithm it runs both on a `data.frame` (where it far outperforms `lm` if the `data.frame` in question is large), and on a distributed dataset such as a large CSV file on disk, a SQL Server table, or data on HDFS.

Summary
-------

Let's review what the `RevoScaleR` package offers:

1.  When our data is large, but still small enough to fit in the memory as a `data.frame`, we can still use `RevoScaleR`'s parallel algorithms to run models on the data much faster than their open-source counterparts (such as using `rxLinMod` instead of `lm`).
2.  When the data is too large to fit in available memory, we can convert the data to an external and distributed format, i.e. data that is saved on disk and processed chunk-wise. `RevoScaleR`'s data-processing and analytics functions work directly with such data in addition to a `data.frame`.
3.  When the data is saved in a distributed environment such as HDFS or SQL Server, which is often the case in production, with some minor adjustments we can deploy our code in such environments, reducing the hurdle of going from development to production.

Loading Packages
----------------

At various points throughout the analysis, we will be using to a set a third-party packages. So let's begin by loading those packages. The `RevoScaleR` package is pre-installed with Microsoft R Server (MRS), since it cannot be downloaded from CRAN, but all the other packages shown below are third-party packages that can be downloaded and installed from CRAN using the `install.packages` command.

Additionally, we override some default options to make it easier to display data or results.

``` r
library(RevoScaleR)
library(tidyverse)
library(lubridate)
library(stringr)
options(dplyr.print_max = 2000)
options(dplyr.width = Inf) # shows all columns of a tbl_df object
library(rgeos) # spatial package
library(maptools) # spatial package
library(ggmap)
library(gridExtra) # for putting plots side by side
library(ggrepel) # avoid text overlap in plots
library(seriation) # package for reordering a distance matrix
```
