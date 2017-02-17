Reading the data
================
Seth Mottaghinejad
2017-02-17

An analysis usually begin with a question we're trying to answer, after which we gather any data that can help us answer it. There are also times when we start with data we've collected and instead of trying to answer a specific question, we explore the data in search of not-so-obvious trends. This is sometimes referred to as **exploratory data analysis** and it can be a great way to help determine what sorts of questions the data can answer.

Learning objectives
-------------------

After reading this section we will understand - how `RevoScaleR` functions can work with data in the memory (`data.frame`) and with data on disk - data on disk can consist of flat files (such as CSV files), MRS's proprietary XDF format, and it can be stored locally or in a distributed file system such as HDFS - XDF files can be created from the original flat files using `rxImport` - the choice of converting from flat files to XDF depends on certain trade-offs

The NYC Taxi data
-----------------

To see how we can use MRS to process and analyze a large dataset, we use the [NYC Taxi dataset](http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml). The raw datasets span over multiple years and consists of a set of 12 CSV files for each month of the year. Each record (row) in the file shows a Taxi trip in New York City, with the following important attributes (columns) recorded: - the date and time the passenger(s) was picked up and dropped off, - the number of passengers per trip, - the distance covered, - the latitude and longitude at which passengers were picked up and dropped off, - payment information such as the type of payment and the cost of the trip broken up by the fare amount, the amount passengers tipped, and any other surcharges.

Each raw CSV file is about 2 Gbs in size, so 6 months worth of it amounts to 12 Gbs. That's usually more than available memory on a single personal computer. A server can have much larger memory capacity, but if a server is used by many users at once, R can very quickly run out of memory.

Loading the top 1000 rows
-------------------------

We begin by loading the first 1000 rows of the data using the `read.table` function. To avoid unnecessary factor conversions, we examined the data and decided on the proper column types ahead of time, storing them in an object called `col_classes` which we then pass to `read.table`.

``` r
col_classes <- c(
  'pickup_datetime'       = "character",
  'dropoff_datetime'      = "character",
  'passenger_count'       = "integer",
  'trip_distance'         = "numeric",
  'pickup_longitude'      = "numeric",
  'pickup_latitude'       = "numeric",
  'rate_code_id'          = "factor",
  'dropoff_longitude'     = "numeric",
  'dropoff_latitude'      = "numeric",
  'payment_type'          = "factor",
  'fare_amount'           = "numeric",
  'extra'                 = "numeric",
  'mta_tax'               = "numeric",
  'tip_amount'            = "numeric",
  'tolls_amount'          = "numeric",
  'improvement_surcharge' = "numeric",
  'total_amount'          = "numeric")
```

**It is a good practice to load a small sample of the data as a `data.frame` in R.** When we want to apply a function to the XDF data, we can first apply it to the `data.frame` where it's easier and faster to catch errors, before applying it to the whole data. We will later learn a method for taking a random sample from the data, but for now the sample simply consists of the first 1000 rows.

``` r
input_csv <- file.path(data_dir, 'yellow_tripsample_2016-01.csv')
# we take a chunk of the data and load it as a data.frame (good for testing things)
nyc_sample <- read.csv(input_csv, nrows = 1000, colClasses = col_classes)
head(nyc_sample)
```

    ##       pickup_datetime    dropoff_datetime passenger_count trip_distance
    ## 1 2016-01-16 19:30:38 2016-01-16 19:44:42               1          2.20
    ## 2 2016-01-16 22:21:42 2016-01-16 22:35:30               2          6.36
    ## 3 2016-01-31 01:25:13 2016-01-31 01:33:31               2          2.97
    ## 4 2016-01-09 10:27:41 2016-01-09 10:30:55               4          0.60
    ## 5 2016-01-01 03:48:48 2016-01-01 03:58:20               3          2.60
    ## 6 2016-01-22 21:11:53 2016-01-22 21:37:07               1          3.11
    ##   pickup_longitude pickup_latitude rate_code_id dropoff_longitude
    ## 1              -74            40.8            1               -74
    ## 2              -74            40.7            1               -74
    ## 3              -74            40.7            1               -74
    ## 4              -74            40.8            1               -74
    ## 5              -74            40.7            1               -74
    ## 6              -74            40.8            1               -74
    ##   dropoff_latitude payment_type fare_amount extra mta_tax tip_amount
    ## 1             40.8            1        11.5   0.0     0.5       3.00
    ## 2             40.7            1        19.5   0.5     0.5       5.20
    ## 3             40.8            1        10.0   0.5     0.5       1.00
    ## 4             40.8            1         4.5   0.0     0.5       1.05
    ## 5             40.8            2        10.0   0.5     0.5       0.00
    ## 6             40.7            2        16.5   0.5     0.5       0.00
    ##   tolls_amount improvement_surcharge total_amount
    ## 1            0                   0.3        15.30
    ## 2            0                   0.3        26.00
    ## 3            0                   0.3        12.30
    ## 4            0                   0.3         6.35
    ## 5            0                   0.3        11.30
    ## 6            0                   0.3        17.80

Reading the whole data
----------------------

We now read the whole data using MRS. MRS has two ways of dealing with flat files:

1.  it can work directly with the flat files, meaning that it reads and writes to flat files directly,
2.  it can covert flat files to a format called XDF (XDF stands for **external `data.frame`**).

We choose to go with the second option. We explain our reasoning in the next section. To convert flat files to XDF, we use the `rxImport` function. By letting `append = "rows"`, we can also combine multiple flat files into a single XDF file.

``` r
input_xdf <- file.path(data_dir, 'yellow_tripsample_2016.xdf')
library(lubridate)
most_recent_date <- ymd("2016-07-01") # the day of the months is irrelevant

# because we keep appending to the same file, we can't run this in parallel
st <- Sys.time()
for(ii in 1:6) { # get each month's data and append it to the first month's data
  file_date <- most_recent_date - months(ii)
  input_csv <- sprintf('yellow_tripsample_%s.csv', substr(file_date, 1, 7))
  input_csv <- file.path(data_dir, input_csv)
  append <- if (ii == 1) "none" else "rows"
  rxImport(input_csv, input_xdf, colClasses = col_classes, overwrite = TRUE, 
    append = append)
  print(input_csv)
}
```

    ## 
    Rows Processed: 500000
    Rows Processed: 1000000 
    ## [1] "C:/Data/NYC_taxi/yellow_tripsample_2016-06.csv"
    ## 
    Rows Processed: 500000
    Rows Processed: 1000000 
    ## [1] "C:/Data/NYC_taxi/yellow_tripsample_2016-05.csv"
    ## 
    Rows Processed: 500000
    Rows Processed: 1000000 
    ## [1] "C:/Data/NYC_taxi/yellow_tripsample_2016-04.csv"
    ## 
    Rows Processed: 500000
    Rows Processed: 1000000 
    ## [1] "C:/Data/NYC_taxi/yellow_tripsample_2016-03.csv"
    ## 
    Rows Processed: 500000
    Rows Processed: 1000000 
    ## [1] "C:/Data/NYC_taxi/yellow_tripsample_2016-02.csv"
    ## 
    Rows Processed: 500000
    Rows Processed: 1000000 
    ## [1] "C:/Data/NYC_taxi/yellow_tripsample_2016-01.csv"

``` r
Sys.time() - st # stores the time it took to import
```

    ## Time difference of 1.38 mins

XDF vs CSV
----------

An XDF file is much smaller than a CSV file because it is compressed. Its main advantage over a CSV file is that an XDF file can be read and processed much faster than a CSV file (we will run a simple benchmark to see how much faster). The disadvantage of an XDF file format is a format that only MRS understands and can work with. So in order to decide whether we chose XDF or CSV we need to understand the I/O trade-offs involved:

1.  Converting from CSV to XDF is itself a cost in terms of runtime.
2.  Once the original CSVs are converted to XDFs, the runtime of processing (reading from and sometimes writing to) the XDFs is lower than what the it would have been if we had directly processed the CSVs instead.

Since an EDA workflow usually consists of cleaning and munging data, and then feeding that to various modeling and data-mining algorithms, the initial runtime of converting from CSV to XDF is quickly offset by the reduced runtime of subsequently working with the XDF file. However, one-off kinds of analyses on datasets that are ready to be fed to the modeling algorithm might run faster if we skip XDF conversion. One-off operations are also common in production code, such as when a dataset is scored with an already existing model everytime new data comes in. In such cases, we need to run some benchmarks in order to find the optimal solution.

In the last section, we used `rxImport` to covert 6 months worth of CSV files into a single XDF file. We can now create an R object called `nyc_xdf` that points to this XDF data. We do so by providing the path to the XDF file to the `RxXdfData` function. Let's look at a summary of this dataset by running the `rxSummary` function against `nyc_xdf`. The `rxSummary` function uses the popular **formula notation** used by many R functions. In this case, the formula `~ fare_amount` means we want to see a summary for the `fare_amount` column only. Just like the `summary` function in `base` R, `rxSummary` will show us a different output depending on the type of each column. Since `fare_amount` is a numeric column, we get numeric summary statistics.

``` r
input_xdf <- file.path(data_dir, 'yellow_tripsample_2016.xdf')
nyc_xdf <- RxXdfData(input_xdf)
system.time(rxsum_xdf <- rxSummary( ~ fare_amount, nyc_xdf))
```

    ## 
    Rows Processed: 500000
    Rows Processed: 1000000
    Rows Processed: 1500000
    Rows Processed: 2000000
    Rows Processed: 2500000
    Rows Processed: 3000000
    Rows Processed: 3500000
    Rows Processed: 4000000
    Rows Processed: 4500000
    Rows Processed: 5000000
    Rows Processed: 5500000
    Rows Processed: 6000000

    ##    user  system elapsed 
    ##    0.02    0.00    0.16

``` r
rxsum_xdf
```

    ## Call:
    ## rxSummary(formula = ~fare_amount, data = nyc_xdf)
    ## 
    ## Summary Statistics Results for: ~fare_amount
    ## Data: nyc_xdf (RxXdfData Data Source)
    ## File name: C:/Data/NYC_taxi/yellow_tripsample_2016.xdf
    ## Number of valid observations: 6000000 
    ##  
    ##  Name        Mean StdDev Min  Max    ValidObs MissingObs
    ##  fare_amount 13   257    -450 628545 6000000  0

Note that we could have done the same analysis with the original CSV file and skipped XDF coversion. Since we have a separate CSV file for each month, unless we combine the CSV files, we can only get the summary for one month's data. For our purposes that will be enough. To run `rxSummary` on the CSV file, we simply create a pointer to the CSV file using `RxTextData` (instead of `RxXdfData` as was the case with the XDF file) and pass the column types directly to it using the `colClasses` argument. The rest is the same. Notice how running the summary on the CSV file takes considerably longer (even though the CSV file comprises only one month's data).

``` r
# we can only use one month's data unless we join the CSVs
input_csv <- file.path(data_dir, 'yellow_tripsample_2016-01.csv')
# point to CSV file and provide column info
nyc_csv <- RxTextData(input_csv, colClasses = col_classes)
system.time(rxsum_csv <- rxSummary( ~ fare_amount, nyc_csv))
```

    ## 
    Rows Processed: 500000
    Rows Processed: 1000000

    ##    user  system elapsed 
    ##    0.00    0.00    3.94

``` r
rxsum_csv
```

    ## Call:
    ## rxSummary(formula = ~fare_amount, data = nyc_csv)
    ## 
    ## Summary Statistics Results for: ~fare_amount
    ## Data: nyc_csv (RxTextData Data Source)
    ## File name: C:/Data/NYC_taxi/yellow_tripsample_2016-01.csv
    ## Number of valid observations: 1000000 
    ##  
    ##  Name        Mean StdDev Min  Max ValidObs MissingObs
    ##  fare_amount 12.5 11     -434 998 1000000  0

The last example was run to demonstrate `RevoScaleR`'s capability to work directly with flat files (even though they take longer than XDF files), but since our analysis involves lots of data processing and running various analytics functions, from now on we work with the XDF file, so we can benefit from faster runtime.

### Exercises

We learned how to use the `rxSummary` function to summarize the data. If we pass the formula `~ .` to `rxSummary`, we get a summary of all the column in the data. This summary consists of counts for `factor` columns and numeric summaries for `numeric` and `integer` columns (`character` columns are ignored).

Using one month of the NYC taxi data (say January 2016), perform the following analysis: (1) Convert the CSV file for that month to XDF, then run `rxSummary` to get a summary of all its columns. Store the summary in an object called `sum_xdf` for later use. Use `system.time` to see how long it takes to do both the conversion and summary together. (2) Run `rxSummary` directly on the CSV file for that month, storing the result in an object called `sum_csv` for later use. Use `system.time` to time how long it takes to summarize the CSV file.

1.  Compare the runtime in part (1) to part (2). What is your conclusion?
2.  Pick one or two columns (one `factor` and one `numeric`) in the data and drill down into `sum_xdf` and `sum_csv` to make sure that the summaries do in fact match.

Here's some code to get started. Lines where user input is required starts with `##`. Insert your solution into those lines.

``` r
input_xdf <- file.path(data_dir, 'yellow_tripsample_2016-01.xdf')
input_csv <- file.path(data_dir, 'yellow_tripsample_2016-01.csv')

st <- Sys.time()
## convert CSV to XDF here
jan_2016_xdf <- RxXdfData(input_xdf)
## summarize XDF file here
rt_xdf <- Sys.time() - st

st <- Sys.time()
jan_2016_csv <- RxTextData(input_csv, colClasses = col_classes)
## summarize CSV file here
rt_csv <- Sys.time() - st

file.remove(input_xdf) # remove the file to keep folder clean

## compare runtimes rt_xdf and rt_csv
## compare results sum_xdf and sum_csv
```

### Solutions

The purpose of this exercise to compare runtimes for running a single call of `rxSummary` on an XDF file versus a CSV file, for the same data. **If the XDF file already exists, `rxSummary` will always be faster on the XDF file than the CSV file.** But for the comparison to be fair, we assume the XDF file does not exist and needs to be created, and we include the time it takes to covert the CSV file into XDF as part of the runtime to run the summary on the XDF file.

1.  Both the `rxImport` and `rxSummary` call are part of the runtime calculation.

``` r
input_xdf <- file.path(data_dir, 'yellow_tripsample_2016-01.xdf')
input_csv <- file.path(data_dir, 'yellow_tripsample_2016-01.csv')

st <- Sys.time()
rxImport(input_csv, input_xdf, colClasses = col_classes, overwrite = TRUE)
```

    ## 
    Rows Processed: 500000
    Rows Processed: 1000000

``` r
jan_2016_xdf <- RxXdfData(input_xdf)
sum_xdf <- rxSummary( ~ ., jan_2016_xdf)
```

    ## 
    Rows Processed: 500000
    Rows Processed: 1000000

``` r
rt_xdf <- Sys.time() - st # runtime for XDF file

file.remove(input_xdf) # remove the file to keep folder clean
```

    ## [1] TRUE

1.  We point `rxSummary` directly to the CSV file this time.

``` r
st <- Sys.time()
jan_2016_csv <- RxTextData(input_csv, colClasses = col_classes)
sum_csv <- rxSummary( ~ ., jan_2016_csv)
```

    ## 
    Rows Processed: 500000
    Rows Processed: 1000000

``` r
rt_csv <- Sys.time() - st # runtime for CSV file
```

1.  We can just take the difference of the runtimes.

``` r
rt_xdf - rt_csv
```

    ## Time difference of -7.9 secs

We can see that the XDF conversion and subsequent summary was still faster than summarizing the CSV file. This is because summarizing the XDF file considerably faster, making up for conversion time. Since our results are I/O dependent, they will depend on our hard drive's infrastructure.

1.  The `sum_xdf` and `sum_csv` are `list` objects and the counts for the `factor` columns are stored in an element called `categorical`. Here's how we can compare the counts for one `factor` column:

``` r
sum_xdf$categorical[[2]]
```

    ##   payment_type Counts
    ## 1            1 658319
    ## 2            2 336987
    ## 3            3   3486
    ## 4            4   1208

``` r
sum_csv$categorical[[2]]
```

    ##   payment_type Counts
    ## 1            1 658319
    ## 2            2 336987
    ## 3            3   3486
    ## 4            4   1208

The statistical summaries for the `numeric` columns are stored in an element called `sDataFrame`.

``` r
sum_xdf$sDataFrame[5, ]
```

    ##               Name  Mean StdDev  Min Max ValidObs MissingObs
    ## 5 pickup_longitude -72.8   9.21 -122   0  1000000          0

``` r
sum_csv$sDataFrame[5, ]
```

    ##               Name  Mean StdDev  Min Max ValidObs MissingObs
    ## 5 pickup_longitude -72.8   9.21 -122   0  1000000          0

In either case results are identical.
