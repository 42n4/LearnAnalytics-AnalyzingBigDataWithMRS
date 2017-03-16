
setwd("C:/Users/sethmott/OneDrive/Documents/courses/LearnAnalytics-AnalyzingBigDataWithMRS/instructor_resources")

rm(list = ls())
rmarkdown::render("01-Getting-started.Rmd", output_dir = "../student_resources")
rmarkdown::render("02-Reading-data.Rmd", output_dir = "../student_resources")
rmarkdown::render("03-Preparing-data.Rmd", output_dir = "../student_resources")
rmarkdown::render("04-Running-sanity-checks.Rmd", output_dir = "../student_resources")
rmarkdown::render("05-Clustering-example.Rmd", output_dir = "../student_resources")
rmarkdown::render("06-Neighborhood-patterns.Rmd", output_dir = "../student_resources")
rmarkdown::render("07-Building-models.Rmd", output_dir = "../student_resources")
rmarkdown::render("08-Scaling-and-deployment.Rmd", output_dir = "../student_resources")
rmarkdown::render("09-SQL-deployment.Rmd", output_dir = "../student_resources")
rmarkdown::render("10-Spark-deployment.Rmd", output_dir = "../student_resources")

file.remove(dir("../student_resources", pattern = "*.html", full.names = TRUE))
dir("../student_resources")

# SQL deployment: 
# find out why payment_type and rate_code_id are <NA>
# find out why trip_duration is missing for plots


library(knitr)
purl("01-Getting-started.Rmd")
purl("02-Reading-data.Rmd")
purl("03-Preparing-data.Rmd")
purl("04-Running-sanity-checks.Rmd")
purl("05-Clustering-example.Rmd")
purl("06-Neighborhood-patterns.Rmd")
purl("07-Building-models.Rmd")
purl("08-Scaling-and-deployment.Rmd")
purl("09-SQL-deployment.Rmd")
purl("10-Spark-deployment.Rmd")
