
SELECT model FROM RDB.dbo.models;

-- We can run this to show that we can successfully retrieve the model
DECLARE @lmodel2 varbinary(max) = (SELECT TOP 1 model FROM RDB.dbo.models);
EXEC sp_execute_external_script @language = N'R',
@script = N'
mod <- unserialize(as.raw(model))
print(summary(mod))',    
@params = N'@model varbinary(max)',
@model = @lmodel2;
GO


-- Create prediction stored procedure

CREATE PROCEDURE [dbo].[PredictTipBatchMode] @inquery nvarchar(max)
AS
BEGIN
DECLARE @lmodel2 varbinary(max) = (SELECT TOP 1 model FROM models);
EXEC sp_execute_external_script @language = N'R',
  @script = N'
              weekday_labels <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
              hour_labels <- c("1AM-5AM", "5AM-9AM", "9AM-12PM", "12PM-4PM", "4PM-6PM", "6PM-10PM", "10PM-1AM")

              library(maptools)
              nyc_shapefile <- readShapePoly("C:/Data/NYC_taxi/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp")
              library(stringr)
              mht_shapefile <- subset(nyc_shapefile, str_detect(CITY, "New York City-Manhattan"))
              manhattan_nhoods <- as.character(mht_shapefile@data$NAME)

              mod <- unserialize(as.raw(model))

              InputDataSet <- transform(InputDataSet, 
                  pickup_dow = factor(pickup_dow, levels = weekday_labels),
                  pickup_hour = factor(pickup_hour, levels = hour_labels),
                  pickup_nb = factor(pickup_nb, levels = manhattan_nhoods),
                  dropoff_nb = factor(dropoff_nb, levels = manhattan_nhoods))

              OutputDataSet <- rxPredict(modelObject = mod, data = InputDataSet, 
                                         outData = NULL, predVarNames = "Score", 
                                         type = "response", writeModelVars = FALSE, 
                                         overwrite = TRUE)
              str(OutputDataSet)
              print(OutputDataSet)',
  @input_data_1 = @inquery,
  @params = N'@model varbinary(max)',
  @model = @lmodel2
WITH RESULT SETS ((Score float));
END


DECLARE @query_string nvarchar(max)
SET @query_string='SELECT top 100 * FROM NYCTaxiSmall'
-- Call stored procedure for scoring
EXEC [dbo].[PredictTipBatchMode] @inquery = @query_string;




-- Create an R plot and serve it in SSRS

INSERT INTO plots(plot)
EXEC  sp_execute_external_script
   @language = N'R'
  ,@script = N'
        image_file = tempfile()
        jpeg(filename = image_file, width = 500, height = 500)
        hist(data$fare_amount, col = "light blue")
        dev.off()
        outds <- data.frame(data = readBin(file(image_file, "rb"), what = raw(), n = 1e6))'
  ,@input_data_1 = N'select fare_amount from rdb.dbo.NYCTaxiSmall tablesample (1 percent);'
  ,@input_data_1_name = N'data'
  ,@output_data_1_name = N'outds';
--WITH RESULT SETS ((plot varbinary(max)));


select plot from
 (select row_number() over (order by Rand() desc) as rnd, * from RDB.dbo.plots) tt 
 where tt.rnd = 1;

-- We can now go to SSRS to serve this plot
-- Just open up Report Builder, create a new data source pointing to the above data, create a new dataset with the above query, insert a new image in the canvas that points to this data and renders into JPEG.
