# kwbGompitz 0.6.4.9000

* Added a `NEWS.md` file to track changes to the package (see https://style.tidyverse.org/news.html for writing a good `NEWS.md`)
* new: checkInputData()
* createExampleFiles(): create output files only for existing binary versions
* improve error handling
* rename further functions whose names start with dot
* fix bug in get_default_colours_for_labels()
* add argument "digits_exp" to readParameters() and writeParameters()

Add new function reformatScientific().

# kwbGompitz 0.6.1

* composeGompitzInputData: by default, give a message instead of a warning when 
  the weight vector is recycled to the desired length. Bug fix: exclude empty
  condition labels when getting unique values from the condition column. Convert
  condition labels from factor to character.

* rename functions whose names start with dot so that the functions will be 
  loaded into the global environment when kwb.utils::assignPackageObjects()
  is called. 

# kwbGompitz 0.1.7

* .getParameterEstimates: if data frame does not have five columns, NA is 
  returned (suggested by Nicolas Caradot)

# kwbGompitz 0.1.6

* ???

# kwbGompitz 0.1.5

* new arg "verbose" for functions runGompitzCalibration, runGompcal, 
  runGompcalInDirectory, readCalibrationResultFromFile, 
  createTemporyGompitzDirAndReturnPath, runGompitzPrediction, 
  runGompredInDirectory, runGompred

# kwbGompitz 0.1.4

* new functions: composeGompitzInputData, runGompitzCalibration, 
  runGompitzPrediction, noCalibrationAvailableForStratum, survivalFunction
* gompcal is not supported anymore, use runGompitzCalibration instead.
* gompred0 is not supported anymore, use runGompitzPrediction instead.
* .getFileContentForInputFile: new arg "weight"
* getInputFileBody: weight does not have a default value (was 1) any more.
* input data to gompcal is written to observations_cal.txt
* input data to gompred is written to observations_val.txt

# kwbGompitz 0.1.3

* new function: .getFileContentForInputFile
* getObservationByCondition: use unlist to prevent bug

# kwbGompitz 0.1.2

* new functions: orderByMostProbableClassAndDecreasingProbability,
orderByWeightedProbabilities
* plotPredictionByYearToPdf: new arg "FUN.name"
* plotPredictionByYear: new args "FUN.name", "main"

# kwbGompitz 0.1.1

.probabilityOrder: NAs removed before finding maximum probability

# kwbGompitz 0.1.0

first version
