# DHSwealth
Scripts for processing and harmonizing wealth data from the [Demographic and Health Surveys](dhsprogram.com) according to [Rutstein and Staveteig, 2014](https://www.dhsprogram.com/pubs/pdf/MR15/MR15.pdf).

These scripts were made for my PhD dissertation work.  This is not intended to be a polished package, but might prove useful to other R users who are interested in harmonizing DHS wealth indices.  If you have any questions, feel free to [email me](mailto:mw.coop.r@gmail.com).

## Using the scripts
### Scope available surveys
First you need to scope all of the available DHS surveys, they should be all in one directory in Stata or Shapefile format.  Run the script `scope/Scope All Surveys.R`, modifying the directories at the top of the script.

### Reading and Harmonizing Data
Then, run the script `extract/Extract Wealth Vars.R`.  Again you will need to modify the paths.  You will also need to choose a baseline survey that all other survey wealth indices will be harmonized to.  The default is the 2008 Nigeria survey.  This script collects all relevant wealth variables from each PR stata file, and joins geospatial data from the GE shapefiles, as well as the wealth data in the WR file when necessary.  It then harmonizes the wealth indices based on the DHS's methodology.
