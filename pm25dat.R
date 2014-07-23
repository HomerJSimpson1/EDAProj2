pm25dat <- function(strFilePath=getwd(), DOWNLD_UNZIP = TRUE, USE_INET2 = FALSE) {
  ## Exploratory Data Analysis Coursera Class
  ## Project 2
  ## DOWNLD_UNZIP = A Flag to determine if I should download and unzip the data or not. Default value = TRUE.
  ## USE_INET2 = A Flag to determine if I need to use setInternet2(use=TRUE).  Default value = FALSE.
  ## Requires the following packages:
  ##          data.table  (data tables can be way faster, and seem to be very much faster for merging)
  ##          
  ##          

    
  ## Get the data and merge into one data table.
  merged <- readdata(strFilePath, DOWNLD_UNZIP, USE_INET2)

  ## Group the data by year
#  checkpkg("plyr")
  ## Use the ddply function from the plyr package to apply the mean function to each of the columns
  ## (except for the subjectId and activityLabel columns)
#  yearsums <- ddply(merged, "year", function(x) sapply(merged$Emissions, sum))
  yearsums <- by(merged$Emissions, merged$year, sum)
  
  
  ## Select years 1999, 2002, 2005, and 2008.  Actually, no need to select by year, as these are the only years
  ## included in the data.

  

  ## Plot the data using the base graphics system.
#  plot(names(sums), logb(sums, 1e6), type="l", xlab="Year", ylab=expression('log'[1e6]' of PM'[2.5]),
#       main=expression('Total PM'[2.5]' From All Sources'))    
#  plot(names(sums), logb(sums, 1e6), type="l", xlab="Year", ylab=expression('log of PM'[2.5]), 
#       main=expression('Total PM From All Sources'))

  png("plot1.png")  
  ## Scatterplot with points
##   ## log10 version
##   plot(names(sums), log10(sums), xlab="Year", ylab=expression('log'[10]*' of PM'[2.5]),
##        main=expression('Total PM'[2.5]*' From All Sources'))

  ## Regular value version
  plot(names(sums), sums, xlab="Year", ylab=expression('PM'[2.5]),
       main=expression('Total PM'[2.5]*' From All Sources'))
  lines(names(sums), sums)

  
##   ## Line plot
##   plot(names(sums), log10(sums), type="l", xlab="Year", ylab=expression('log'[10]*' of PM'[2.5]),
##        main=expression('Total PM'[2.5]*' From All Sources'))

##   ## Line plot
##   plot(names(sums), sums, type="l", xlab="Year", ylab=expression('PM'[2.5]),
##        main=expression('Total PM'[2.5]*' From All Sources'))

  
  ## The regression line fails to appear on the plot.  It does give a warning that only 2 of 4 points are use.
  #abline(lm(names(sums)~sums))
  dev.off()

  
  #plot(names(sums), logb(sums, 1e6), type="l", xlab="Year", ylab=expression('log'[1e6]*' of PM'[2.5]),
  #     main=expression('Total PM'[2.5]*' From All Sources'))    
  #plot(names(yearsums), yearsums, type="l")
  ## Regression line.  This yields an error - only uses 2 of 4 points?   Just skip it.
  #fit <- lm(yearsums ~ names(yearsums))
  #abline(fit)
  
  ## Return the merged data frames?
  return(yearsums)
}




readdata <- function(strFilePath, DOWNLD_UNZIP, USE_INET2) {
  ## Set the values of some directories and filenames that will be used throughout.

  ## Check to see that the data.table package has been installed and loaded.  If not, then install and load it.
  checkpkg("data.table")

  ## Some basic directory variables
  destdir <- strFilePath
  dstfile <- paste(destdir, "/exdata_data_NEI_data.zip", sep="")  
  unzipdst <- paste(destdir, "/unzipped", sep="")
  
  if (DOWNLD_UNZIP)
    {
      ## Used to determine if I changed the setInternet2 value in the call to the useinet2() function.  i.e. it may
      ## already have been set to setInternet2(use=TRUE), prior to calling the useinet2() function.
      changed = FALSE
      if (USE_INET2)
        { 
          changed = useinet2(changed)
        }
  
      ## Download data from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
      fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
      datadownld(fileURL, destdir, dstfile)
      
      ## Unzip the file(s)
      unzipthedata(destdir, dstfile)

      ## Not sure if I need to do the following, or if it even raises an error.
      if (USE_INET2 && changed)
        {
          setInternet2(use=FALSE)
        }
    }

  ## Examine the files
  ## Top level directory for the unzipped files is now unzipdst
  filelst <- list.files(unzipdst)
  filelst
  print(filelst[[1]])
  print(filelst[[2]])

  ## Read the files into R (there are 2 files).  The files are stored as .rds files, which are read into R
  ## using the readRDS function (and written using the saveRDS function).
  NEI <- as.data.table(readRDS(paste(unzipdst,"/",filelst[[2]], sep="")))
  SCC <- as.data.table(readRDS(paste(unzipdst,"/",filelst[[1]], sep="")))

  ##print(str(NEI))
  ##print(str(SCC))  

  ## Use merge() to join the two data frames.
  merged <- merge(NEI, SCC, by="SCC")

  return(merged)  
}


datadownld <- function(fileURL, destdir, dstfile) {
  ## Download the data
  ## First, set the URL value.
  ## e.g. fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

  print(destdir)
  print(dstfile)
  print(fileURL)
  
  ## If the directory where we're going to put the data doesn't exist, create it.
  if (!file.exists(destdir))
    {
      dir.create(destdir)
    }

  ## Call download.file().  If the URL is secure (https), then on some systems you need to specify method="curl".
  ## Don't use method="curl" on Windows when setInternet2 is TRUE.
  if(substr(fileURL, 1, 5) == "https")
    download.file(fileURL, destfile = dstfile, method="curl")
  else if(substr(fileURL, 1, 5) == "http:")
    download.file(fileURL, destfile = dstfile)

  dateDownloaded <- date()  
}


useinet2 <- function(changed) {
  ## Only need this at work, not at home.
  ## setInternet2(use=TRUE) causes R to load the internet2.dll file
  ## Use setInternet2(NA) to return the current setting.  If FALSE, change to true.
  if (!setInternet2(NA))
    {
      print("Changing setInternet2 setting")
      setInternet2(use=TRUE)
      changed = TRUE
    }
  return(changed)
}


unzipthedata <- function(datadir, datafile) {
  ## The directory to which I will unzip
  unzipdst <- paste(datadir, "/unzipped", sep="")

  ## If this directory does not already exist, create it.
  if (!file.exists(unzipdst))
    {
      dir.create(unzipdst)
    }
  
  ## Unzip the file
  unzip(datafile, exdir=unzipdst)  
}



checkpkg <- function(packageName){
  ## Function to check to see if a required package is installed and loaded.
  if (!require(packageName, character.only=TRUE))
    {
      install.packages(packageName, repos=url("http://cran.us.r-project.org"), dependencies=TRUE)
      if (!require(packageName, character.only=TRUE)) { stop("Package not found.") }
    }
}

