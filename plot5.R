plot5 <- function(strFilePath=getwd(), DOWNLD_UNZIP = TRUE, USE_INET2 = FALSE) {
  ## Exploratory Data Analysis Coursera Class
  ## Project 2
  ## Filename: plot5.R
  ##
  ## DOWNLD_UNZIP = A flag to determine if I should download and unzip the data or not
  ##               (useful especially while testing by setting = FALSE). Default value = TRUE.
  ## USE_INET2 = A flag to determine if I need to use setInternet2(use=TRUE).  Default value = FALSE.
  ##  
  ## The following code requires the following package(s):
  ##          data.table  (data tables can be way faster, and seem to be very much faster for merging)
  ##          ggplot2
  ##          

  ## Get the data and merge into one data table.
  merged <- readdata(strFilePath, DOWNLD_UNZIP, USE_INET2)

  ## Subset the data to get only coal combustion-related sources
  coals1 <- merged[grep("coal", merged$Short.Name, ignore.case=TRUE),]
  coalsum <- by(coals1$Emissions, coals1$year, sum)
  df <- as.data.frame(cbind(as.numeric(names(coalsum)), as.vector(coalsum)))
  names(df) <- c("Year","Emissions")

  ## Check to see if the ggplot2 package is installed and loaded.
  checkpkg("ggplot2")
  
  ## Generate the plot.
  png("plot5.png", width=700, height=700)  
  g <- ggplot(data=df, aes(Year, Emissions))
  g <- g + geom_point() + geom_smooth(method="lm", size=2, linetype=3) +
    labs(x="Year", y=expression('PM'[2.5]*' (tons)'),
         title=expression('Total PM'[2.5]*' (tons) from Coal Combustion-Related Sources'))
  print(g)
  dev.off()
  
  return(df)
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

