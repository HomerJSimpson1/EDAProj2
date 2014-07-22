pm25dat <- function(strFilePath=getwd(), DOWNLD_UNZIP = TRUE, USE_INET2 = FALSE) {
  ## Exploratory Data Analysis Coursera Class
  ## Project 2
  ## Requires the following packages:
  ##          data.table  (data tables can be way faster, and seem to be very much faster for merging)
  ##          
  ##          

    
  ## #require(data.table)  ## Need to add a function that installs the data.table package if it's not already installed!
  ## checkpkg(data.table)  ## Need to add a function that installs the data.table package if it's not already installed!  

  ## ## Some basic path variables
  ## destdir <- strFilePath
  ## dstfile <- paste(destdir, "/exdata_data_NEI_data.zip", sep="")
  ## unzipdst <- paste(destdir, "/unzipped", sep="")

  ## ## ## Read the data into R.
  ## ## ##readdata(strFilePath, DOWNLD_UNZIP, USE_INET2)
  
  ## ## Get the data
  ## filelst <- readdata(destdir, dstfile, unzipdst, DOWNLD_UNZIP, USE_INET2)

  ## ## Read the files into R (there are 2 files).  The files are stored as .rds files, which are read into R
  ## ## using the readRDS function (and written using the saveRDS function).
  ## ## NEI <- readRDS(paste(unzipdst,"/",filelst[[2]], sep=""))
  ## ## SCC <- readRDS(paste(unzipdst,"/",filelst[[1]], sep=""))
  ## NEI <- as.data.table(readRDS(paste(unzipdst,"/",filelst[[2]], sep="")))
  ## SCC <- as.data.table(readRDS(paste(unzipdst,"/",filelst[[1]], sep="")))

  ## print(str(NEI))
  ## print(str(SCC))  
  

  ## ## Use merge() to join the two data frames.
  ## merged <- merge(NEI, SCC, by="SCC")


  
  ## Get the data
  merged <- readdata(strFilePath, DOWNLD_UNZIP, USE_INET2)

  
  ## Return the merged data frames?
  return(merged)
}




readdata <- function(strFilePath, DOWNLD_UNZIP, USE_INET2) {
#########readdata <- function(destdir, dstfile, unzipdst, DOWNLD_UNZIP, USE_INET2) {    
  ## Flag to determine if I should download and unzip the data or not.
  #DOWNLD_UNZIP <- FALSE
  #DOWNLD_UNZIP <- TRUE

  ## Flag to determine if I need to use setInternet2(use=TRUE)
  #USE_INET2 <- FALSE
  #USE_INET2 <- TRUE

  ## Set the values of some directories and filenames that will be used throughout.
  #destdir <- "C:/Users/petersj/My Documents/data"
  ## if (!file.exists("C:/temp"))
  ##   {
  ##     dir.create("C:/temp")
  ##   }

  #destdir <- "C:/Temp/data"
  #destdir <- getwd()
  ## destdir <- strFilePath 
  ## dstfile <- paste(destdir, "/exdata_data_NEI_data.zip", sep="")
  ## unzipdst <- paste(destdir, "/unzipped", sep="")

  #print(destdir)

  #require(data.table)  ## Need to add a function that installs the data.table package if it's not already installed!
  checkpkg("data.table")  ## Need to add a function that installs the data.table package if it's not already installed!

  ## Some basic path variables
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
  ## Top level directory for the unzipped files is now unzipdst + "UCI HAR Dataset"
  #filelst <- list.files(paste(unzipdst, "/UCI HAR Dataset", sep=""))
  filelst <- list.files(unzipdst)
  filelst
  print(filelst[[1]])
  print(filelst[[2]])


  ## ## Read the files into R (there are 2 files).  The files are stored as .rds files, which are read into R
  ## ## using the readRDS function (and written using the saveRDS function).
  ## NEI <- readRDS(paste(unzipdst,"/summarySCC_PM25.rds", sep=""))
  ## SCC <- readRDS(paste(unzipdst,"/Source_Classification_Code.rds", sep=""))
  ## print(str(NEI))
  ## print(str(SCC))


  ## Read the files into R (there are 2 files).  The files are stored as .rds files, which are read into R
  ## using the readRDS function (and written using the saveRDS function).
  ## NEI <- readRDS(paste(unzipdst,"/",filelst[[2]], sep=""))
  ## SCC <- readRDS(paste(unzipdst,"/",filelst[[1]], sep=""))
  NEI <- as.data.table(readRDS(paste(unzipdst,"/",filelst[[2]], sep="")))
  SCC <- as.data.table(readRDS(paste(unzipdst,"/",filelst[[1]], sep="")))

  print(str(NEI))
  print(str(SCC))  
  

  ## Use merge() to join the two data frames.
  merged <- merge(NEI, SCC, by="SCC")

  
  ##return(filelst)
  return(merged)  
}


datadownld <- function(fileURL, destdir, dstfile) {
  ## Download the data
  ## First, set the URL value.
  ## e.g. fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

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
  # download.file(fileURL, destfile = dstfile, method="curl")

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

