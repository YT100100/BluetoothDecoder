# get names of csv files in the designated path
findCSV <- function(inputPath) {
  
  fileNames <- list.files(inputPath)
  fileNamesSplit <- strsplit(fileNames, '\\.')
  ext <- sapply(fileNamesSplit, function(x) x[length(x)])
  return(fileNames[ext == 'csv'])
  
}

# display numeric sequence in short form
displaySeq <- function(x) {
  
  iStart <- NULL
  iEnd <- NULL
  for(i in x) {
    if(! (i - 1) %in% x) iStart <- c(iStart, i)
    if(! (i + 1) %in% x) iEnd <- c(iEnd, i)
  }
  iRange <- mapply(function(s, e) if(s == e) s else paste0(s, '-', e), iStart, iEnd)
  return(paste(iRange, collapse = ', '))
  
}


##' Reshaping csv files in useful format
#' 
#' @param inputPath Path of csv files obtained through 'gatherInputFiles'
#' @param outFolderName Folder name for output data
#' 
#' @export
#'
reshapeData <- function(inputPath, outputFolderName = 'step2_reshape') {
  
  # get file names of input
  inputFileNames <- findCSV(inputPath)
  
  # create folder for output
  if(! outputFolderName %in% list.files()) {
    dir.create(outputFolderName)
  }
  
  # processing
  for(inputFileName in inputFileNames) {
    reshapeDataEach(inputPath, inputFileName, outputFolderName)
  }
  
}


##' Reshaping csv files in useful format for each files
#' 
#' @param inputPath Path of csv files obtained through 'gatherInputFiles'
#' @param inputFileName File Name of csv
#' 
#' @export
#'
reshapeDataEach <- function(inputPath, inputFileName, outputFolderName) {
  
  cat(paste(rep('=', 50), collapse = ''))
  cat(paste0('\n', inputFileName, '\n'))
  
  # input data
  inputFilePath <- paste0(inputPath, '/', inputFileName)
  dat <- read.csv(inputFilePath, colClasses = 'character')
  
  # rename duplicated trait names
  nTrait <- tapply(dat$V8, dat$V8, length)
  if(any(nTrait != nTrait[1])) {
    cat('Duplicated trait name exists\n')
    traitName <- dat$V8[1:dat$V7[1]]
    traitNameDup <- duplicated(traitName)
    traitNameNew <- traitName
    traitNameNew[traitNameDup] <- paste0(traitNameNew[traitNameDup], '_dup', 1:sum(traitNameDup))
    dat$V8 <- rep(traitNameNew, length.out = nrow(dat))
  }
  
  # separate data into each repeat
  datSep <- tapply(dat$V3, dat$V8, function(x) x)
  datSep <- data.frame(sapply(datSep, function(x) x))
  memo <- tapply(dat$V4, dat$V8, function(x) x)
  time <- tapply(dat$V1, dat$V8, function(x) x)
  datSep <- data.frame(date = substr(time[[1]], 1, 10), 
                       time = time[[1]], 
                       datSep, 
                       memo = memo[[1]])
  
  # remove duplicated data
  dup <- which(duplicated(datSep))
  if(length(dup) > 0) {
    cat(paste("Removed duplicated row", displaySeq(dup), '\n'))
    datDup <- datSep[- dup, ]
  } else {
    cat(paste('No duplicated row was found.\n'))
    datDup <- datSep
  }
  
  # separate for each measured date
  for(d in unique(datDup$date)) {
    
    datDate0 <- subset(datDup, date == d)
    
    # display memos
    memo <- as.character(datDate0$memo)
    memoExist <- ! memo %in% c(NA, '')
    memoMessage <- paste("Memo in row", displaySeq(which(memoExist)), "of date", d, '\n')
    if(any(memoExist)) cat(memoMessage)
    
    # output
    d0 <- format(as.Date(d, format = "%Y/%m/%d"), format = "%Y%m%d")
    valName <- strsplit(inputFileName, '\\.')[[1]][1]
    outputFileName <- paste0(outputFolderName, '/', valName, "_", d0, ".csv")
    write.csv(datDate0[-1], outputFileName, row.names = F)
    
  }
  
}
