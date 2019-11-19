# get names of csv files in the designated path
findCSV <- function(inputPath) {
  
  fileNames <- list.files(inputPath)
  fileNamesSplit <- strsplit(fileNames, '\\.')
  ext <- sapply(fileNamesSplit, function(x) x[length(x)])
  return(fileNames[ext == 'csv'])
  
}

# fild readsettings.csv
readValidation <- function(validationPath) {
  
  fileName <- 'readsettings.csv'
  if(! fileName %in% list.files(validationPath)) {
    stop('readsetting.csv does not exist in the designated path.')
  }
  
  # get maximum number of the columns
  pathName <- paste0(validationPath, '/', fileName)
  nCol <- count.fields(pathName, sep = ',')
  maxCol <- max(nCol)
  
  # input
  valDF <- read.csv(pathName, 
                    header = F, stringsAsFactors = F, fill = T, 
                    col.names = paste0('C', seq_len(maxCol)))
  
  # reshape to list
  val <- list()
  for(i in 1:nrow(valDF)) {
    nData <- (sum(valDF[i, ] != '') - 1) / 2
    val[[valDF[i, 1]]] <- as.character(valDF[i, 2 * (1:nData)])
  } 
  return(val)
  
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

# find validation pattern
valFind <- function(val0, x) {
  
  if(length(val0) != length(x)) return(F)
  if(any(is.na(x))) return(F)
  
  if(all(val0 == x)) {
    return(T)
  } else {
    return(F)
  }
  
}


#' Finding csv files with irregular format
#' 
#' @param inputPath Path of csv files obtained through Bluetooth Recoder
#' @param validationPath Path of csv files of validation files
#' 
#' @export
#'
checkInputFiles <- function(inputPath, validationPath) {
  
  # get file names of input
  inputFileNames <- findCSV(inputPath)
  
  # get validation data
  val <- readValidation(validationPath)
  for(fn in inputFileNames) {
    
    error1 <- F
    error2 <- F
    cat(paste(rep('=', 70), collapse = ''))
    cat('\n')
    cat(fn)
    cat('\n')
    
    # input data
    dat <- read.csv(paste0(inputPath, '/', fn), header = F, colClasses = 'character')
    
    # find row with NA
    datNA <- is.na(dat$V7) | dat$V7 == ''
    if(any(datNA)) {
      errorRow <- displaySeq(which(datNA))
      cat(paste('Irregular format in row', errorRow))
      cat('\n')
      error1 <- T
    }
    
    # find row with irregular trait pattern
    while(nrow(dat) > 0) {
      
      if(error1) break
      
      # choose row which 'V6' and 'V7' is the same
      nr1 <- which(dat$V6 == dat$V7)[1]
      dat0 <- dat[1:nr1, ]
      dat  <- dat[- (1:nr1), ]
      valPattern <- names(val)[sapply(val, valFind, x = dat0$V8)]
      if(length(valPattern) == 0) {
        errorRow <- as.integer(rownames(dat)[1]) - 1
        cat(paste('Irregular pattern of trait name in row', errorRow))
        cat('\n')
        error2 <- T
      }
      
    }
    
    # if no error was found
    if(!(error1 | error2)) {
      cat('No error was found.')
      cat('\n')
    }
    
  }
  
}


#' Reshaping csv files: put data of same validation form together
#' 
#' @param inputPath Path of csv files obtained through Bluetooth Recoder
#' @param validationPath Path of csv files of validation files
#' @param outFolderName folder name for output data
#' 
#' @export
#'
gatherInputFiles <- function(
  
  inputPath, validationPath, outputFolderName = 'step1_gather'
  
) {
  
  # get file names of input
  inputFileNames <- findCSV(inputPath)
  
  # get validation data
  val <- readValidation(validationPath)

  # prepare empty list
  datSep <- list()
  for(valName in names(val)) datSep[[valName]] <- data.frame(matrix(nrow = 0, ncol = 8))
  
  for(fn in inputFileNames) {
    
    error1 <- F
    error2 <- F
    cat(paste(rep('=', 70), collapse = ''))
    cat('\n')
    cat(fn)
    cat('\n')
    
    # input data
    dat <- read.csv(paste0(inputPath, '/', fn), header = F, colClasses = 'character')
    
    # find row with NA
    datNA <- is.na(dat$V7) | dat$V7 == ''
    if(any(datNA)) {
      errorRow <- displaySeq(which(datNA))
      cat(paste('Irregular format in row', errorRow))
      cat('\n')
      error1 <- T
    }
    
    # find row with irregular trait pattern
    while(nrow(dat) > 0) {
      
      if(error1) break
      
      nr1 <- which(dat$V6 == dat$V7)[1]
      dat0 <- dat[1:nr1, ]
      dat  <- dat[- (1:nr1), ]
      valPattern <- names(val)[sapply(val, valFind, x = dat0$V8)]
      if(length(valPattern) == 0) {
        errorRow <- as.integer(rownames(dat)[1]) - 1
        cat(paste('Irregular pattern of trait name in row', errorRow))
        cat('\n')
        error2 <- T
      } else {
        datSep[[valPattern]] <- rbind(datSep[[valPattern]], dat0)
      }
      
    }
    
    # if no error was found
    if(!(error1 | error2)) {
      cat('No error was found.')
      cat('\n')
    }
    
  }
  
  # output
  if(! outputFolderName %in% list.files()) {
    dir.create(outputFolderName)
  }
  for(i in 1:length(datSep)) {
    if(nrow(datSep[[i]]) != 0) {
      outputFileName <- paste0(outputFolderName, '/', names(datSep)[i], ".csv")
      write.csv(datSep[[i]], outputFileName, row.names = F)
    }
  }
  
}
