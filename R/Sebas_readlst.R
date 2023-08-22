
# Open a test lst file from a nonmem run

setwd("A:/02. Projects/BV100/BV100_120423")
nmFile <- scan(
  '012.lst',
  sep = "\n",
  what = character(),
  quiet = TRUE
)


# Termination message
#-------------------------------------------------------------------------------
minStart <- grep(pattern="#TERM:", x=nmFile)

minEnd <- grep(pattern='ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES',
               x=nmFile)

termMsg <- nmFile[(minStart+1):minEnd]
for(val in termMsg){print(val)}


# ETAbar
#-------------------------------------------------------------------------------
min <- grep(pattern="ETABAR:", x=nmFile)
end <- grep(pattern="P VAL.", x=nmFile)

ETAmsg <- nmFile[(min):(end)]

ETAbar_p <- scan(text=ETAmsg[4], what="")
ETAbar_p <- as.numeric(grep("\\d", ETAbar_p, value=T))


# OFV number
#-------------------------------------------------------------------------------
OFV <- grep(pattern="OBJECTIVE FUNCTION VALUE", x=nmFile, value=T)
OFV_nr <- strsplit(OFV[1],split = ":")
OFV_nr <- as.numeric(OFV_nr[[1]][2])


# Run date
#-------------------------------------------------------------------------------
run_date <- nmFile[1:2]


# Condition number
#-------------------------------------------------------------------------------
eigenStart <- grep('EIGENVALUES OF COR MATRIX OF ESTIMATE  ', x=nmFile)
eigen <- nmFile[eigenStart:(eigenStart+9)]

# problem is that number of rows depends on number of variables
# First possible occurrence is line 7
# Search for 'E' in the lines
eigen_vals <- c()
if(grepl('E',eigen[7], fixed = T)){
  eigen_vals <- paste(eigen_vals, eigen[7])
}
if(grepl('E',eigen[8], fixed = T)){
  eigen_vals <- paste(eigen_vals, eigen[8])
}
if(grepl('E',eigen[9], fixed = T)){
  eigen_vals <- paste(eigen_vals, eigen[9])
}
if(grepl('E',eigen[10], fixed = T)){
  eigen_vals <- paste(eigen_vals, eigen[10])
}
eigen_vals2 <- scan(text=eigen_vals, what="")

cond_nr <- as.numeric(tail(eigen_vals2, n=1))/as.numeric(eigen_vals2[[1]])



# ext file with final estimates
#-------------------------------------------------------------------------------

ext <- read.table('111.ext', skip=1, header=TRUE, sep='')
ext2 <- ext[ext$ITERATION < 0 ,]
finalest <- head(ext2, n=1)

# can request with name e.g. finalest['THETA2']


