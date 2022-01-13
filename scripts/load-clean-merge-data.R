#### INITIALIZATIONS #### 

# Sliding window function
source("usefulFunctions.R")


#### LOAD DATA ####

##### Criblage data #####

# Source of the data:  
# <https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-pour-le-depistage-indicateurs-sur-les-mutations/>
  
# OLD URLS
URL_Deps_old <- "https://www.data.gouv.fr/fr/datasets/r/4d3e5a8b-9649-4c41-86ec-5420eb6b530c"
URL_Regions_old <- "https://www.data.gouv.fr/fr/datasets/r/5ff0cad6-f150-47ea-a4e0-57e354c1b2a4"
URL_France_old <- "https://www.data.gouv.fr/fr/datasets/r/848debc4-0e42-4e3b-a176-afc285ed5401"

# NEW URLS
URL_Regions <- "https://www.data.gouv.fr/fr/datasets/r/9ed7f76c-09bc-43fb-997a-a1733faa6e8b"
URL_France <- "https://www.data.gouv.fr/fr/datasets/r/7eade8d7-f79a-4c7f-8579-51f3f7104cfb"
URL_Deps <- "https://www.data.gouv.fr/fr/datasets/r/ba8219dc-948c-418a-9116-f792c79c54b8"

## Old data
# Download old data
# Needed to be done only once because the files do not change anymore

# download.file(URL_France_old, 
#                 destfile="../data/muts_France_old.csv",
#                 method="curl",
#                 extra='-L')
# download.file(URL_Regions_old, 
#                 destfile="../data/muts_Regions_old.csv",
#                 method="curl",
#                 extra='-L')
# 
# download.file(URL_Deps_old, 
#                 destfile="../data/muts_Deps_old.csv",
#                 method="curl",
#                 extra='-L')

# Load old data
dat.France.old <- read.csv("../data/muts_France_old.csv", sep = ";", stringsAsFactors = FALSE)
dat.Regions.old <- read.csv("../data/muts_Regions_old.csv", sep = ";", stringsAsFactors = FALSE)
dat.Deps.old <- read.csv("../data/muts_Deps_old.csv", sep = ";", stringsAsFactors = FALSE)

## New data
# Download files from repo
# Need to use extra option to follow the redirection

if(dlData){
  download.file(URL_France, 
                destfile="../data/muts_France.csv",
                method="curl",
                extra='-L')
  download.file(URL_Regions, 
                destfile="../data/muts_Regions.csv",
                method="curl",
                extra='-L')
  if(!is.na(URL_Deps)){
    download.file(URL_Deps, 
                  destfile="../data/muts_Deps.csv",
                  method="curl",
                  extra='-L')
  }
}

# Save datasets
system("git add ../data/muts_*.csv")
system("git commit -m 'update data'")

# Load data
dat.France.new <- read.csv("../data/muts_France.csv", sep = ";", stringsAsFactors = FALSE)
dat.Regions.new <- read.csv("../data/muts_Regions.csv", sep = ";", stringsAsFactors = FALSE)

if(!is.na(URL_Deps)){
  dat.Deps.new <- read.csv("../data/muts_Deps.csv", sep = ";", stringsAsFactors = FALSE)
}


##### Test data #####

# Source:
# <https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/#>
  
URLtest_France <- "https://www.data.gouv.fr/fr/datasets/r/dd0de5d9-b5a5-4503-930a-7b08dc0adc7c"
URLtest_Reg <- "https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01"
URLtest_Dep <- "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675"

if(dlData){
  download.file(URLtest_France, "../data/tests_France.csv")
  download.file(URLtest_Reg, "../data/tests_Reg.csv")
  download.file(URLtest_Dep, "../data/tests_Deps.csv")
  
  system("git add ../data/tests*csv")
  system("git commit -m 'update test data'")
}

# Load data
tests.France <- read.csv("../data/tests_France.csv", sep = ";", stringsAsFactors = FALSE)
tests.Reg <- read.csv("../data/tests_Reg.csv", sep = ";", stringsAsFactors = FALSE)
tests.Deps <- read.csv("../data/tests_Deps.csv", sep = ";", stringsAsFactors = FALSE)

# Criblage is without age classes: remove them now to avoid errors
tests.France <- tests.France[which(tests.France$cl_age90 == 0), ]
tests.Reg <- tests.Reg[which(tests.Reg$cl_age90 == 0), ]
tests.Deps <- tests.Deps[which(tests.Deps$cl_age90 == 0), ]

# Compute sliding averages
tests.France$P7j <- sliding.window(tests.France$P)
tests.France$T7j <- sliding.window(tests.France$T)

# Note: Sliding averages for Reg and Deps need to be computed separately, because
# need to do it for each location independently


#### DELISSAGE ####

## Define functions

# Define function to "delisser" the data
deliss <- function(s, print = FALSE){
  # s vector to be delissé 
  # print: bool, whether to print diagnotic outputs
  
  # Create output of daily data
  n <- c(rep(0, 7), rep(NA, length(s) - 7))
  # Label days along the week
  values <- rep(1:7, ceiling(length(s)/7))[1:length(s)]
  
  # Recursion; Loop on values
  for(i in 8:length(n)){
    n[i] <- s[i] - s[i-1] + n[i-7]
  }
  
  # Find the minimum values for each of the first 7 points
  # such that there is no negative value
  for(j in 1:7){ # Days along a week
    n[j] <- - min(n[values == j])
  }
  if(print) print(n)
  
  # Check how many tests are still missing
  difference <- s[7] - sum(n[1:7])
  if(print) print(difference)
  
  if(difference < 0) error("Investigate, difference is negative")
  
  # If we have not assigned all values
  if(difference > 0){
    # Repeat delissage to remove negative values
    for(i in 8:length(n)){
      n[i] <- s[i] - s[i-1] + n[i-7]
    }
    
    # Compute the distribution of numbers of tests along the days of the week
    aggn <- aggregate(n, by = list(values), FUN = sum)
    sumx <- sum(aggn$x)
    if(sumx > 0){
      aggn$p <- aggn$x / sumx
    }else{ # Can be = 0...!
      aggn$p <- aggn$x
    }
    
    # Assign the left-over values according to this distribution
    newn <- rep(NA, 7)
    newn[1:6] <- round(aggn[1:6, "p"] * difference)
    newn[7] <- difference - sum(newn[1:6])
    if(print) print(newn)
    
    # If we had too many tests in the previous days, and the last one ends up negative,
    # remove values before
    while(newn[7] < 0){
      # Choose one day at random
      k <- sample(1:6, size = 1)
      if(newn[k] > 0){ # If we can remove a day
        newn[k] <- newn[k] - 1 # Remove it
        newn[7] <- difference - sum(newn[1:6]) # Recompute newn[7]
      }
    }
    
    if(print) print(newn)
    stopifnot(all(newn >= 0)) # Security check
    
    # Add them to the ns
    n[1:7] <- n[1:7] + newn
  }
  
  # Repeat delissage with these values
  for(i in 8:length(n)){
    n[i] <- s[i] - s[i-1] + n[i-7]
  }
  
  n
}

# Function to delisser multiple columns and add date
# Need to be careful with dates
delissWithDate <- function(dat, cols, print = FALSE){
  # dat our dataset, with time columns
  # cols columns to be "delisee", as vector of chars
  # Check that all dates are consecutive
  dates <- as.Date(as.Date(substring(dat$semaine, 12, 21)))
  stopifnot(all(as.numeric(diff(dates)) == 1))
  out <- dat
  for(col in cols){
    out[, paste0(col, ".dl")] <- deliss(dat[, col], print = print)
  }
  out$dateDeliss <- dates
  out 
}

## Do it!
# Column names in the datasets
cols.old <- c("nb_A0", "nb_A1", "nb_B0", "nb_B1", "nb_C0", "nb_C1")
cols.new <- c("nb_A0", "nb_A1", "nb_C0", "nb_C1", "nb_D0", "nb_D1", "nb_A0C0", "nb_A01C01")

#------

# France
dat.France.old <- delissWithDate(dat.France.old, cols.old)
dat.France.new <- delissWithDate(dat.France.new, cols.new)

#------

# By regions
## Old
regs <- sort(unique(dat.Regions.old$reg))
# Initialize
i <- 1
tmpdat <- dat.Regions.old[which(dat.Regions.old$reg == regs[i]), ]
tmp <- delissWithDate(tmpdat, cols.old)

for(i in 2:length(regs)){
  tmpdat <- dat.Regions.old[which(dat.Regions.old$reg == regs[i]), ]
  tmp <- rbind(tmp, delissWithDate(tmpdat, cols.old))
}
dat.Regions.old <- tmp

#delissWithDate(tmpdat, cols.old, print = TRUE)
## New
regs <- sort(unique(dat.Regions.new$reg))
# Initialize
i <- 1
tmpdat <- dat.Regions.new[which(dat.Regions.new$reg == regs[i]), ]
tmp <- delissWithDate(tmpdat, cols.new)

for(i in 2:length(regs)){
  tmpdat <- dat.Regions.new[which(dat.Regions.new$reg == regs[i]), ]
  tmp <- rbind(tmp, delissWithDate(tmpdat, cols.new))
}
dat.Regions.new <- tmp


#------

# By départements
## Old
deps <- sort(unique(dat.Deps.old$dep))
# Initialize
i <- 1
tmpdat <- dat.Deps.old[which(dat.Deps.old$dep == deps[i]), ]
tmp <- delissWithDate(tmpdat, cols.old)
# Loop through deps
for(i in 2:length(deps)){
  tmpdat <- dat.Deps.old[which(dat.Deps.old$dep == deps[i]), ]
  tmp <- rbind(tmp, delissWithDate(tmpdat, cols.old))
}
dat.Deps.old <- tmp

## New
deps <- sort(unique(dat.Deps.new$dep))
# Initialize
i <- 1
tmpdat <- dat.Deps.new[which(dat.Deps.new$dep == deps[i]), ]
tmp <- delissWithDate(tmpdat, cols.new)
# Loop through deps
for(i in 2:length(deps)){
  tmpdat <- dat.Deps.new[which(dat.Deps.new$dep == deps[i]), ]
  tmp <- rbind(tmp, delissWithDate(tmpdat, cols.new))
}
dat.Deps.new <- tmp

#### COMBINE OLD AND NEW CRIBLAGE DATASETS ####


# Useful functions

pchOld <- 16
pchNew <- 15
pchCombined <- 5
lwdCombined <- 1.5

# Plot colums to check that things are done alright
compare.xy <- function(dat, col, addCombined = TRUE){
  # dat: dataset
  # col: column
  par(las = 1, xpd = TRUE)
  plot(dat$dateMid, dat[, paste0(col, ".x")], xlab = "", ylab = "", main = col, col = colNew, pch = pchNew)
  points(dat$dateMid, dat[, paste0(col, ".y")], col = colOld, pch = pchOld, cex = 1)
  
  if(addCombined){
    points(dat$dateMid, dat[, col], pch = pchCombined, col = colCombined, lwd = lwdCombined)
    legend("topleft", col = c(colNew, colOld, colCombined), pch = c(pchNew, pchOld, pchCombined), legend = c("new", "old", "combined"), bty = "n", lwd = c(1, 1, lwd = lwdCombined), lty = 0)
  }else{
    legend("topleft", col = c(colNew, colOld), pch = c(pchNew, pchOld), legend = c("new", "old"), bty = "n")
  }
  par(xpd = FALSE)
}

# Sum columns, in spite of NAs
computeSum <- function(dat, col){
  tmpx <- dat[, paste0(col, ".x")]
  tmpy <- dat[, paste0(col, ".y")]
  
  # Set NAs to 0
  tmpx[is.na(tmpx)] <- 0
  tmpy[is.na(tmpy)] <- 0
  
  # Return sum
  tmpx + tmpy
}

#------

## France

tmp.France <- merge(dat.France.new, dat.France.old, all = TRUE, by = c("fra", "semaine", "dateDeliss"))
dim(dat.France.new)
dim(dat.France.old)
dim(tmp.France)
head(tmp.France)

# Format date
tmp.France$date1 <- as.Date(substring(tmp.France$semaine, 1, 10))
tmp.France$date2 <- as.Date(substring(tmp.France$semaine, 12, 21))
tmp.France$dateDeliss <- tmp.France$date2

# Mid-interval date
tmp.France$dateMid <- tmp.France$date2 - 3

# Rewrite time as days since beginning of the data
tmp.France$time <- tmp.France$dateMid - min(tmp.France$dateMid)

# nb_crib
tmp.France$nb_crib <- computeSum(tmp.France, "nb_crib")
compare.xy(tmp.France, "nb_crib")

# nb_pos
tmp.France$nb_pos <- tmp.France$nb_pos.x # New ones for times >= 2021-01-06
tmp.France[is.na(tmp.France$nb_pos), "nb_pos"] <- tmp.France[is.na(tmp.France$nb_pos), "nb_pos.y"] # Old ones
compare.xy(tmp.France, "nb_pos")

# A0
tmp.France$nb_A0 <- computeSum(tmp.France, "nb_A0")
compare.xy(tmp.France, "nb_A0")

# A1
tmp.France$nb_A1 <- computeSum(tmp.France, "nb_A1")
compare.xy(tmp.France, "nb_A1")

# C0
tmp.France$nb_C0 <- computeSum(tmp.France, "nb_C0")
compare.xy(tmp.France, "nb_C0")

# C1
tmp.France$nb_C1 <- computeSum(tmp.France, "nb_C1")
compare.xy(tmp.France, "nb_C1")

# .dl
tmp.France$nb_A0.dl <- computeSum(tmp.France, "nb_A0.dl")
tmp.France$nb_A1.dl <- computeSum(tmp.France, "nb_A1.dl")
tmp.France$nb_C0.dl <- computeSum(tmp.France, "nb_C0.dl")
tmp.France$nb_C1.dl <- computeSum(tmp.France, "nb_C1.dl")

#------

## Regions

tmp.Regions <- merge(dat.Regions.new, dat.Regions.old, all = TRUE, by = c("reg", "semaine", "dateDeliss"))
dim(dat.Regions.new)
dim(dat.Regions.old)
dim(tmp.Regions)
head(tmp.Regions)
head(dat.Regions.new)

# Format date
tmp.Regions$date1 <- as.Date(substring(tmp.Regions$semaine, 1, 10))
tmp.Regions$date2 <- as.Date(substring(tmp.Regions$semaine, 12, 21))
tmp.Regions$dateDeliss <- tmp.Regions$date2

# Mid-interval date
tmp.Regions$dateMid <- tmp.Regions$date2 - 3

# Rewrite time as days since beginning of the data
tmp.Regions$time <- tmp.Regions$dateMid - min(tmp.Regions$dateMid)

# nb_crib
tmp.Regions$nb_crib <- computeSum(tmp.Regions, "nb_crib")
compare.xy(tmp.Regions, "nb_crib")

# nb_pos
tmp.Regions$nb_pos <- tmp.Regions$nb_pos.x # New ones for times >= 2021-01-06
tmp.Regions[is.na(tmp.Regions$nb_pos), "nb_pos"] <- tmp.Regions[is.na(tmp.Regions$nb_pos), "nb_pos.y"] # Old ones
compare.xy(tmp.Regions, "nb_pos")

# A0
tmp.Regions$nb_A0 <- computeSum(tmp.Regions, "nb_A0")
compare.xy(tmp.Regions, "nb_A0")

# A1
tmp.Regions$nb_A1 <- computeSum(tmp.Regions, "nb_A1")
compare.xy(tmp.Regions, "nb_A1")

# C0
tmp.Regions$nb_C0 <- computeSum(tmp.Regions, "nb_C0")
compare.xy(tmp.Regions, "nb_C0")

# C1
tmp.Regions$nb_C1 <- computeSum(tmp.Regions, "nb_C1")
compare.xy(tmp.Regions, "nb_C1")

# .dl
tmp.Regions$nb_A0.dl <- computeSum(tmp.Regions, "nb_A0.dl")
tmp.Regions$nb_A1.dl <- computeSum(tmp.Regions, "nb_A1.dl")
tmp.Regions$nb_C0.dl <- computeSum(tmp.Regions, "nb_C0.dl")
tmp.Regions$nb_C1.dl <- computeSum(tmp.Regions, "nb_C1.dl")

#------

## Departements

if(!is.na(URL_Deps)){
  tmp.Deps <- merge(dat.Deps.new, dat.Deps.old, all = TRUE, by = c("dep", "semaine", "dateDeliss"))
  dim(dat.Deps.new)
  dim(dat.Deps.old)
  dim(tmp.Deps)
  head(tmp.Deps)
  head(dat.Deps.new)
  
  # Format date
  tmp.Deps$date1 <- as.Date(substring(tmp.Deps$semaine, 1, 10))
  tmp.Deps$date2 <- as.Date(substring(tmp.Deps$semaine, 12, 21))
  tmp.Deps$dateDeliss <- tmp.Deps$date2
  
  # Mid-interval date
  tmp.Deps$dateMid <- tmp.Deps$date2 - 3
  
  # Rewrite time as days since beginning of the data
  tmp.Deps$time <- tmp.Deps$dateMid - min(tmp.Deps$dateMid)
  
  
  # nb_crib
  tmp.Deps$nb_crib <- computeSum(tmp.Deps, "nb_crib")
  #compare.xy(tmp.Deps, "nb_crib")
  
  # nb_pos
  tmp.Deps$nb_pos <- tmp.Deps$nb_pos.x # New ones for times >= 2021-01-06
  tmp.Deps[is.na(tmp.Deps$nb_pos), "nb_pos"] <- tmp.Deps[is.na(tmp.Deps$nb_pos), "nb_pos.y"] # Old ones
  #compare.xy(tmp.Deps, "nb_pos")
  
  # A0
  tmp.Deps$nb_A0 <- computeSum(tmp.Deps, "nb_A0")
  #compare.xy(tmp.Deps, "nb_A0")
  
  # A1
  tmp.Deps$nb_A1 <- computeSum(tmp.Deps, "nb_A1")
  #compare.xy(tmp.Deps, "nb_A1")
  
  # C0
  tmp.Deps$nb_C0 <- computeSum(tmp.Deps, "nb_C0")
  #compare.xy(tmp.Deps, "nb_C0")
  
  # C1
  tmp.Deps$nb_C1 <- computeSum(tmp.Deps, "nb_C1")
  #compare.xy(tmp.Deps, "nb_C1")
  
  # .dl
  tmp.Deps$nb_A0.dl <- computeSum(tmp.Deps, "nb_A0.dl")
  tmp.Deps$nb_A1.dl <- computeSum(tmp.Deps, "nb_A1.dl")
  tmp.Deps$nb_C0.dl <- computeSum(tmp.Deps, "nb_C0.dl")
  tmp.Deps$nb_C1.dl <- computeSum(tmp.Deps, "nb_C1.dl")
  
  
}else{
  tmp.Deps <- dat.Deps.old
}

#------

## Rename data

dat.France <- tmp.France
dat.Regions <- tmp.Regions
dat.Deps <- tmp.Deps


#### COMBINE CRIBLAGE AND TESTS ####

# Turn dates into dates for merging
tests.France$dateMid <- as.Date(tests.France$jour)
tests.Reg$dateMid <- as.Date(tests.Reg$jour)
tests.Deps$dateMid <- as.Date(tests.Deps$jour)

unique(tests.Reg$reg)
unique(tests.Deps$dep)
unique(dat.Deps$dep)
head(tests.France)
head(dat.France)

dat.France <- merge(dat.France, tests.France, all = TRUE, by = "dateMid")

dat.Regions <- merge(dat.Regions, tests.Reg, all = TRUE, by = c("dateMid", "reg"))

if(!is.na(URL_Deps)){
  dat.Deps <- merge(dat.Deps, tests.Deps, all = TRUE, by = c("dateMid", "dep"))
}


#### ADD GEOGRAPHIC DATA ####

## Regions 

# Codes regions
URL <- "https://www.data.gouv.fr/en/datasets/r/34fc7b52-ef11-4ab0-bc16-e1aae5c942e7"
dataFile <- "../data/coderegions.csv"

if(!file.exists(dataFile)){
  download.file(URL, dataFile)
}

codesRegions <- read.csv(dataFile, sep = ",", stringsAsFactors = FALSE)

# Turn into dictionary
regs <- codesRegions$nom_region
names(regs) <- as.character(codesRegions$code_region)

# Add region name
dat.Regions$reg_name <- regs[as.character(dat.Regions$reg)]

unique(dat.Regions$reg_name)
unique(dat.Regions[which(is.na(dat.Regions$reg_name)), "reg"])


## Departments

# Add name
deps <- read.csv("../data/departement2020.csv", stringsAsFactors = FALSE)
# Turn into dictionnary
dps <- deps$libelle
names(dps) <- as.character(deps$dep)

if(!is.na(URL_Deps)){
  dat.Deps$departement <- dps[as.character(dat.Deps$dep)]
  
  unique(dat.Deps$departement)
  
  unique(dat.Deps[which(is.na(dat.Deps$departement)), "dep"])
  
  # 977 Saint-Barthélemy, 978 Saint-Martin
}

#### SAVE OUTPUTS ####
write.csv(dat.France, "../data/datCribTest_France.csv", row.names = FALSE)
write.csv(dat.Regions, "../data/datCribTest_Regions.csv", row.names = FALSE)
write.csv(dat.Deps, "../data/datCribTest_Deps.csv", row.names = FALSE)

# Save datasets
system("git add ../data/datCribTest_*.csv")
system("git commit -m 'update merged data'")

