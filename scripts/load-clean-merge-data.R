#### INITIALIZATIONS #### 

# Whether to plot diagnostic plots
plotCompare <- FALSE

# Colors for diagnostic plots
library("MetBrewer")
colON <- met.brewer("Tiepolo", n = 5, type = "discrete")
colOld <- colON[2]
colNew <- colON[3]
colCombined <- colON[5]

# Sliding window function
source("usefulFunctions.R")

#### LOAD DATA ####

##### Criblage data #####

# Old Source of the data:  
# <https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-pour-le-depistage-indicateurs-sur-les-mutations/>
# New source of the data: 
# <https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-pour-le-depistage-a-compter-du-18-05-2022-si-dep/>
  
# OLD URLS
URL_Deps_old <- "https://www.data.gouv.fr/fr/datasets/r/4d3e5a8b-9649-4c41-86ec-5420eb6b530c"
URL_Regions_old <- "https://www.data.gouv.fr/fr/datasets/r/5ff0cad6-f150-47ea-a4e0-57e354c1b2a4"
URL_France_old <- "https://www.data.gouv.fr/fr/datasets/r/848debc4-0e42-4e3b-a176-afc285ed5401"

# NEW URLS
# The commented versions are for the older versions of the files (between Jan 2022 and May 2022)
URL_Regions <- "https://www.data.gouv.fr/fr/datasets/r/0f26c511-e237-4b76-8a8b-8d8b37b7b287"
URL_France <- "https://www.data.gouv.fr/fr/datasets/r/3e4fa086-8642-4d30-8316-7d4ec4cf893b"
URL_Deps <- "https://www.data.gouv.fr/fr/datasets/r/bc318bc7-fb90-4e76-a6cb-5cdc0a4e5432"
#URL_Deps <- "https://www.data.gouv.fr/fr/datasets/r/ba8219dc-948c-418a-9116-f792c79c54b8"
#URL_Regions <- "https://www.data.gouv.fr/fr/datasets/r/9ed7f76c-09bc-43fb-997a-a1733faa6e8b"
#URL_France <- "https://www.data.gouv.fr/fr/datasets/r/7eade8d7-f79a-4c7f-8579-51f3f7104cfb"

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
  # This control exists because at some point the file was missing
  dat.Deps.new <- read.csv("../data/muts_Deps.csv", sep = ";", stringsAsFactors = FALSE)
}


##### Test data #####

# Source:
# <https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/#>
# New Source:
# <https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-pour-le-depistage-a-compter-du-18-05-2022-si-dep/>
  
URLtest_France <- "https://www.data.gouv.fr/fr/datasets/r/d349accb-56ef-4b53-b218-46c2a7f902e0" #"https://www.data.gouv.fr/fr/datasets/r/dd0de5d9-b5a5-4503-930a-7b08dc0adc7c"
URLtest_Reg <- "https://www.data.gouv.fr/fr/datasets/r/8b382611-4b86-41ff-9e58-9ee638a6d564" #"https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01"
URLtest_Dep <- "https://www.data.gouv.fr/fr/datasets/r/674bddab-6d61-4e59-b0bd-0be535490db0" #"https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675"

if(dlData){
  download.file(URLtest_France, "../data/tests_France.csv")
  download.file(URLtest_Reg, "../data/tests_Reg.csv")
  download.file(URLtest_Dep, "../data/tests_Deps.csv")
  
  system("git add ../data/tests*csv")
  system("git commit -m 'update test data'")
}

# Load data
tests.France <- read.csv("../data/tests_France.csv", sep = ";", stringsAsFactors = FALSE, dec = ",")
tests.Reg <- read.csv("../data/tests_Reg.csv", sep = ";", stringsAsFactors = FALSE, dec = ",")
tests.Deps <- read.csv("../data/tests_Deps.csv", sep = ";", stringsAsFactors = FALSE, dec = ",")

# Criblage is without age classes: remove them now to avoid errors
#tests.France <- tests.France[which(tests.France$cl_age90 == 0), ]
#tests.Reg <- tests.Reg[which(tests.Reg$cl_age90 == 0), ]
#tests.Deps <- tests.Deps[which(tests.Deps$cl_age90 == 0), ]

# Hopefully this is only temporary: the 0 age class with all data has gone missing...
tF <- aggregate(tests.France[, c("P", "T")], by = list("jour" = tests.France$jour), FUN = sum)
tR <- aggregate(tests.Reg[, c("P", "T")], by = list("reg" = tests.Reg$reg, "jour" = tests.Reg$jour), FUN = sum)
head(tests.Deps)
tD <- aggregate(tests.Deps[, c("P", "T")], by = list("dep" = tests.Deps$dep, "jour" = tests.Deps$jour), FUN = sum)

tests.France <- tF
tests.Reg <- tR
tests.Deps <- tD
#--- end of hopefully temporary section


# Compute sliding averages
tests.France$P7j <- sliding.window(tests.France$P)
tests.France$T7j <- sliding.window(tests.France$T)

# Note: Sliding averages for Reg and Deps need to be computed separately, because
# need to do it for each location independently


#### DELISSAGE ####

## Define functions

source("deliss.R")

# # Define function to "delisser" the data
# deliss <- function(s, print = FALSE){
#   # s vector to be delissé 
#   # print: bool, whether to print diagnotic outputs
#   
#   # Create output of daily data
#   n <- c(rep(0, 7), rep(NA, length(s) - 7))
#   # Label days along the week
#   values <- rep(1:7, ceiling(length(s)/7))[1:length(s)]
#   
#   # Recursion; Loop on values
#   # Here s is the sum of days [i-6, i]
#   for(i in 8:length(n)){
#     n[i] <- s[i] - s[i-1] + n[i-7]
#   }
#   
#   # Find the minimum values for each of the first 7 points
#   # such that there is no negative value
#   for(j in 1:7){ # Days along a week
#     mj <- min(n[values == j])
#     if(mj < 0){ 
#       # If there are negative values, set the new value, 
#       # but if there are no negative values, keep 0
#       n[j] <- - min(n[values == j])
#     }
#   }
#   if(print) print(n)
#   
#   # Repeat delissage to remove negative values
#   for(i in 8:length(n)){
#     n[i] <- s[i] - s[i-1] + n[i-7]
#   }
# 
#   # Check how many tests are still missing
#   difference <- s[7] - sum(n[1:7])
#   if(print) print(difference)
#   
#   if(difference < 0) error("Investigate, difference is negative")
#   
#   # If we have not assigned all values
#   if(difference > 0){
#     
#     # Compute the distribution of numbers of tests along the days of the week
#     aggn <- aggregate(n, by = list(values), FUN = sum)
#     sumx <- sum(aggn$x)
#     if(sumx > 0){
#       aggn$p <- aggn$x / sumx
#     }else{ # Can be = 0...!
#       aggn$p <- aggn$x
#     }
#     
#     # Assign the left-over values according to this distribution
#     newn <- rep(NA, 7)
#     newn[1:6] <- round(aggn[1:6, "p"] * difference)
#     newn[7] <- difference - sum(newn[1:6])
#     if(print) print(newn)
#     
#     # If we had too many tests in the previous days, and the last one ends up negative,
#     # remove values before
#     while(newn[7] < 0){
#       # Choose one day at random
#       k <- sample(1:6, size = 1)
#       if(newn[k] > 0){ # If we can remove a day
#         newn[k] <- newn[k] - 1 # Remove it
#         newn[7] <- difference - sum(newn[1:6]) # Recompute newn[7]
#       }
#     }
#     
#     if(print) print(newn)
#     stopifnot(all(newn >= 0)) # Security check
#     
#     # Add them to the ns
#     n[1:7] <- n[1:7] + newn
#   }
#   
#   # Repeat delissage with these values
#   for(i in 8:length(n)){
#     n[i] <- s[i] - s[i-1] + n[i-7]
#   }
#   
#   n
# }
# 
# # Function to delisser multiple columns and add date
# # Need to be careful with dates
# delissWithDate <- function(dat, cols, print = FALSE){
#   # dat our dataset, with time columns
#   # cols columns to be "delisee", as vector of chars
#   # Check that all dates are consecutive
#   dates <- as.Date(as.Date(substring(dat$semaine, 12, 21)))
#   stopifnot(all(as.numeric(diff(dates)) == 1))
#   out <- dat
#   for(col in cols){
#     out[, paste0(col, ".dl")] <- deliss(dat[, col], print = print)
#   }
#   out$dateDeliss <- dates
#   out 
# }

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
  if(plotCompare){ # Only do this if plotCompare == TRUE
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

# Finalize merge of old and new datasets
finalizeMerge <- function(dat){
  # dat : merged dataset
  
  # Format date
  dat$date <- dat$dateDeliss
  
  # nb_pos
  dat$nb_pos <- dat$nb_pos.x # New ones for times >= 2021-01-06
  #   and add old ones
  dat[is.na(dat$nb_pos), "nb_pos"] <- dat[is.na(dat$nb_pos), "nb_pos.y"] 
  #compare.xy(dat, "nb_pos")
  
  # nb_crib
  dat[, "nb_crib"] <- computeSum(dat, "nb_crib")
  
  # Other columns
  for(cible in c("A0", "A1", "C0", "C1")){
    for(suffix in c("", ".dl", ".sum7_mid")){
      dat[, paste0("nb_", cible, suffix)] <- computeSum(dat, paste0("nb_", cible, suffix))
    }
  }
  
  return(dat)
}



## France
tmp.France <- finalizeMerge(merge(dat.France.new, dat.France.old, all = TRUE, by = c("dateDeliss")))

## Regions
tmp.Regions <- finalizeMerge(merge(dat.Regions.new, dat.Regions.old, all = TRUE, by = c("reg", "dateDeliss")))

## Departements
tmp.Deps <- finalizeMerge(merge(dat.Deps.new, dat.Deps.old, all = TRUE, by = c("dep", "dateDeliss")))


#------

## Rename data

dat.France <- tmp.France
dat.Regions <- tmp.Regions
dat.Deps <- tmp.Deps


#### COMBINE CRIBLAGE AND TESTS ####

# Turn dates into dates for merging
tests.France$date <- as.Date(tests.France$jour)
tests.Reg$date <- as.Date(tests.Reg$jour)
tests.Deps <- tests.Deps[nchar(tests.Deps$jour) == 10, ]
tests.Deps$date <- as.Date(tests.Deps$jour)

unique(tests.Reg$reg)
unique(tests.Deps$dep)
unique(dat.Deps$dep)
head(tests.France)
head(dat.France)

dat.France <- merge(dat.France, tests.France, all = TRUE, by = "date")

dat.Regions <- merge(dat.Regions, tests.Reg, all = TRUE, by = c("date", "reg"))

if(!is.na(URL_Deps)){
  dat.Deps <- merge(dat.Deps, tests.Deps, all = TRUE, by = c("date", "dep"))
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

