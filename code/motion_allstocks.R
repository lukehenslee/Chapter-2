#==================================================================================================
# Preparing data for GIS animation
# 9/12/2022
# Luke Henslee, College of Fisheries and Ocean Sciences, UAF
#
#==================================================================================================
#NOTES: This script combines capture data with detection data for animation with 
# GIS software.

#==================================================================================================

# Load packages
library(tidyverse)
library(zoo)
library(lubridate)


# Set working directory ####
setwd("H:/git_repo/Chapter-2")
data <- file.path(getwd(), "data")


# Data ####

## Load data ####
## Detection data
str <- read.csv(file=file.path(data, "detsum_colClasses.csv"))
detsum <- read.csv(file=file.path(data, "detsum.csv"), 
                colClasses = paste(str[1,]))

## Capture data
str <- read.csv(file=file.path(data, "mcode_colClasses.csv"))
tag <- read.csv(file=file.path(data, "mcode.csv"))

## Recapture data
recap <- read.csv(file=file.path(data, "recaptures.csv"))

## Receiver coordinates 
rec <- read.csv(file=file.path(data, "rec_coords.csv"))

## Data organization ####
## We want a dataframe with each detection by tag ID

## We need to join detection data and receiver location
## Note that 'detections' include capture encounters, receiver detections, and
## physical recaptures 

## Because receivers were placed differently among years, the data must be 
## joined by receiver of detection AND year of detection

## Format detection data
### Create rec ID by combining 'array' and 'rec'
detsum$rec.ID <- paste(detsum$array, detsum$rec, sep = '.')
### Format 'year'
detsum$year <- year(mdy_hm(detsum$ymd.hms))

### Join with 'rec'
detsum <- left_join(detsum, rec, by = c('rec.ID', 'year'))

## Format recap data
recap$rec.ID <- paste(recap$array, recap$rec, sep = '.')
recap$year <- year(mdy_hm(recap$ymd.hms))

### join with 'rec'
recap <- left_join(recap, rec, by = c('rec.ID', 'year'))

### Retain tag ID, timestamp, and coordinates as well as grouping variable '
### year'
### Bind detections with recaps
det <- rbind(detsum[,c(1,5, 9:10)], recap[,c(1:2, 7:8)])

### Join with 'tag' to get grouping variables 'sex','species', and 'stock'
det <- left_join(det, tag[, c(1, 5, 8, 28)], by = 'tag.ID')

### Now need to bind with capture data
cap <- tag[,c(1, 19, 10, 11, 5, 8, 28)]
### Only join with tags detected after capture
det <- rbind(det, subset(cap, tag.ID %in% det$tag.ID))

### We now have a row for each detection of a tagged fish, including capture
### encounter. Make sure all columns are in the format we want
str(det)
det$ymd.hms <- mdy_hm(det$ymd.hms)

## Data filtering ####
### We want to make animations of coho movement by stock, so we will remove 
### other species and any fish with no stock assignment

detstock <- det[complete.cases(det),]
detstockcoho <- filter(detstock, species == 'coho')

### Since our results from 2020 and 2021 were so similar, we just keep 2020 so
### the animation is not overwhelmed by points, also remove species column
detstockcoho20 <- detstockcoho[,c(1:5, 7)]

### And let's convert 'stock' to a friendlier label
table(detstockcoho20$stock)

detstockcoho20$stock <- ifelse(detstockcoho20$stock %in% c(4, 'N', 'S'), 'Transitory', 
       ifelse(detstockcoho20$stock == 5, 'Shaktoolik', 'Unalakleet')) 

# Interpolate detections ####

## Inriver paths ####
### We need to animate upriver paths for coho

### Let's see how many detections there were in each river
detcoho20 <- subset(detsum, tag.ID %in% detstockcoho20$tag.ID)

table(detcoho20$array)

### Now let's look at coho detection histories 
tagcoho20 <- subset(tag, species == 'coho' & year == 2020 & is.na(stock) == F)

table(tagcoho20$det.hist)

### Okay let's import the paths made on google earth
Y <- read.csv(file=file.path(data, "movement/rivers/koyuk.csv"))
I <- read.csv(file=file.path(data, "movement/rivers/inglutalik.csv"))
N <- read.csv(file=file.path(data, "movement/rivers/ungalik.csv"))
S <- read.csv(file=file.path(data, "movement/rivers/shaktoolik.csv"))
T <- read.csv(file=file.path(data, "movement/rivers/tagoomenik.csv"))
E <- read.csv(file=file.path(data, "movement/rivers/egavik.csv"))
U <- read.csv(file=file.path(data, "movement/rivers/unalakleet.csv"))
G <- read.csv(file=file.path(data, "movement/rivers/golsovia.csv"))

## Expand detection data ####
### We use detection location and timestamps to interpolate movement in a 
### straight line at a constant rate

### First separate data into lists by tag ID
det.list <- split(detstockcoho20, f = detstockcoho20$tag.ID)

### Now loop through each list and expand detection history into 'move.list'
move.list <- list() # Empty list

for(i in 1:length(det.list)){
  move.list[[i]] <- complete(det.list[[i]], 
                             ymd.hms = seq(min(ymd.hms),
                                           max(ymd.hms), by = '15 min')) %>% 
    fill(tag.ID, stock, sex) %>% 
    arrange(ymd.hms)
  move.list[[i]]$lat <- na.approx(move.list[[i]]$lat)
  move.list[[i]]$long <- na.approx(move.list[[i]]$long)
}

  # Koyuk
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(64.916964, 64.927713)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(Y)+1):(nrow(move.list[[i]]))),3] <- Y[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(Y)+1):(nrow(move.list[[i]]))),4] <- Y[,2]
  }
  # Inglutalik
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(64.837735, 64.830505)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(I)+1):(nrow(move.list[[i]]))),3] <- I[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(I)+1):(nrow(move.list[[i]]))),4] <- I[,2]
  }
  # Ungalik
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(64.557998, 64.548801)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(N)+1):(nrow(move.list[[i]]))),3] <- N[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(N)+1):(nrow(move.list[[i]]))),4] <- N[,2]
  }
  # Shaktoolik
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(64.368624, 64.366297)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(S)+1):(nrow(move.list[[i]]))),3] <- S[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(S)+1):(nrow(move.list[[i]]))),4] <- S[,2]
  }
  # Tagoomenik
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(64.318634,64.320926)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(T)+1):(nrow(move.list[[i]]))),3] <- T[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(T)+1):(nrow(move.list[[i]]))),4] <- T[,2]
  }
  # Egavik
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(64.042944, 64.039991)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(E)+1):(nrow(move.list[[i]]))),3] <- E[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(E)+1):(nrow(move.list[[i]]))),4] <- E[,2]
  }
  # Unalakleet
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(63.557764,63.555623)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(U)+1):(nrow(move.list[[i]]))),3] <- U[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(U)+1):(nrow(move.list[[i]]))),4] <- U[,2]
  }
  # Golsovia
  if(move.list[[i]][nrow(move.list[[i]]), 3] %in% c(63.860209, 63.871681)) {
    move.list[[i]][c((nrow(move.list[[i]])-nrow(G)+1):(nrow(move.list[[i]]))),3] <- G[,1] 
    move.list[[i]][c((nrow(move.list[[i]])-nrow(G)+1):(nrow(move.list[[i]]))),4] <- G[,2]
  }
}

### We now have all movement with upriver animation!!

## Multiple rivers ####

### Some fish explored other streams en route to their spawning destination 
### We need to identify fish that visited multiple streams and animate their
### inriver adventures

### We'll loop through the move.list, identify fish that visited inriver sites
### and add the river path before the detection, then we need to invert the path
### and stick it to the back

for(i in 1:length(det.list)) {
  for(j in 1:nrow(move.list[[i]])) {
  # Koyuk
    if(move.list[[i]][j, 3] %in% c(64.916964, 64.927713)) {
      move.list[[i]][c((j - nrow(Y) + 1):j),3] <- Y[,1]
      move.list[[i]][c(j:c(j + nrow(Y) - 1)),3] <- rev(Y[,1])
      move.list[[i]][c((j - nrow(Y) + 1):j),4] <- Y[,2]
      move.list[[i]][c(j:c(j + nrow(Y) - 1)),4] <- rev(Y[,2])
    }
  
  # Inglutalik
    if(move.list[[i]][j, 3] %in% c(64.837735, 64.830505)) {
      move.list[[i]][c((j - nrow(I) - 1):j),3] <- I[,1]
      move.list[[i]][c(j:c(j + nrow(I) + 1)),3] <- rev(I[,1])
      move.list[[i]][c((j - nrow(I) + 1):j),4] <- I[,2]
      move.list[[i]][c(j:c(j + nrow(I) - 1)),4] <- rev(I[,2])
    }
    
  # Ungalik
    if(move.list[[i]][j, 3] %in% c(64.557998, 64.548801)) {
      move.list[[i]][c((j - nrow(N) + 1):j),3] <- N[,1]
      move.list[[i]][c(j:c(j + nrow(N) - 1)),3] <- rev(N[,1])
      move.list[[i]][c((j - nrow(N) + 1):j),4] <- N[,2]
      move.list[[i]][c(j:c(j + nrow(N) - 1)),4] <- rev(N[,2])
  }
    
  # Shaktoolik
    if(move.list[[i]][j, 3] %in% c(64.368624, 64.366297)) {
      move.list[[i]][c((j - nrow(S) + 1):j),3] <- S[,1]
      move.list[[i]][c(j:c(j + nrow(S) - 1)),3] <- rev(S[,1])
      move.list[[i]][c((j - nrow(S) + 1):j),4] <- S[,2]
      move.list[[i]][c(j:c(j + nrow(S) - 1)),4] <- rev(S[,2])
    }
    
  # Tagoomenik
    if(move.list[[i]][j, 3] %in% c(64.318634,64.320926)) {
      move.list[[i]][c((j - nrow(T) + 1):j),3] <- T[,1]
      move.list[[i]][c(j:c(j + nrow(T) - 1)),3] <- rev(T[,1])
      move.list[[i]][c((j - nrow(T) + 1):j),4] <- T[,2]
      move.list[[i]][c(j:c(j + nrow(T) - 1)),4] <- rev(T[,2])
    }
    
  # Egavik
    if(move.list[[i]][j, 3] %in% c(64.042944, 64.039991)) {
      move.list[[i]][c((j - nrow(E) + 1):j),3] <- E[,1]
      move.list[[i]][c(j:c(j + nrow(E) - 1)),3] <- rev(E[,1])
      move.list[[i]][c((j - nrow(E) + 1):j),4] <- E[,2]
      move.list[[i]][c(j:c(j + nrow(E) - 1)),4] <- rev(E[,2])
    }
    
  # Unalakleet
    if(move.list[[i]][j, 3] %in% c(63.557764,63.555623)) {
      move.list[[i]][c((j - nrow(U) + 1):j),3] <- U[,1]
      move.list[[i]][c(j:c(j + nrow(U) - 1)),3] <- rev(U[,1])
      move.list[[i]][c((j - nrow(U) + 1):j),4] <- U[,2]
      move.list[[i]][c(j:c(j + nrow(U) - 1)),4] <- rev(U[,2])
    } 
    
  # Golsovia
    if(move.list[[i]][j, 3] %in% c(63.860209, 63.871681)) {
      move.list[[i]][c((j - nrow(G) + 1):j),3] <- G[,1]
      move.list[[i]][c(j:c(j + nrow(G) - 1)),3] <- rev(G[,1])
      move.list[[i]][c((j - nrow(G) + 1):j),4] <- G[,2]
      move.list[[i]][c(j:c(j + nrow(G) - 1)),4] <- rev(G[,2])
    }
  }
  move.list[[i]]$lat <- na.approx(move.list[[i]]$lat)
  move.list[[i]]$long <- na.approx(move.list[[i]]$long)
}

# Finalize and write dataframe ####
movement <- do.call(rbind.data.frame, move.list)
movement$ymd.hms <- as.character(movement$ymd.hms)
write.csv(movement, 'data/movement/movement_both_years2.csv', row.names = F)
