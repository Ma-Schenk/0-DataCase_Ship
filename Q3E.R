AgeCat <- seq(from=1, to=881, by=1) #create a sequence to be replaced in the loop
ship1 <- mutate(ship1, AgeCat) 
# add an age category for the vessel ship
for(i in 1:nrow(ship1)){
  va <- ship1[i,3]
  if(va<=5){
    ship1[i, 7] <- 1
  }else if(va>5 & va<=10){
    ship1[i, 7] <- 2
  }else if(va>10 & va<=15){
    ship1[i, 7] <- 3
  }else if(va>15 & va<=20){
    ship1[i, 7] <- 4
  }else{
    ship1[i, 7] <- 5
  }
}

# create a list of regression with moving time windows, taking only 30 observations at once 
# note these are the last 30 days before the index considered, so it starts at the 30th sale
reglist <- list()
k <- 1
for(i in 30:nrow(ship1)){
  up_index <- i 
  low_index <- i-30+1
  ship1_rest <- ship1[low_index:up_index,] #restrict the dataset of interest to 30 observations
  modtmp <- lm(ship1_rest$SellingPrice~ship1_rest$VesselAge+ship1_rest$Dwt+ship1_rest$Freight)
  # train the model on the restricted dataset 
  intcpttmp <- as.numeric(modtmp$coefficients[1])
  vatmp <- as.numeric(modtmp$coefficients[2])
  dwttmp <- as.numeric(modtmp$coefficients[3])
  bsitmp <- as.numeric(modtmp$coefficients[4])
  rsqtmp <- as.numeric(summary(modtmp)$r.squared)
  sptmp <- ship1_rest[30, 2]
  estsptmp <- intcpttmp+vatmp*ship1_rest[30,3]+dwttmp*ship1_rest[30,4]+bsitmp*ship1_rest[30,5]
  errpcttmp <- (abs(estsptmp-sptmp)/sptmp)*100
  errtmp <- abs(estsptmp-sptmp)
  agetmp <- ship1_rest[30, 7]
  
  vectmp <- c(intcpttmp,vatmp,dwttmp,bsitmp,rsqtmp,sptmp,estsptmp,errpcttmp,errtmp,agetmp)
  
  reglist[[k]] <- vectmp
  
  k <- k+1
}

# average relative error and sd (overall)
relerr <- c()
for(i in 1:length(reglist)){
  relerr <- c(relerr, reglist[[i]][8])
}
mean(relerr)
sd(relerr)

# average error and sd (overall)
err <- c()
for(i in 1:length(reglist)){
  err <- c(err, reglist[[i]][9])
}
mean(err)
sd(err)

# average relative error by age category
relerr1 <- c()
relerr2 <- c()
relerr3 <- c()
relerr4 <- c()
relerr5 <- c()

for(i in 1:length(reglist)){
  if(reglist[[i]][10]==1){
    relerr1 <- c(relerr1, reglist[[i]][8])
  }else if(reglist[[i]][10]==2){
    relerr2 <- c(relerr2, reglist[[i]][8])
  }else if(reglist[[i]][10]==3){
    relerr3 <- c(relerr3, reglist[[i]][8])
  }else if(reglist[[i]][10]==4){
    relerr4 <- c(relerr4, reglist[[i]][8])
  }else if(reglist[[i]][10]==5){
    relerr5 <- c(relerr5, reglist[[i]][8])
  }
}

mean(relerr1) #for category 1: <=5 years old
mean(relerr2) #for category 2: 6 to 10 years old
mean(relerr3) #for category 3: 11 to 15 years old 
mean(relerr4) #for category 4: 16 to 20 years old
mean(relerr5) #for category 5: >20 years old

# average error by age category
err1 <- c()
err2 <- c()
err3 <- c()
err4 <- c()
err5 <- c()

for(i in 1:length(reglist)){
  if(reglist[[i]][10]==1){
    err1 <- c(err1, reglist[[i]][9])
  }else if(reglist[[i]][10]==2){
    err2 <- c(err2, reglist[[i]][9])
  }else if(reglist[[i]][10]==3){
    err3 <- c(err3, reglist[[i]][9])
  }else if(reglist[[i]][10]==4){
    err4 <- c(err4, reglist[[i]][9])
  }else if(reglist[[i]][10]==5){
    err5 <- c(err5, reglist[[i]][9])
  }
}
mean(err1)
mean(err2)
mean(err3)
mean(err4)
mean(err5)

# By category, number of vessels within 5% and 10% range error
cat1.5pc <- 0
cat2.5pc <- 0
cat3.5pc <- 0
cat4.5pc <- 0
cat5.5pc <- 0
cat1.10pc <- 0
cat2.10pc <- 0
cat3.10pc <- 0
cat4.10pc <- 0
cat5.10pc <- 0

for(i in 1:length(reglist)){
  if(reglist[[i]][10]==1){
    if(reglist[[i]][8]<=5.00){
      cat1.5pc <- cat1.5pc + 1
    }
    if(reglist[[i]][8]<=10.00){
      cat1.10pc <- cat1.10pc + 1
    }
  }else if(reglist[[i]][10]==2){
    if(reglist[[i]][8]<=5.00){
      cat2.5pc <- cat2.5pc + 1
    }
    if(reglist[[i]][8]<=10.00){
      cat2.10pc <- cat2.10pc + 1
    }
  }else if(reglist[[i]][10]==3){
    if(reglist[[i]][8]<=5.00){
      cat3.5pc <- cat3.5pc + 1
    }
    if(reglist[[i]][8]<=10.00){
      cat3.10pc <- cat3.10pc + 1
    }
  }else if(reglist[[i]][10]==4){
    if(reglist[[i]][8]<=5.00){
      cat4.5pc <- cat4.5pc + 1
    }
    if(reglist[[i]][8]<=10.00){
      cat4.10pc <- cat4.10pc + 1
    }
  }else if(reglist[[i]][10]==5){
    if(reglist[[i]][8]<=5.00){
      cat5.5pc <- cat5.5pc + 1
    }
    if(reglist[[i]][8]<=10.00){
      cat5.10pc <- cat5.10pc + 1
    }
  }
}

cat1.5pc/nrow(ship1[ship1[,"AgeCat"]==1,]) #percentage of vessels in AgeCat 1 within 5% error
cat1.10pc/nrow(ship1[ship1[,"AgeCat"]==1,]) #percentage of vessels in AgeCat 1 within 10% error

cat2.5pc/nrow(ship1[ship1[,"AgeCat"]==2,]) #percentage of vessels in AgeCat 2 within 5% error
cat2.10pc/nrow(ship1[ship1[,"AgeCat"]==2,]) #percentage of vessels in AgeCat 2 within 10% error

cat3.5pc/nrow(ship1[ship1[,"AgeCat"]==3,]) #percentage of vessels in AgeCat 2 within 5% error
cat3.10pc/nrow(ship1[ship1[,"AgeCat"]==3,]) #percentage of vessels in AgeCat 2 within 10% error

cat4.5pc/nrow(ship1[ship1[,"AgeCat"]==4,]) #percentage of vessels in AgeCat 2 within 5% error
cat4.10pc/nrow(ship1[ship1[,"AgeCat"]==4,]) #percentage of vessels in AgeCat 2 within 10% error

cat5.5pc/nrow(ship1[ship1[,"AgeCat"]==5,]) #percentage of vessels in AgeCat 2 within 5% error
cat5.10pc/nrow(ship1[ship1[,"AgeCat"]==5,]) #percentage of vessels in AgeCat 2 within 10% error