# Adding other pages of data to the selection page
# without removing communal fixation between two pages # with a second and calculated duration of events by timestamp
# rm(list = ls(all.names = TRUE)) removing all variables

setwd("~/Desktop/MSTBI/TobiiExtractedData/02-MSTBI05-RawData-csv")
library(stringr)
library(dplyr)
options(scipen = 999)

#list files
files_full <- list.files(getwd())

# relabeling part of fixations without x,y as Fi
# for (i in 1:length(files_full)) {
# # for (i in 1:2) {
#         print(i)
#         print(Sys.time())
#         z <- read.csv(files_full[i])
#         levels(z$GazeEventType) <- c(levels(z$GazeEventType), "Fi")
#         z[which(is.na(z[,"GazePointX..ADCSpx."] & is.na(z[,"GazePointY..ADCSpx."])) & z[,"GazeEventType"]=="Fixation"),"GazeEventType"] <- "Fi"
#         write.csv(z, file = paste( c(files_full[i]),"_Fi.csv", sep = "") )
#         # browser()
#         print(Sys.time())
# }

# reading csv files
# for (i in 1:1) {
#         a <- read.csv(files_full[i], header = FALSE)
# }
# names(a) <- c(list.files(getwd(), full.names = FALSE)[1])
a <- lapply(files_full, read.csv)
names(a) <- c(list.files(getwd(), full.names = FALSE))
namesA <- names(a) #copy of names(a)
Sys.time()

# plotting complete set of pupil size of a particpant
# for (i in 1) {
#         plot(a[[i]]$GazePointIndex, a[[i]]$PupilLeft)
#         points(a1[[i]]$GazePointIndex, a1[[i]][,"PupilLeft"], col="Red")
#         points(a1[[i]][which(a1[[i]]$FixationIndex>0),"GazePointIndex"], a1[[i]][which(a1[[i]]$FixationIndex>0),"PupilLeft"], col="Blue")
#         legend("bottomleft",
#                c("All pupil data","Saccades gaze points in the selection page",
#                  "Fixation gaze points in the selection page"),
#                fill=c("Black","red", "Blue")
#         )
# }


# subsetting desired columns # quick function
b <- list()
for (i in 1:length(a)) {
                b[[i]] <- a[[i]][,c(which(names(a[[i]]) %in% c("GazePointX..ADCSpx.",
                                                       "GazePointY..ADCSpx.","FixationIndex",
                                                       "PupilLeft","PupilRight",
                                                       "GazeEventDuration", "MediaName", "RecordingTimestamp",
                                                       "GazeEventType", "SaccadicAmplitude","RecordingDuration",
                                                       "SaccadeIndex","AbsoluteSaccadicDirection","EyeTrackerTimestamp",
                                                       "FixationPointX..MCSpx.","FixationPointY..MCSpx.",
                                                       "GazePointX..MCSpx.","GazePointY..MCSpx.")),which(str_detect(names(a[[i]]),"AOI")))
                         ]
}
Sys.time()

#replacing 0 with NAs in FixationIndex # quick function # FixationIndex & SaccadeIndex cannot be the same
# FixIndex=SaccadeIndex=1 would be an error; FixIndex=1 & SaccadeIndex=0 is 
for (i in 1:length(b)) {
        b[[i]]$FixationIndex <- ifelse (is.na(b[[i]]$FixationIndex), 0, b[[i]]$FixationIndex)
        b[[i]]$SaccadeIndex <- ifelse (is.na(b[[i]]$SaccadeIndex), 0, b[[i]]$SaccadeIndex)
        # b[[i]]$X0.1 <- ifelse (is.na(b[[i]]$X0.1), 0, b[[i]]$X0.1)
}


# using all points again including gaze points without GazePointX..ADCSpx. and GazePointY..ADCSpx. and probabaly pupil data
##############################
# selecting complete cases # removing gaze points without GazePointX..ADCSpx. and GazePointY..ADCSpx. and probabaly pupil data
# selecting compelete cases from X,Y,FixationIndex
# b1 <- list()
# b1 <- lapply(b, function(x) x[,c("GazePointX..ADCSpx.",
#                                  "GazePointY..ADCSpx.")])
# 
# # finding complete cases of three columns; based on that: all columns output
# c <- list()
# for (i in 1:length(b1)) {
#         c[[i]] <- b[[i]][complete.cases(b1[[i]]),]
# }
# Sys.time()
# 
# # d: matrix with details at the level of gaze data
# d <- c
# d <- lapply(d, function(x) {
#         AvePupil <- apply(x[,c("PupilLeft","PupilRight")],1,mean, na.rm= TRUE)
#         cbind(x, AvePupil)
# })
# Sys.time()
##############################

# without removing gaze points which miss GazeX, GazeY and probably pupil data #these points mostly includes unclassified, fixation at the length of 1 gaze happened in the time start/end an URL, 
Sys.time()
c <- b
for (i in 1: length(c)) { # running for about 5:30 hours 
# for (i in 1:1) {
        for (j in 1:nrow(c[[i]]))
        if (is.na(c[[i]][j,"PupilLeft"]) & is.na(c[[i]][j,"PupilRight"])) {c[[i]][j,"AvePupil"] <- NA
        } else { c[[i]][j,"AvePupil"] <- mean(c(c[[i]][j,"PupilLeft"], c[[i]][j,"PupilRight"]),na.rm = TRUE)
        }
}

d <- c

dcopy <- d
d <- dcopy

Sys.time()
for (i in 1:length(d)) { # running for about 15 hours
# for (i in 10:length(d)) {
        d[[i]][,"Page"] <- NA
        # browser()
        # levels(d[[i]][,"Page"]) <- c(-1:21)     # when reviewing the code, I never understood why I did not need this line for Test1 participants but Test2. Without this line, for Test2 participants, we only get page=-1 but the page for others would be NA.
        # browser()
        d[[i]]$MediaName <- as.character(d[[i]]$MediaName) #converting factor to character to enable selecting one element
        for (j in 1:nrow(d[[i]])) {
        # for (j in 1:5000) {
                if ((d[[i]][j,"MediaName"])=="") {d[[i]][j,"Page"] <- as.character(-1) 
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/") {d[[i]][j,"Page"] <- as.character(0)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/1" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/1") {d[[i]][j,"Page"] <- as.character(1)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/2" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/2") {d[[i]][j,"Page"] <- as.character(2)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/3" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/3") {d[[i]][j,"Page"] <- as.character(3)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/4" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/4") {d[[i]][j,"Page"] <- as.character(4)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/5" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/5") {d[[i]][j,"Page"] <- as.character(5)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/6" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/6") {d[[i]][j,"Page"] <- as.character(6)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/7" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/7") {d[[i]][j,"Page"] <- as.character(7)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/8" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/8") {d[[i]][j,"Page"] <- as.character(8)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/9" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/9") {d[[i]][j,"Page"] <- as.character(9)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/10" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/10") {d[[i]][j,"Page"] <- as.character(10)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/11" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/11") {d[[i]][j,"Page"] <- as.character(11)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/12" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/12") {d[[i]][j,"Page"] <- as.character(12)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/13" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/13") {d[[i]][j,"Page"] <- as.character(13)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/14" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/14") {d[[i]][j,"Page"] <- as.character(14)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/15" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/15") {d[[i]][j,"Page"] <- as.character(15)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/16" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/16") {d[[i]][j,"Page"] <- as.character(16)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/17" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/17") {d[[i]][j,"Page"] <- as.character(17)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/18" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/18") {d[[i]][j,"Page"] <- as.character(18)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/report" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/report") {d[[i]][j,"Page"] <- as.character(19)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/summary" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/summary") {d[[i]][j,"Page"] <- as.character(20)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/sus" | d[[i]][j,"MediaName"] == "http://tbidecisionaid.wpi.edu/sus") {d[[i]][j,"Page"] <- as.character(21)
                }
                if (j>1) {
                        d[[i]][j,"PD"] <- ((d[[i]][j,"AvePupil"])-(d[[i]][j-1,"AvePupil"]))
                        d[[i]][j,"PD%"] <- round(((d[[i]][j,"PD"])/(d[[i]][j-1,"AvePupil"])*100), digits=3)
                # we have used several methods for calculating durations, in case of using velocity again, recheck everything.
                        # d[[i]][j,"Fix_Velocity"] <- sqrt((d[[i]][j,"GazePointX..ADCSpx."]-d[[i]][j-1,"GazePointX..ADCSpx."])^2+
                        #                                      (d[[i]][j,"GazePointY..ADCSpx."]-d[[i]][j-1,"GazePointY..ADCSpx."])^2)/
                        #         (d[[i]][j,"RecordingTimestamp"]-d[[i]][j-1,"RecordingTimestamp"])
                }
        }
        d[[i]][,"Page"] <- as.factor(d[[i]][,"Page"])
        d[[i]]$IVTfac <- factor(d[[i]]$FixationIndex)
        d[[i]]$IVTSacFac <- factor(d[[i]]$SaccadeIndex)
        # d[[i]]$FIDfac <- factor(d[[i]]$"X0.1")
}
names(d) <- namesA
Sys.time()

dcopy1 <- d
d <- dcopy1

# e <- d
# fx <- data.frame(fxr=0,t1=0,t2=0,FixInd=0
#                  # , Time=0
#                  )
# Sys.time()
# for (i in 1:length(e)) { #length(g) is equal to number of participants # terribly long running: 20 hours for about 1/4 of columns
# # for (i in 1:1) { #length(g) is equal to number of participants
#         p <- split(e[[i]], e[[i]]$Page)
#         q <- as.numeric(levels(e[[i]]$Page))  # I could not select an element of a vector, so I converted it
#         for (k in 1:length(q)) {
#                 if (q[k]>=0) {
#                         z <- which(str_detect(names(p[[k]]),"AOI")) # finding AOI columns
#                         z1 <- z[which(sapply(p[[k]][,z], function(x) sum(x,na.rm = TRUE))>0)] # finding AOI columns of current page
#                         if (length(z1)>0) {
#                                 for (m in 1:length(z1)) {
#                                         fx$fxr <- fx$t1 <- fx$t2 <- fx$FixInd <- 0
#                                         for (r in 1:nrow(p[[k]])){
#                                                 fx$fxr <- r
#                                                 if ((p[[k]][r, "FixationIndex"]== 0 & (p[[k]][r,z1[m]]) %in% 0)) {
#                                                                 fx$t2 <- r
#                                                 }
#                                                 if ((p[[k]][r, "FixationIndex"])> 0 & (p[[k]][r,z1[m]]) %in% 0 & fx$FixInd > 0) {
#                                                                 fx$FixInd <- 0
#                                                 }
#                                                 if ((p[[k]][r, "FixationIndex"])> 0 & (p[[k]][r,z1[m]]) %in% 1) {
#                                                         if (fx$FixInd ==0) {
#                                                                 fx$t1 <- r
#                                                                 fx$FixInd <- p[[k]][r, "FixationIndex"]
#                                                         } else if ((p[[k]][r, "FixationIndex"]-fx$FixInd)==0) {
#                                                                 fx$t1 <- r
#                                                         } else if ((p[[k]][r, "FixationIndex"]-fx$FixInd)==1) {
#                                                                 for (s in (fx$t1+1):(fx$t2)) {
#                                                                         e[[i]][which(e[[i]]$Page==q[k]),][s,z1[m]] <- 2
#                                                                 }
#                                                                 p[[k]][(fx$t1+1):(fx$t2), z1[m]] <- 2
#                                                                 fx$t1 <- r
#                                                                 fx$FixInd <- p[[k]][r, "FixationIndex"]
#                                                         }
#                                                 }
#                                         }
#                                 }
#                         }
#                 }
#         }
# }
# ecopy <- e
# e <- ecopy
# Sys.time()

# relabeling non-fixation gazes between each two consecutive fixation on the AOI as gazes belong to AOI (2 on the AOI column for these gazes)
e <- d
fx <- data.frame(t1=0,t2=0,FixInd=0)
Sys.time()

for (i in 1:length(e)) { #length(g) is equal to number of participants # terribly long running: 20 hours for about 1/4 of columns
        # for (i in 1:1) { #length(g) is equal to number of participants
        p <- split(e[[i]], e[[i]]$Page)
        q <- as.numeric(levels(e[[i]]$Page))  # I could not select an element of a vector, so I converted it
        for (k in 1:length(q)) {
                if (q[k]>=0) {
                        z <- which(str_detect(names(p[[k]]),"AOI")) # finding AOI columns
                        z1 <- z[which(sapply(p[[k]][,z], function(x) sum(x,na.rm = TRUE))>0)] # finding AOI columns of current page
                        if (length(z1)>0) {
                                for (m in 1:length(z1)) { # go column by column of AOIs
                                        fx$t1 <- fx$t2 <- fx$FixInd <- 0
                                        x <- unique(p[[k]]$FixationIndex)[unique(p[[k]]$FixationIndex)>0] # x: fixations
                                        for  (r in 1:length(x)) {
                                                xy <- p[[k]][which(p[[k]]$FixationIndex==x[r]),]
                                                if (xy[2,z1[m]] %in% 1) {           # the first row of each fixation might be NA, so looked at the second row
                                                
                                                        if (fx$FixInd ==0) {
                                                                fx$FixInd <- xy[1,"FixationIndex"] 
                                                                fx$t1 <- xy[nrow(xy),"RecordingTimestamp"]      #timestamp of last row of the fixation on the AOI (to see if the next fixation would be still on the AOI)
                                                        } else {
                                                                fx$t2 <- xy[1,"RecordingTimestamp"]     #timestamp of first row of second consecutive fixation on the AOI
                                                                e[[i]][which(e[[i]]["RecordingTimestamp"] > fx$t1 & e[[i]]["RecordingTimestamp"] < fx$t2),z1[m]] <- 2
                                                                fx$FixInd <- xy[1,"FixationIndex"] 
                                                                fx$t1 <- xy[nrow(xy),"RecordingTimestamp"] 
                                                        }
                                                } else if (xy[2,z1[m]] %in% 0 & fx$FixInd>0) {
                                                        fx$FixInd <- 0
                                                }
                                        }
                                }
                        }
                }
        }
}

ecopy <- e
e <- ecopy
Sys.time()

###########

# g: matrix with separation at the level of fixations for each participant
# g: matrix with computed pupil parameters (summary of each fixation)
z <- which(str_detect(names(e[[1]]),"AOI")) # assuming that No of AOIs are the same among all participants.(needs to be checked)
va <- c("Page", "IVT", "Fix_Dens", # variables we want to report
        "Fix_P_Mean" ,
        "Fix_PD_Mean","Fix_PD_Perc_Mean",
        "Fix_Dur", "Fix_C_Dur",
        "Fix_Cnt_Dur")
temp <- matrix(rep(NA,length(va)+length(z)),nrow = 1) 
temp <- data.frame(temp)
name <- c(va, names(e[[1]][,which(str_detect(names(e[[1]]),"AOI"))]))
names(temp) <- name
g <- list()
length(g) <- length(e)
for (i in 1:length(g)) {
        g[[i]] <- temp
}

for (i in 1:length(g)) { #length(g) is equal to number of participants #running time:20 min
# for (i in 2:2) { 
        s <- 1          #counter for rows
        p <- split(e[[i]], e[[i]]$Page)
        q <- as.numeric(levels(e[[i]]$Page))  #I could not select an element of a vector, so I converted it
        for (k in 1:length(q)) {
                if (q[k]>=0) {
                        h <- as.character(unique(p[[k]]$IVTfac))  #p[[k]]$IVTfac provides levels that probably assigned in previous part of the code. So, I used uniques of the page and converted to character, because with as.numeric, for some reasons, it was starting from 1 instead of 0.
                        for (j in 1:length(h)) {        #number of fixations for each participant      
                                y <- p[[k]][which(p[[k]]$IVTfac==h[j]),]
                                if (h[j] > 0) {
                                        g[[i]][s,"Page"] <- q[k]
                                        z <- which(str_detect(names(y),"AOI")) 
                                        z1 <- z[which(sapply(y[,z], function(x) length(which(!is.na(x))))>0)] # finding which columns has non-NA values: showing the AOI belongs to this page
                                        for (m in 1:length(z)) {
                                                if (z[m] %in% z1) { #condition to separate AOIs on/out of the page
                                                        if (sum(y[,z[m]],na.rm = TRUE)>0) { #condition to check AOI with value of 1 (>0): showing the fixation on the AOI
                                                                g[[i]][s,length(va)+m] <- 1
                                                        } else if (sum(y[,z[m]],na.rm = TRUE)==0) { #condition to check AOI with value of 0: showing the fixation not on the AOI
                                                                g[[i]][s,length(va)+m] <- 0
                                                        }
                                                } else { #AOIs out of this page
                                                        g[[i]][s,length(va)+m] <- NA
                                                }
                                        }
                                        g[[i]][s,"IVT"] <- h[j]
                                        Area <- (max(y$"GazePointX..ADCSpx.", na.rm = TRUE) - min(y$"GazePointX..ADCSpx.", na.rm = TRUE))*(max(y$"GazePointY..ADCSpx.", na.rm = TRUE) - min(y$"GazePointY..ADCSpx.", na.rm = TRUE))
                                        if (Area==0 | Area==Inf | Area==-Inf) {          #if fixation includes only a point, it prevents generating NA and NAN.
                                                g[[i]][s,c("Fix_Dens",
                                                           # "Fix_Dens_C_Dur",
                                                       "Fix_P_Mean",
                                                       "Fix_PD_Mean","Fix_PD_Perc_Mean",
                                                       "Fix_Dur", "Fix_C_Dur", "Fix_Cnt_Dur"
                                                        # , "Fix_Velocity"
                                                        )] <- NA
                                        } else {
                                                g[[i]][s,"Fix_Dens"] <- (nrow(y)/Area)
                                                # g[[i]][s,"Fix_Dens"] <- ((y[1,"GazeEventDuration"])/Area)
                                                # g[[i]][s,"Fix_Dens_C_Dur"] <- (z/Area)
                                                g[[i]][s,"Fix_P_Mean"] <- mean(y$AvePupil, na.rm = TRUE)
                                                g[[i]][s,"Fix_PD_Mean"] <- mean(y$PD, na.rm = TRUE)
                                                g[[i]][s,"Fix_PD_Perc_Mean"] <- mean(y$"PD%", na.rm = TRUE)
                                                # g[[i]][s,"Fix_Velocity"] <- mean(y$"Fix_Velocity", na.rm = TRUE)
                                                g[[i]][s,"Fix_Dur"] <- y[1,"GazeEventDuration"]
                                                g[[i]][s,"Fix_C_Dur"] <- max(y$"RecordingTimestamp", na.rm = TRUE)-min(y$"RecordingTimestamp", na.rm = TRUE)
                                                g[[i]][s,"Fix_Cnt_Dur"] <- nrow(y)*3.333
                                        }
                                        s <- s+1
                                }
                        }
                }
        }
}
names(g) <- namesA
Sys.time()

gcopy <- g
g <- gcopy

# gs: g for saccade
# gs: matrix with separation at the level of saccades for each participant
# gs: matrix with computed pupil parameters (summary of each saccade)
z <- which(str_detect(names(e[[1]]),"AOI")) # assuming that No of AOIs are the same among all participants.(needs to be checked)
vas <- c("Page", "IVTSac", "Sac_Dens",
        # "Sac_Dens_C_Dur" = NA,
        "Sac_P_Mean",
        "Sac_PD_Mean", "Sac_PD_Perc_Mean",
        "Sac_Dur", "Sac_C_Dur", "Sac_Cnt_Dur"
        # , "Sac_Velocity"=NA
        )
temps <- matrix(rep(NA,length(vas)+length(z)),nrow = 1)
temps <- data.frame(temps)
namess <- c(vas, names(e[[1]][,which(str_detect(names(e[[1]]),"AOI"))]))
names(temps) <- namess
gs <- list()
length(gs) <- length(e)
for (i in 1:length(gs)) {
        gs[[i]] <- temps
}

Sys.time()
for (i in 1:length(gs)) { #length(gs) is equal to number of participants # running time 30 min
# for (i in 1:2) { #length(gs) is equal to number of participants
        s <- 1          #counter for rows
        ps <- split(e[[i]], e[[i]]$Page)
        qs <- as.numeric(levels(e[[i]]$Page))  #I could not select an element of a vector, so I converted it
        for (k in 1:length(qs)) {
                # browser()
                if (qs[k]>=0) {
                        hs <- as.character(unique(ps[[k]]$IVTSacFac))  #ps[[k]]$IVTSacFac provides levels that probably assigned in previous part of the code. So, I used uniques of the page and converted to character, because with as.numeric, for some reasons, it was starting from 1 instead of 0.
                        for (j in 1:length(hs)) {        #number of fixations for each participant      
                                ys <- ps[[k]][which(ps[[k]]$IVTSacFac==hs[j]),]
                                if (hs[j] > 0) {
                                        gs[[i]][s,"Page"] <- qs[k]
                                        z <- which(str_detect(names(ys),"AOI")) 
                                        # z1 <- z[which(sapply(ys[,z], function(x) sum(x,na.rm = TRUE))>0)]
                                        z1 <- z[which(sapply(ys[,z], function(x) length(which(!is.na(x))))>0)] # finding which columns has non-NA values: showing the AOI belongs to this page
                                        # for (m in 1:length(z)) {
                                        #         if (z[m] %in% z1) {
                                        #                 gs[[i]][s,length(vas)+m] <- 1      
                                        #         } else {
                                        #                 gs[[i]][s,length(vas)+m] <- 0
                                        #         }
                                        # }
                                        for (m in 1:length(z)) {
                                                if (z[m] %in% z1) { #condition to separate AOIs on/out of the page
                                                        if (sum(ys[,z[m]],na.rm = TRUE)>0) { #condition to check AOI with value of 1 (>0): showing the saccade on the AOI
                                                                gs[[i]][s,length(vas)+m] <- 1
                                                        } else if (sum(ys[,z[m]],na.rm = TRUE)==0) { #condition to check AOI with value of 0: showing the saccade not on the AOI
                                                                gs[[i]][s,length(vas)+m] <- 0
                                                        }
                                                } else { #AOIs out of this page
                                                        gs[[i]][s,length(vas)+m] <- NA
                                                }
                                        }
                                        # browser()
                                        gs[[i]][s,"IVTSac"] <- hs[j]
                                        Area <- (max(ys$"GazePointX..ADCSpx.", na.rm = TRUE) - min(ys$"GazePointX..ADCSpx.", na.rm = TRUE))*(max(ys$"GazePointY..ADCSpx.", na.rm = TRUE) - min(ys$"GazePointY..ADCSpx.", na.rm = TRUE))
                                        if (Area==0 | Area==Inf | Area==-Inf) {          #if fixation includes only a point, it prevents generating NA and NAN.
                                                gs[[i]][s,c("Sac_Dens",
                                                            # "Sac_Dens_C_Dur",
                                                            "Sac_P_Mean",
                                                            "Sac_PD_Mean","Sac_PD_Perc_Mean",
                                                            "Sac_Dur", "Sac_C_Dur", "Sac_Cnt_Dur"
                                                            # , "Sac_Velocity"=NA
                                                        )] <- NA
                                        } else {
                                                gs[[i]][s,"Sac_Dens"] <- (nrow(ys)/Area)
                                                # gs[[i]][s,"Sac_Dens_C_Dur"] <- (z/Area)
                                                gs[[i]][s,"Sac_P_Mean"] <- mean(ys$AvePupil, na.rm = TRUE)
                                                gs[[i]][s,"Sac_PD_Mean"] <- mean(ys$PD, na.rm = TRUE)
                                                gs[[i]][s,"Sac_PD_Perc_Mean"] <- mean(ys$"PD%", na.rm = TRUE)
                                                # gs[[i]][s,"Sac_Velocity"] <- mean(ys$"Sac_Velocity", na.rm = TRUE)
                                                gs[[i]][s,"Sac_Dur"] <- ys[1,"GazeEventDuration"]
                                                gs[[i]][s,"Sac_C_Dur"] <- max(ys$"RecordingTimestamp", na.rm = TRUE)-min(ys$"RecordingTimestamp", na.rm = TRUE)
                                                gs[[i]][s,"Sac_Cnt_Dur"] <- nrow(ys)*3.333
                                        }
                                        # gs[[i]][s,"Page"] <- qs[k]
                                        s <- s+1
                                }
                        }
                }
        }
}
names(gs) <- namesA
Sys.time()

gscopy <- gs
gs <- gscopy

# gAOI and gsAOI: matrix of means of columns (means of data of all fixations of each participant for each AOI on each page)
# Summarize the fixation data for each AOI
zz <- which(str_detect(names(g[[1]]),"AOI"))
tempAOI <- data.frame("Rec"=NA, "Page"=NA, "AOI"=NA, 
                      "Fix_Dens_M"=NA, "Fix_Dens_SD"=NA,
                      "Fix_P_M"=NA, "Fix_P_SD"=NA, 
                      "Fix_PD_M"=NA, "Fix_PD_SD"=NA,
                      "Fix_PD_Perc_M"=NA, "Fix_PD_Perc_SD"=NA,
                      "Fix_Dur_M"=NA, "Fix_Dur_SD"=NA,
                      "Fix_C_Dur_M"=NA, "Fix_C_Dur_SD"=NA,
                      "Fix_Cnt_Dur_M"=NA, "Fix_Cnt_Dur_SD"=NA
                      )
tempAOI <- tempAOI[-1,]
gAOI <- list()
length(gAOI) <- length(g)
for (i in 1:length(g)) {
        gAOI[[i]] <- tempAOI
}
Sys.time()

####
# needed:  renaming AOIs
####
for (i in 1:length(g)) {
# for (i in 2:2) {
        s <- 1
        print(i)
        for (j in 1:length(zz)) {
                # print(zz[j])
                # browser() #checking AOI.WholePage.Hit.2 on page 2
                if (length(which(g[[i]][zz[j]]==0))>0) {
                        
                        if (length(which(g[[i]][zz[j]]==1))>0) {
                                y <- g[[i]][which(g[[i]][zz[j]]==1),1:length(va)]
                                o <- 0
                                # browser()
                                for (k in 3:length(y)) {
                                        # browser()
                                        
                                        # below two lines may move out of the for loop # even page and AOI assignment may move out of if loop
                                        gAOI[[i]][s,"Rec"] <- substr(names(g)[i],start=16, stop=21)
                                        gAOI[[i]][s,"Page"] <- y[1,"Page"]
                                        gAOI[[i]][s,"AOI"] <- names(g[[i]])[zz[j]]
                                        gAOI[[i]][s,k+1+o] <- mean(y[,k],na.rm = TRUE) #k+1: because the number of columns in gAOI is one more than g (added Rec)
                                        gAOI[[i]][s,k+1+o+1] <- sd(y[,k],na.rm = TRUE) #o & o+1: columns for mean & sd of each variable
                                        o <- o+1
                                }
                        } else {
                                y <- g[[i]][which(g[[i]][zz[j]]==0),1:length(va)]
                                gAOI[[i]][s,"Rec"] <- substr(names(g)[i],start=16, stop=21)
                                gAOI[[i]][s,"Page"] <- y[1,"Page"]
                                gAOI[[i]][s,"AOI"] <- names(g[[i]])[zz[j]]
                                o <- 0
                                for (k in 3:length(y)) {
                                        gAOI[[i]][s,k+1+o] <- NA
                                        gAOI[[i]][s,k+1+o+1] <- NA
                                        o <- o+1
                                }
                        }
                s <- s+1
                # browser()
                }
        }
}
names(gAOI) <- names(g)
Sys.time()
gAOIcopy <- gAOI
gAOI<- gAOIcopy
# test should be added. renaming AOIs should be done.
AOI <- do.call(rbind,gAOI)
rownames(AOI) <- c()


# for (i in 1:length(a)) {
#         print(names(a)[i])
#         print(length(a[[i]]))
# 
# }

write.csv(AOI, file = "AOI-Fix-report.csv")


# Summarize the saccade data for each AOI
zzs <- which(str_detect(names(gs[[1]]),"AOI"))
tempsAOI <- data.frame("Rec"=NA, "Page", "AOI"=NA,
                       "Sac_Dens_M"=NA, "Sac_Dens_SD"=NA,
                       "Sac_P_M"=NA, "Sac_P_SD"=NA,
                       "Sac_PD_M"=NA, "Sac_PD_SD"=NA,
                       "Sac_PD_Perc_M"=NA, "Sac_PD_Perc_SD"=NA,
                       "Sac_Dur_M"=NA, "Sac_Dur_SD"=NA,
                       "Sac_C_Dur_M"=NA, "Sac_C_Dur_SD"=NA,
                       "Sac_Cnt_Dur_M"=NA, "Sac_Cnt_Dur_SD"=NA
)
tempsAOI <- tempsAOI[-1,]
gsAOI <- list()
length(gsAOI) <- length(gs)
for (i in 1:length(gs)) {
        gsAOI[[i]] <- tempsAOI
}

####
# needed:  renaming AOIs

####
for (i in 1:length(gs)) {
        s <- 1
        for (j in 1:length(zzs)) {
                if (length(which(gs[[i]][zzs[j]]==1))>0) {
                        y <- gs[[i]][which(gs[[i]][zzs[j]]==1),1:length(vas)]
                        o <- 0
                        for (k in 3:length(y)) {
                                # browser()
                                gsAOI[[i]][s,"Page"] <- y[1,"Page"]
                                gsAOI[[i]][s,"AOI"] <- names(gs[[i]])[zzs[j]]
                                gsAOI[[i]][s,k+1+o] <- mean(y[,k],na.rm = TRUE)
                                gsAOI[[i]][s,k+1+o+1] <- sd(y[,k],na.rm = TRUE)
                                o <- o+1
                        }
                } else {
                        gsAOI[[i]][s,"Page"] <- y[1,"Page"]
                        gsAOI[[i]][s,"AOI"] <- names(gs[[i]])[zzs[j]]
                        o <- 0
                        for (k in 3:length(y)) {
                                gsAOI[[i]][s,k+1+o] <- NA
                                gsAOI[[i]][s,k+1+o+1] <- NA
                                o <- o+1
                        }
                }
                gsAOI[[i]][s,"Sac_Dens_SD"] <- NA #overriding the Sac_Dens_SD calculations because this is meaningless, but to keep same structure of fixation and saccade outputs, we kept this column
                s <- s+1
                # browser()
        }
}

Sys.time()
gsAOIcopy <- gsAOI
gsAOI<- gsAOIcopy

#######
library(dplyr)
# FixPage and SacPage: matrix of means of columns (means of data of all fixations of each participant for each page)
# summary of each paricipant

gPage <- list()
gPage <- lapply(g, function(x) summarise(x %>% group_by(Page),
                                        Fix_Dens_M= mean(Fix_Dens, na.rm = TRUE),
                                        Fix_Dens_SD= sd(Fix_Dens, na.rm = TRUE),
                                        # Fix_Dens_C_Dur= mean(Fix_Dens_C_Dur, na.rm = TRUE),
                                        Fix_P_M= mean(Fix_P_Mean, na.rm = TRUE),
                                        Fix_P_SD= sd(Fix_P_Mean, na.rm = TRUE), 
                                        Fix_PD_M= mean(Fix_PD_Mean, na.rm = TRUE),
                                        Fix_PD_SD= sd(Fix_PD_Mean, na.rm = TRUE),
                                        Fix_PD_Perc_M= mean(Fix_PD_Perc_Mean, na.rm = TRUE), 
                                        Fix_PD_Perc_SD= sd(Fix_PD_Perc_Mean, na.rm = TRUE),
                                        # Fix_Velocity_M= mean(Velocity, na.rm = TRUE),
                                        # Fix_Velocity_SD= sd(Velocity, na.rm = TRUE),
                                        Fix_Dur_M= mean(Fix_Dur, na.rm = TRUE),
                                        Fix_Dur_SD= sd(Fix_Dur, na.rm = TRUE),
                                        Fix_C_Dur_M= mean(Fix_C_Dur, na.rm = TRUE),
                                        Fix_C_Dur_SD= sd(Fix_C_Dur, na.rm = TRUE),
                                        Fix_Cnt_Dur_M= mean(Fix_Cnt_Dur, na.rm = TRUE),
                                        Fix_Cnt_Dur_SD= sd(Fix_Cnt_Dur, na.rm = TRUE)))
FixPage <- do.call(rbind,gPage)
FixPageCopy <- FixPage

Sys.time()

# gsPage: corresponding gPage for Saccades
gsPage <- list()
gsPage <- lapply(gs, function(x) summarise(x %>% group_by(Page), 
                                        Sac_Dens_M= mean(Sac_Dens, na.rm = TRUE),
                                        Sac_Dens_SD= NA,
                                        # Sac_Dens_C_Dur= mean(Sac_Dens_C_Dur, na.rm = TRUE),
                                        Sac_P_M= mean(Sac_P_Mean, na.rm = TRUE),
                                        Sac_P_SD= sd(Sac_P_Mean, na.rm = TRUE), 
                                        Sac_PD_M= mean(Sac_PD_Mean, na.rm = TRUE),
                                        Sac_PD_SD= sd(Sac_PD_Mean, na.rm = TRUE),
                                        Sac_PD_Perc_M= mean(Sac_PD_Perc_Mean, na.rm = TRUE), 
                                        Sac_PD_Perc_SD= sd(Sac_PD_Perc_Mean, na.rm = TRUE),
                                        # Sac_Velocity_M= mean(Sac_Velocity, na.rm = TRUE),
                                        # Sac_Velocity_SD= sd(Sac_Velocity, na.rm = TRUE),
                                        Sac_Dur_M= mean(Sac_Dur, na.rm = TRUE),
                                        Sac_Dur_SD= sd(Sac_Dur, na.rm = TRUE),
                                        Sac_C_Dur_M= mean(Sac_C_Dur, na.rm = TRUE),
                                        Sac_C_Dur_SD= sd(Sac_C_Dur, na.rm = TRUE),
                                        Sac_Cnt_Dur_M= mean(Sac_Cnt_Dur, na.rm = TRUE),
                                        Sac_Cnt_Dur_SD= sd(Sac_Cnt_Dur, na.rm = TRUE)))
SacPage <- do.call(rbind,gsPage)
Sys.time()

options(scipen = 999) # converts scientific numbers
# AllPage is aggregation of fixation and sacccade data at the page level
AllPage <- cbind(FixPage, SacPage)
# Separating Participant, Test and Rec and removing row.names   # the code has written in a way that handles Recording number with 3 digits
AllPage[,"Participant"] <- substr(row.names(AllPage),start=10, stop=21)
AllPage[,"Test"] <- substr(AllPage$Participant,start=1, stop=5)
AllPage[,"Rec"] <- substr(AllPage$Participant,start=11, stop=12)
AllPage <- AllPage[,c(31,32,33,1:30)]
rownames(AllPage) <- c()
# Removing second column of page number
AllPage <- subset(AllPage, select = -c(Page.1) )


AllPageCopy <- AllPage
AllPage <- AllPageCopy

library(stringr)
# Adding NoFixation, NoSaccade, and RecDuration for each page # takes a few minutes to run
for (i in 1:length(d)) { #length(d) is equal to number of participants
        p <- split(d[[i]], d[[i]]$Page)
        q <- as.numeric(levels(d[[i]]$Page))  #I could not select an element of a vector, so I converted it
        for (k in 1:length(q)) {
                if (q[k]>=0) {
                        AllPage[which(p[[k]]$Page[1] == AllPage$Page & str_detect(names(d[i]), AllPage$Participant) ),"NoFixation"] <- length(unique(p[[k]][which(p[[k]]["FixationIndex"]>0),"FixationIndex"]))
                        AllPage[which(p[[k]]$Page[1] == AllPage$Page & str_detect(names(d[i]), AllPage$Participant) ),"NoSaccade"] <- length(unique(p[[k]][which(p[[k]]["SaccadeIndex"]>0),"SaccadeIndex"]))
                        AllPage[which(p[[k]]$Page[1] == AllPage$Page & str_detect(names(d[i]), AllPage$Participant) ),"PageDuration"] <- (p[[k]][max(which(p[[k]]["RecordingTimestamp"]>0)),"RecordingTimestamp"])-(p[[k]][min(which(p[[k]]["RecordingTimestamp"]>0)),"RecordingTimestamp"])
                        # PageCntDuration is a better variable for duration since it does not go wrong like PageDuration, which is calculated based on timestamp, when go back and forth between pages
                        AllPage[which(p[[k]]$Page[1] == AllPage$Page & str_detect(names(d[i]), AllPage$Participant) ),"PageCntDuration"] <- nrow(p[[k]][which(!is.na(p[[k]]$EyeTrackerTimestamp)),])*3.333
                        # AllPage[which(p[[k]]$Page[1] == AllPage$Page & str_detect(names(d[i]), AllPage$Participant) ),"Diff"] <- AllPage[which(p[[k]]$Page[1] == AllPage$Page & str_detect(names(d[i]), AllPage$Participant)),"PageDuration"]- AllPage[which(p[[k]]$Page[1] == AllPage$Page & str_detect(names(d[i]), AllPage$Participant)),"PageCntDuration"]
                        AllPage[which(p[[k]]$Page[1] == AllPage$Page & str_detect(names(d[i]), AllPage$Participant) ),"Uncls_Dur_Cnt_Sum"] <- length(which(p[[k]]$GazeEventType == "Unclassified"))*3.333
                }
        }
}
Sys.time()

# redoing page level calculation for all data points of each participant
# FixTotal and SacTotal: matrix of means of columns (means of data of all fixations of each participant with accumulation of pages)
# summary of each paricipant
gTotal <- list()
gTotal <- lapply(g, function(x) summarise(x,
                                         Fix_Dens_M= mean(Fix_Dens, na.rm = TRUE),
                                         Fix_Dens_SD= sd(Fix_Dens, na.rm = TRUE),
                                         # Fix_Dens_C_Dur= mean(Fix_Dens_C_Dur, na.rm = TRUE),
                                         Fix_P_M= mean(Fix_P_Mean, na.rm = TRUE),
                                         Fix_P_SD= sd(Fix_P_Mean, na.rm = TRUE), 
                                         Fix_PD_M= mean(Fix_PD_Mean, na.rm = TRUE),
                                         Fix_PD_SD= sd(Fix_PD_Mean, na.rm = TRUE),
                                         Fix_PD_Perc_M= mean(Fix_PD_Perc_Mean, na.rm = TRUE), 
                                         Fix_PD_Perc_SD= sd(Fix_PD_Perc_Mean, na.rm = TRUE),
                                         # Fix_Velocity_M= mean(Velocity, na.rm = TRUE),
                                         # Fix_Velocity_SD= sd(Velocity, na.rm = TRUE),
                                         Fix_Dur_M= mean(Fix_Dur, na.rm = TRUE),
                                         Fix_Dur_SD= sd(Fix_Dur, na.rm = TRUE),
                                         Fix_C_Dur_M= mean(Fix_C_Dur, na.rm = TRUE),
                                         Fix_C_Dur_SD= sd(Fix_C_Dur, na.rm = TRUE),
                                         Fix_Cnt_Dur_M= mean(Fix_Cnt_Dur, na.rm = TRUE),
                                         Fix_Cnt_Dur_SD= sd(Fix_Cnt_Dur, na.rm = TRUE)))
FixTotal <- do.call(rbind,gTotal)

# gsTotal: corresponding gTotal for Saccades
gsTotal <- list()
gsTotal <- lapply(gs, function(x) summarise(x, 
                                           Sac_Dens_M= mean(Sac_Dens, na.rm = TRUE),
                                           Sac_Dens_SD= NA,
                                           # Sac_Dens_C_Dur= mean(Sac_Dens_C_Dur, na.rm = TRUE),
                                           Sac_P_M= mean(Sac_P_Mean, na.rm = TRUE),
                                           Sac_P_SD= sd(Sac_P_Mean, na.rm = TRUE), 
                                           Sac_PD_M= mean(Sac_PD_Mean, na.rm = TRUE),
                                           Sac_PD_SD= sd(Sac_PD_Mean, na.rm = TRUE),
                                           Sac_PD_Perc_M= mean(Sac_PD_Perc_Mean, na.rm = TRUE), 
                                           Sac_PD_Perc_SD= sd(Sac_PD_Perc_Mean, na.rm = TRUE),
                                           # Sac_Velocity_M= mean(Sac_Velocity, na.rm = TRUE),
                                           # Sac_Velocity_SD= sd(Sac_Velocity, na.rm = TRUE),
                                           Sac_Dur_M= mean(Sac_Dur, na.rm = TRUE),
                                           Sac_Dur_SD= sd(Sac_Dur, na.rm = TRUE),
                                           Sac_C_Dur_M= mean(Sac_C_Dur, na.rm = TRUE),
                                           Sac_C_Dur_SD= sd(Sac_C_Dur, na.rm = TRUE),
                                           Sac_Cnt_Dur_M= mean(Sac_Cnt_Dur, na.rm = TRUE),
                                           Sac_Cnt_Dur_SD= sd(Sac_Cnt_Dur, na.rm = TRUE)))
SacTotal <- do.call(rbind,gsTotal)

# AllPage is aggregation of fixation and sacccade data at the page level
AllTotal <- cbind(FixTotal, SacTotal)
# separating Participant, Test and Rec and removing row.names   # the code has written in a way that handles Recording number with 3 digits
AllTotal[,"Participant"] <- substr(row.names(AllTotal),start=10, stop=21)
AllTotal[,"Test"] <- substr(AllTotal$Participant,start=1, stop=5)
AllTotal[,"Rec"] <- substr(AllTotal$Participant,start=11, stop=12)
AllTotal <- AllTotal[,c(29,30,31,1:28)]
rownames(AllTotal) <- c()


# Adding NoFixation, NoSaccade, and RecDuration to each participant's data
for (i in 1:length(g)) { #length(g) is equal to number of participants
# for (i in 1:2) { #length(g) is equal to number of participants
        AllTotal[i,"NoFixation"] <- sum(AllPage[which((AllPage$Participant==AllTotal$Participant[i])),"NoFixation"])
        AllTotal[i,"NoSaccade"] <- sum(AllPage[which((AllPage$Participant==AllTotal$Participant[i])),"NoSaccade"])
        AllTotal[i,"RecDuration"] <- sum(AllPage[which((AllPage$Participant==AllTotal$Participant[i])),"PageDuration"])
        # RecCntDuration is a more accurate variables since it does not go wrong with going back and forth between pages
        AllTotal[i,"RecCntDuration"] <- sum(AllPage[which((AllPage$Participant==AllTotal$Participant[i])),"PageCntDuration"])
        AllTotal[i,"Uncls_CntDuration"] <- sum(AllPage[which((AllPage$Participant==AllTotal$Participant[i])),"Uncls_Dur_Cnt_Sum"])
}
Sys.time()


# for (i in 1:nrow(AllPage)) {
#         if (AllPage$Page[i]==19) {
#                 AllPage$Page[i] <- "report"        
#         } else if (AllPage$Page[i]==20) {
#                 AllPage$Page[i] <- "summary" 
#         } else if (AllPage$Page[i]==21) {
#                 AllPage$Page[i] <- "sus" 
#         }
# }
# library(data.table)
# AllPage2 <- AllPage2[order(AllPage2$Test,AllPage2$Rec),]
# AllPage2 <- AllPage2[order(AllPage2$Rec=c(1:18,"sus","report","summary")),]
# z <- c(1:18,"sus","report","summary")
# AllPage2 <- AllPage2 %>% arrange(factor(Rec, levels = z))
# AllPage2 <- arrange(AllPage2,(Rec))
# AllPage2[with(AllPage2, order(AllPage2$Test, "Rec")),]
# AllPage2[order(AllPage2$Test, my_data2$group2), ] 
# data_ordered <- data                                 # Replicate example data
# setorder(AllPage2, z)                       # Order data with data.table
# AllPage2 
# 
# AllPage2$Rec <- ordered(AllPage2$Rec, z)
# z <- AllPage2[with(AllPage2, order(Rec, Test)),]

write.csv(AllPage, file = "AllPage.csv")
write.csv(AllTotal, file = "AllTotal.csv")

# the number of pages should be converted to a variable not constant
# ttest <- matrix(ncol=length(colnames(AllPage)[4:35]))
# colnames(ttest) <- colnames(AllPage)[4:35]

# Running t-test at the page level
ttestP <- AllPage
ttestP <- ttestP[-c(2:nrow(ttestP)),-c(1:3)]
ttestP[1,] <- NA
s <- 1
for (i in 1:length(unique(AllPage$Page))) {
        for (j in 2:ncol(ttestP)) {
        # for (j in 2:5) {
                z <- t.test(AllPage[which(AllPage$Test=="Test1" & AllPage$Page==i-1),colnames(ttestP)[j]],AllPage[which(AllPage$Test=="Test2" & AllPage$Page==i-1),colnames(ttestP)[j]])
                ttestP[s,"Page"] <- AllPage$Page[i]
                ttestP[s,colnames(ttestP)[j]] <- z$p.value
        }
        s <- s+1
}
write.csv(ttestP, file = "ttestP.csv")

# Running t-test at the participant level
ttestT <- AllTotal
ttestT <- ttestT[-c(2:nrow(ttestT)),-c(1:3)]
ttestT[1,] <- NA
s <- 1
for (i in 1:1) {
        for (j in 1:ncol(ttestT)) {
                z <- t.test(AllTotal[which(AllTotal$Test=="Test1"),colnames(ttestT)[j]],AllTotal[which(AllTotal$Test=="Test2"),colnames(ttestT)[j]])
                ttestT[s,colnames(ttestT)[j]] <- z$p.value
        }
        s <- s+1
}
write.csv(ttestT, file = "ttestT.csv")


remove(fx,p,ps,temp,tempAOI,tempsAOI,x2,x3,xy,xyz,y,ys,z)

# finding a text
# strsplit(rownames(m2),"_")
# substr(row.names(AllPage),start=1, stop=12)
