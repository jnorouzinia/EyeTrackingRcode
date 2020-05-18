# with removing communal fixation between two pages #This file would be a backup for the code even though we decided not to remove communal fixations and change the code
# rm(list = ls(all.names = TRUE))
# Adding other pages of data to the selection page

setwd("~/Desktop/MSTBI/TobiiExtractedData/output")
library(stringr)
library(dplyr)

#list files
files_full <- list.files(getwd())

#reading csv files
a <- lapply(files_full, read.csv)
names(a) <- c(list.files(getwd(), full.names = FALSE))
namesA <- names(a) #copy of names(a)

# # do not need for subsetting desired rows (selecting selection page only) anymore
# a1 <- list()
# for (i in 1:length(a)) {
#         a1[[i]] <- a[[i]][grep("selection.php",a[[i]]$MediaName),]
# }

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

###

# subsetting desired columns
b <- list()
for (i in 1:length(a)) {
        b[[i]] <- a[[i]][,c("GazePointX..ADCSpx.",
                             "GazePointY..ADCSpx.","FixationIndex",
                             # "X0.1","X0.1_saccade_fid",
                             "PupilLeft","PupilRight",
                             "GazeEventDuration", "MediaName", "RecordingTimestamp",
                             "GazeEventType", "SaccadicAmplitude","RecordingDuration",
                             "SaccadeIndex","AbsoluteSaccadicDirection"
        )]
}

#replacing 0 with NAs in FixationIndex
for (i in 1:length(b)) {
        b[[i]]$FixationIndex <- ifelse (is.na(b[[i]]$FixationIndex), 0, b[[i]]$FixationIndex)
        b[[i]]$SaccadeIndex <- ifelse (is.na(b[[i]]$SaccadeIndex), 0, b[[i]]$SaccadeIndex)
        # b[[i]]$X0.1 <- ifelse (is.na(b[[i]]$X0.1), 0, b[[i]]$X0.1)
}

# selecting complete cases 
# selecting compelete cases from X,Y,FixationIndex
b1 <- list()
b1 <- lapply(b, function(x) x[,c("GazePointX..ADCSpx.",
                                 "GazePointY..ADCSpx.")])

# finding complete cases of three columns; based on that: all columns output
c <- list()
for (i in 1:length(b1)) {
        c[[i]] <- b[[i]][complete.cases(b1[[i]]),]
}

# d: matrix with details at the level of gaze data
d <- c
d <- lapply(d, function(x) {
        AvePupil <- apply(x[,c("PupilLeft","PupilRight")],1,mean, na.rm= TRUE)
        cbind(x, AvePupil)
        #x <- x[,-c(x$PupilLeft,x$PupilRight)]
})
# dcopy <- d
d <- dcopy

# levels(d[[1]][,"Page"]) <- c(-1:21)

Sys.time()
# for (i in 1:length(d)) {
for (i in 1:2) {
        d[[i]]$MediaName <- as.character(d[[i]]$MediaName) #converting factor to character to enable selecting one element
        for (j in 1:nrow(d[[i]])) {
        # for (j in 1:5000) {
                if ((d[[i]][j,"MediaName"])=="") {d[[i]][j,"Page"] <- as.character(-1)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/") {d[[i]][j,"Page"] <- as.character(0)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/1") {d[[i]][j,"Page"] <- as.character(1)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/2") {d[[i]][j,"Page"] <- as.character(2)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/3") {d[[i]][j,"Page"] <- as.character(3)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/4") {d[[i]][j,"Page"] <- as.character(4)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/5") {d[[i]][j,"Page"] <- as.character(5)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/6") {d[[i]][j,"Page"] <- as.character(6)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/7") {d[[i]][j,"Page"] <- as.character(7)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/8") {d[[i]][j,"Page"] <- as.character(8)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/9") {d[[i]][j,"Page"] <- as.character(9)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/10") {d[[i]][j,"Page"] <- as.character(10)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/11") {d[[i]][j,"Page"] <- as.character(11)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/12") {d[[i]][j,"Page"] <- as.character(12)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/13") {d[[i]][j,"Page"] <- as.character(13)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/14") {d[[i]][j,"Page"] <- as.character(14)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/15") {d[[i]][j,"Page"] <- as.character(15)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/16") {d[[i]][j,"Page"] <- as.character(16)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/17") {d[[i]][j,"Page"] <- as.character(17)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/18") {d[[i]][j,"Page"] <- as.character(18)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/report") {d[[i]][j,"Page"] <- as.character(19)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/summary") {d[[i]][j,"Page"] <- as.character(20)
                } else if (d[[i]][j,"MediaName"] == "http://mis573server.wpi.edu/sus") {d[[i]][j,"Page"] <- as.character(21)
                }
                if (j>1) {
                        d[[i]][j,"PD"] <- ((d[[i]][j,"AvePupil"])-(d[[i]][j-1,"AvePupil"]))
                        d[[i]][j,"PD%"] <- round(((d[[i]][j,"PD"])/(d[[i]][j-1,"AvePupil"])*100), digits=3)
                        # d[[i]][j,"Fix_Velocity"] <- sqrt((d[[i]][j,"GazePointX..ADCSpx."]-d[[i]][j-1,"GazePointX..ADCSpx."])^2+
                        #                                      (d[[i]][j,"GazePointY..ADCSpx."]-d[[i]][j-1,"GazePointY..ADCSpx."])^2)/
                        #         (d[[i]][j,"RecordingTimestamp"]-d[[i]][j-1,"RecordingTimestamp"])
                }
                # error=browser()
        }
        d[[i]][,"Page"] <- as.factor(d[[i]][,"Page"])
        d[[i]]$IVTfac <- factor(d[[i]]$FixationIndex)
        d[[i]]$IVTSacFac <- factor(d[[i]]$SaccadeIndex)
        # d[[i]]$FIDfac <- factor(d[[i]]$"X0.1")
}
Sys.time()

# dcopy1 <- d # after running for first 2 participants with Pages
d <- dcopy1

# g: matrix with separation at the level of fixations for each participant
# g: matrix with computed pupil parameters (summary of each fixation)
g <- list()
length(g) <- length(d)
for (i in 1:length(g)) {
        g[[i]] <- data.frame("IVT"= NA, "Fix_Dens" = NA, "Fix_P_Mean" =NA,
                             "Fix_PD_Mean" =NA,"Fix_PD_Perc_Mean"=NA,
                             # "MediaName"=NA,
                             "Page"=NA, "Fix_Dur"=NA, "Dur"=NA, "Diff"=NA
                             # , "Fix_Velocity"=NA
                             )
}

Sys.time()
# for (i in 1:length(g)) { #length(g) is equal to number of participants
for (i in 1:2) { #length(g) is equal to number of participants
        s <- 1          #counter for rows
        p <- split(d[[i]], d[[i]]$Page)
        q <- as.numeric(levels(d[[i]]$Page))  #I could not select an element of a vector, so I converted it
        for (k in 1:length(q)) {
                h <- as.character(unique(p[[k]]$IVTfac))  #p[[k]]$IVTfac provides levels that probably asiigned in previous part of the code. So, I used uniques of the page and converted to character, because with as.numeric, for some reasons, it was starting from 1 instead of 0.
##########                
                o <- 0 # This counter will be used for removing data of fixations which are mutual between two pages
##########          
                for (j in 1:length(h)) {        #number of fixations for each participant      
                        y <- p[[k]][which(p[[k]]$IVTfac==h[j]),]
                        g[[i]][s,"IVT"] <- h[j]
                        # browser()
                        if (h[j] == 0 | j==length(h)) {         #with j=length(h) will remove the data for the last fixation which can prevent wrong calculations for the last fixation of each page which might be mutual between two pages
                                g[[i]][s,2:length(g[[i]])] <- NA
                                # g[[i]][s,"MediaName"] <- y[1,"MediaName"]
                                g[[i]][s,"Page"] <- q[k]

########## removing the first fixation of page if before any saccede
                        } else if (o==0 & p[[k]]$IVTfac[1]!=0) { # This part removes the data for fixations which are not started in the current page. We identify them if there is no saccade before the first fixation
                                        g[[i]][s,2:length(g[[i]])] <- NA
                                        # g[[i]][s,"MediaName"] <- y[1,"MediaName"]
                                        g[[i]][s,"Page"] <- q[k]
                                        g[[i]][s,"IVT"] <- -1
                                        o <- o+1
##############
                        } else {
                                Area <- (max(y$"GazePointX..ADCSpx.") - min(y$"GazePointX..ADCSpx."))*(max(y$"GazePointY..ADCSpx.") - min(y$"GazePointY..ADCSpx."))
                                if (Area==0) {          #if fixation includes only a point, it prevents generating NA and NAN.
                                        g[[i]][s,2:8] <- NA
                                } else {
                                        g[[i]][s,"Fix_Dens"] <- ((y[1,"GazeEventDuration"])/Area)
                                        g[[i]][s,"Fix_P_Mean"] <- mean(y$AvePupil, na.rm = TRUE)
                                        g[[i]][s,"Fix_PD_Mean"] <- mean(y$PD, na.rm = TRUE)
                                        g[[i]][s,"Fix_PD_Perc_Mean"] <- mean(y$"PD%", na.rm = TRUE)
                                        # g[[i]][s,"Fix_Velocity"] <- mean(y$"Fix_Velocity", na.rm = TRUE)
                                        # g[[i]][s,"MediaName"] <- y[1,"MediaName"]
                                        g[[i]][s,"Dur"] <- max(y$"RecordingTimestamp", na.rm = TRUE)-min(y$"RecordingTimestamp", na.rm = TRUE)
                                }
                                g[[i]][s,"Fix_Dur"] <- y[1,"GazeEventDuration"]
                                g[[i]][s,"Diff"] <- g[[i]][s,"Fix_Dur"]-g[[i]][s,"Dur"]
                                g[[i]][s,"Page"] <- q[k]
                                # browser()
                        }
                        s <- s+1
                }
        }
}
names(g) <- namesA
Sys.time()

###########################

# gs: g for saccade
# gs: matrix with separation at the level of saccades for each participant
# gs: matrix with computed pupil parameters (summary of each saccade)
gs <- list()
length(gs) <- length(d)
for (i in 1:length(gs)) {
        gs[[i]] <- data.frame("IVTSac"= NA, "Sac_Dens" = NA, "Sac_P_Mean" =NA,
                              "Sac_PD_Mean" =NA,"Sac_PD_Perc_Mean"=NA,
                              # "MediaName"=NA,
                              "SacDur"=NA
                              # , "Sac_Velocity"=NA
                              )
}

for (i in 1:length(gs)) { #length(gs) is equal to number of participants
        # browser()
        ys <- split(d[[i]], d[[i]]$IVTSacFac)
        # browser()
        hs <- as.numeric(levels(d[[i]]$IVTSacFac))  #I could not select an element of a vector, so I converted it
        for (j in 1:length(hs)) {        #number of fixations for each participant
                gs[[i]][j,"IVTSac"] <- hs[j] 
                if (hs[j] == 0) {        
                        gs[[i]][j,2:length(gs[[i]])] <- NA
                        #browser()
                } else {
                        Area <- (max(ys[[j]]$"GazePointX..ADCSpx.") - min(ys[[j]]$"GazePointX..ADCSpx."))*(max(ys[[j]]$"GazePointY..ADCSpx.") - min(ys[[j]]$"GazePointY..ADCSpx."))
                        if (Area==0) {          #if fixation includes only a point, it prevents generating NA and NAN.
                                gs[[i]][j,2:8] <- NA
                        } else {
                                gs[[i]][j,"Sac_Dens"] <- ((ys[[j]][1,"GazeEventDuration"])/Area)
                                gs[[i]][j,"Sac_P_Mean"] <- mean(ys[[j]]$AvePupil, na.rm = TRUE)
                                gs[[i]][j,"Sac_PD_Mean"] <- mean(ys[[j]]$PD, na.rm = TRUE)
                                gs[[i]][j,"Sac_PD_Perc_Mean"] <- mean(ys[[j]]$"PD%", na.rm = TRUE)
                                # gs[[i]][j,"Sac_Velocity"] <- mean(ys[[j]]$"Velocity", na.rm = TRUE)
                        }
                        # gs[[i]][j,"MediaName"] <- ys[[j]][1,"MediaName"]
                        gs[[i]][j,"SacDur"] <- ys[[j]][1,"GazeEventDuration"]
                        #browser()
                }
        }
}
names(gs) <- namesA


# m2: matrix of means of columns (means of data of all fixations of each participant)
# summary of each paricipant
gSum <- list()
gSum <- lapply(g, function(x) summarise(x, 
                                        F_Dens_M= mean(Fix_Dens, na.rm = TRUE),
                                        F_P_M= mean(Fix_P_Mean, na.rm = TRUE),
                                        F_P_SD= sd(Fix_P_Mean, na.rm = TRUE), 
                                        F_PD_M= mean(Fix_PD_Mean, na.rm = TRUE),
                                        F_PD_SD= sd(Fix_PD_Mean, na.rm = TRUE),
                                        F_PD_Perc_M= mean(Fix_PD_Perc_Mean, na.rm = TRUE), 
                                        F_PD_Perc_SD= sd(Fix_PD_Perc_Mean, na.rm = TRUE),
                                        # F_Velocity_M= mean(Velocity, na.rm = TRUE),
                                        # F_Velocity_SD= sd(Velocity, na.rm = TRUE),
                                        F_Dur= sum(Fix_Dur, na.rm = TRUE)))
m2 <- do.call(rbind,gSum)

gSum2 <- data.frame("NoFixation" = NA, "NoSaccade"=NA, "RecDuration"=NA)
for (i in 1:length(d)) {
        gSum2[i,"NoFixation"] <- ((d[[i]][max(which(d[[i]]["FixationIndex"]>0)),"FixationIndex"])-(d[[i]][min(which(d[[i]]["FixationIndex"]>0)),"FixationIndex"]))
        gSum2[i,"NoSaccade"] <- ((d[[i]][max(which(d[[i]]["SaccadeIndex"]>0)),"SaccadeIndex"])-(d[[i]][min(which(d[[i]]["SaccadeIndex"]>0)),"SaccadeIndex"]))
        gSum2[i,"RecDuration"] <- ((d[[i]][max(which(d[[i]]["RecordingTimestamp"]>0)),"RecordingTimestamp"])-(d[[i]][min(which(d[[i]]["RecordingTimestamp"]>0)),"RecordingTimestamp"]))
        #browser()
}
# gSum2
FixTable <- cbind(m2,gSum2)

# m2s: matrix of means of columns (means of data of all saccedes of each participant)
# summary of each paricipant
gsSum <- list()
gsSum <- lapply(gs, function(x) summarise(x, 
                                          S_Dens_M= mean(Sac_Dens, na.rm = TRUE),
                                          S_P_M= mean(Sac_P_Mean, na.rm = TRUE),
                                          S_P_SD= sd(Sac_P_Mean, na.rm = TRUE), 
                                          S_PD_M= mean(Sac_PD_Mean, na.rm = TRUE),
                                          S_PD_SD= sd(Sac_PD_Mean, na.rm = TRUE),
                                          S_PD_Perc_M= mean(Sac_PD_Perc_Mean, na.rm = TRUE), 
                                          S_PD_Perc_SD= sd(Sac_PD_Perc_Mean, na.rm = TRUE),
                                          S_Velocity_M= mean(Sac_Velocity, na.rm = TRUE),
                                          S_Velocity_SD= sd(Sac_Velocity, na.rm = TRUE),
                                          S_Dur= sum(Sac_Dur, na.rm = TRUE)))
SacTable <- do.call(rbind,gsSum)

## there is no need for corresponding m3 calculations for saccades.
