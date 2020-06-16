# Adding other pages of data to the selection page
# without removing communal fixation between two pages # with a second and calculated duration of events by timestamp
# rm(list = ls(all.names = TRUE)) removing all variables


setwd("~/Desktop/MSTBI/TobiiExtractedData/outputWRemovedAOIs")
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

#reading csv files
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


# subsetting desired columns
b <- list()
for (i in 1:length(a)) {
        b[[i]] <- a[[i]][,c("GazePointX..ADCSpx.",
                             "GazePointY..ADCSpx.","FixationIndex",
                             # "X0.1","X0.1_saccade_fid",
                             "PupilLeft","PupilRight",
                             "GazeEventDuration", "MediaName", "RecordingTimestamp",
                             "GazeEventType", "SaccadicAmplitude","RecordingDuration",
                             "SaccadeIndex","AbsoluteSaccadicDirection","EyeTrackerTimestamp"
        )]
}
Sys.time()

#replacing 0 with NAs in FixationIndex
for (i in 1:length(b)) {
        b[[i]]$FixationIndex <- ifelse (is.na(b[[i]]$FixationIndex), 0, b[[i]]$FixationIndex)
        b[[i]]$SaccadeIndex <- ifelse (is.na(b[[i]]$SaccadeIndex), 0, b[[i]]$SaccadeIndex)
        # b[[i]]$X0.1 <- ifelse (is.na(b[[i]]$X0.1), 0, b[[i]]$X0.1)
}


##############################
# using all points again including gaze points without GazePointX..ADCSpx. and GazePointY..ADCSpx. and probabaly pupil data
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

# without removing gaze points which miss GazeX, GazeY and probably pupil data
Sys.time()
c <- b
for (i in 1: length(c)) {
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
for (i in 1:length(d)) {
# for (i in 1:1) {
        d[[i]][,"Page"] <- NA
        # browser()
        # levels(d[[i]][,"Page"]) <- c(-1:21)     # I never understood why I did not need this line for Test1 participants but Test2. Without this line, for Test2 participants, we only get page=-1 but the page for others would be NA.
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

# adding EyeTrackerTimestamp later on for removing click and keyboard strokes. The above code will be updated to add this variable rather than repeating the task for adding this.
# e <- d
# for (i in 1:length(d)) {
#         z <- read.csv(file = files_full[i])[ ,"EyeTrackerTimestamp"]
#         e[[i]]$EyeTrackerTimestamp <- z
#         browser()
# }
# dcopy2 <- e
# d <- dcopy2

# g: matrix with separation at the level of fixations for each participant
# g: matrix with computed pupil parameters (summary of each fixation)
g <- list()
length(g) <- length(d)
for (i in 1:length(g)) {
        g[[i]] <- data.frame("IVT"= NA, "Fix_Dens" = NA, 
                             # "Fix_Dens_C_Dur" = NA,
                             "Fix_P_Mean" =NA,
                             "Fix_PD_Mean" =NA,"Fix_PD_Perc_Mean"=NA,
                             "Page"=NA, "Fix_Dur"=NA, "Fix_C_Dur" =NA, "Fix_Cnt_Dur" =NA
                             # , "Fix_Velocity"=NA
                             )
}

for (i in 1:length(g)) { #length(g) is equal to number of participants
# for (i in 11:11) { #length(g) is equal to number of participants
        s <- 1          #counter for rows
        p <- split(d[[i]], d[[i]]$Page)
        q <- as.numeric(levels(d[[i]]$Page))  #I could not select an element of a vector, so I converted it
        for (k in 1:length(q)) {
                if (q[k]>=0) {
                        h <- as.character(unique(p[[k]]$IVTfac))  #p[[k]]$IVTfac provides levels that probably assigned in previous part of the code. So, I used uniques of the page and converted to character, because with as.numeric, for some reasons, it was starting from 1 instead of 0.
                        for (j in 1:length(h)) {        #number of fixations for each participant      
                                y <- p[[k]][which(p[[k]]$IVTfac==h[j]),]
                                if (h[j] > 0) {
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
                                        g[[i]][s,"Page"] <- q[k]
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
gs <- list()
length(gs) <- length(d)
for (i in 1:length(gs)) {
        gs[[i]] <- data.frame("IVTSac"= NA, "Sac_Dens" = NA,
                              # "Sac_Dens_C_Dur" = NA,
                             "Sac_P_Mean" =NA,
                             "Sac_PD_Mean" =NA,"Sac_PD_Perc_Mean"=NA,
                             "Page"=NA, "Sac_Dur"=NA, "Sac_C_Dur" =NA, "Sac_Cnt_Dur" =NA
                             # , "Sac_Velocity"=NA
        )
}

for (i in 1:length(gs)) { #length(gs) is equal to number of participants
# for (i in 1:2) { #length(gs) is equal to number of participants
        s <- 1          #counter for rows
        ps <- split(d[[i]], d[[i]]$Page)
        qs <- as.numeric(levels(d[[i]]$Page))  #I could not select an element of a vector, so I converted it
        for (k in 1:length(qs)) {
                if (qs[k]>=0) {
                        hs <- as.character(unique(ps[[k]]$IVTSacFac))  #ps[[k]]$IVTSacFac provides levels that probably assigned in previous part of the code. So, I used uniques of the page and converted to character, because with as.numeric, for some reasons, it was starting from 1 instead of 0.
                        for (j in 1:length(hs)) {        #number of fixations for each participant      
                                ys <- ps[[k]][which(ps[[k]]$IVTSacFac==hs[j]),]
                                if (hs[j] > 0) {
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
                                        gs[[i]][s,"Page"] <- qs[k]
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

library(dplyr)
# FixPage and SacPage: matrix of means of columns (means of data of all fixations of each participant for each page)
# summary of each paricipant
gPage <- list()
gPage <- lapply(g, function(x) summarise(x %>% group_by(Page),
                                        F_Dens_M= mean(Fix_Dens, na.rm = TRUE),
                                        F_Dens_SD= sd(Fix_Dens, na.rm = TRUE),
                                        # F_Dens_C_Dur= mean(Fix_Dens_C_Dur, na.rm = TRUE),
                                        F_P_M= mean(Fix_P_Mean, na.rm = TRUE),
                                        F_P_SD= sd(Fix_P_Mean, na.rm = TRUE), 
                                        F_PD_M= mean(Fix_PD_Mean, na.rm = TRUE),
                                        F_PD_SD= sd(Fix_PD_Mean, na.rm = TRUE),
                                        F_PD_Perc_M= mean(Fix_PD_Perc_Mean, na.rm = TRUE), 
                                        F_PD_Perc_SD= sd(Fix_PD_Perc_Mean, na.rm = TRUE),
                                        # F_Velocity_M= mean(Velocity, na.rm = TRUE),
                                        # F_Velocity_SD= sd(Velocity, na.rm = TRUE),
                                        F_Dur_M= mean(Fix_Dur, na.rm = TRUE),
                                        F_Dur_Sum= sum(Fix_Dur, na.rm = TRUE),
                                        F_C_Dur_M= mean(Fix_C_Dur, na.rm = TRUE),
                                        F_C_Dur_Sum= sum(Fix_C_Dur, na.rm = TRUE),
                                        F_Cnt_Dur_M= mean(Fix_Cnt_Dur, na.rm = TRUE),
                                        F_Cnt_Dur_Sum= sum(Fix_Cnt_Dur, na.rm = TRUE)))
FixPage <- do.call(rbind,gPage)
FixPageCopy <- FixPage

Sys.time()

# gsPage: corresponding gPage for Saccades
gsPage <- list()
gsPage <- lapply(gs, function(x) summarise(x %>% group_by(Page), 
                                        S_Dens_M= mean(Sac_Dens, na.rm = TRUE),
                                        # S_Dens_C_Dur= mean(Sac_Dens_C_Dur, na.rm = TRUE),
                                        S_P_M= mean(Sac_P_Mean, na.rm = TRUE),
                                        S_P_SD= sd(Sac_P_Mean, na.rm = TRUE), 
                                        S_PD_M= mean(Sac_PD_Mean, na.rm = TRUE),
                                        S_PD_SD= sd(Sac_PD_Mean, na.rm = TRUE),
                                        S_PD_Perc_M= mean(Sac_PD_Perc_Mean, na.rm = TRUE), 
                                        S_PD_Perc_SD= sd(Sac_PD_Perc_Mean, na.rm = TRUE),
                                        # S_Velocity_M= mean(Sac_Velocity, na.rm = TRUE),
                                        # S_Velocity_SD= sd(Sac_Velocity, na.rm = TRUE),
                                        S_Dur_M= mean(Sac_Dur, na.rm = TRUE),
                                        S_Dur_Sum= sum(Sac_Dur, na.rm = TRUE),
                                        S_C_Dur_M= mean(Sac_C_Dur, na.rm = TRUE),
                                        S_C_Dur_Sum= sum(Sac_C_Dur, na.rm = TRUE),
                                        S_Cnt_Dur_M= mean(Sac_Cnt_Dur, na.rm = TRUE),
                                        S_Cnt_Dur_Sum= sum(Sac_Cnt_Dur, na.rm = TRUE)))
SacPage <- do.call(rbind,gsPage)
Sys.time()

options(scipen = 999) # converts scientific numbers
# AllPage is aggregation of fixation and sacccade data at the page level
AllPage <- cbind(FixPage, SacPage)
# Separating Participant, Test and Rec and removing row.names   # the code has written in a way that handles Recording number with 3 digits
AllPage[,"Participant"] <- substr(row.names(AllPage),start=1, stop=12)
AllPage[,"Test"] <- substr(AllPage$Participant,start=1, stop=5)
AllPage[,"Rec"] <- substr(AllPage$Participant,start=11, stop=12)
AllPage <- AllPage[,c(30,31,32,1:29)]
rownames(AllPage) <- c()
# Removing second column of page number
AllPage <- subset(AllPage, select = -c(Page.1) )


AllPageCopy <- AllPage
AllPage <- AllPageCopy

library(stringr)
# Adding NoFixation, NoSaccade, and RecDuration for each page
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

# redoing page level calculation for all data points of each participant
# FixTotal and SacTotal: matrix of means of columns (means of data of all fixations of each participant with accumulation of pages)
# summary of each paricipant
gTotal <- list()
gTotal <- lapply(g, function(x) summarise(x,
                                         F_Dens_M= mean(Fix_Dens, na.rm = TRUE),
                                         F_Dens_SD= sd(Fix_Dens, na.rm = TRUE),
                                         # F_Dens_C_Dur= mean(Fix_Dens_C_Dur, na.rm = TRUE),
                                         F_P_M= mean(Fix_P_Mean, na.rm = TRUE),
                                         F_P_SD= sd(Fix_P_Mean, na.rm = TRUE), 
                                         F_PD_M= mean(Fix_PD_Mean, na.rm = TRUE),
                                         F_PD_SD= sd(Fix_PD_Mean, na.rm = TRUE),
                                         F_PD_Perc_M= mean(Fix_PD_Perc_Mean, na.rm = TRUE), 
                                         F_PD_Perc_SD= sd(Fix_PD_Perc_Mean, na.rm = TRUE),
                                         # F_Velocity_M= mean(Velocity, na.rm = TRUE),
                                         # F_Velocity_SD= sd(Velocity, na.rm = TRUE),
                                         F_Dur_M= mean(Fix_Dur, na.rm = TRUE),
                                         F_Dur_Sum= sum(Fix_Dur, na.rm = TRUE),
                                         F_C_Dur_M= mean(Fix_C_Dur, na.rm = TRUE),
                                         F_C_Dur_Sum= sum(Fix_C_Dur, na.rm = TRUE),
                                         F_Cnt_Dur_M= mean(Fix_Cnt_Dur, na.rm = TRUE),
                                         F_Cnt_Dur_Sum= sum(Fix_Cnt_Dur, na.rm = TRUE)))
FixTotal <- do.call(rbind,gTotal)

# gsTotal: corresponding gTotal for Saccades
gsTotal <- list()
gsTotal <- lapply(gs, function(x) summarise(x, 
                                           S_Dens_M= mean(Sac_Dens, na.rm = TRUE),
                                           # S_Dens_C_Dur= mean(Sac_Dens_C_Dur, na.rm = TRUE),
                                           S_P_M= mean(Sac_P_Mean, na.rm = TRUE),
                                           S_P_SD= sd(Sac_P_Mean, na.rm = TRUE), 
                                           S_PD_M= mean(Sac_PD_Mean, na.rm = TRUE),
                                           S_PD_SD= sd(Sac_PD_Mean, na.rm = TRUE),
                                           S_PD_Perc_M= mean(Sac_PD_Perc_Mean, na.rm = TRUE), 
                                           S_PD_Perc_SD= sd(Sac_PD_Perc_Mean, na.rm = TRUE),
                                           # S_Velocity_M= mean(Sac_Velocity, na.rm = TRUE),
                                           # S_Velocity_SD= sd(Sac_Velocity, na.rm = TRUE),
                                           S_Dur_M= mean(Sac_Dur, na.rm = TRUE),
                                           S_Dur_Sum= sum(Sac_Dur, na.rm = TRUE),
                                           S_C_Dur_M= mean(Sac_C_Dur, na.rm = TRUE),
                                           S_C_Dur_Sum= sum(Sac_C_Dur, na.rm = TRUE),
                                           S_Cnt_Dur_M= mean(Sac_Cnt_Dur, na.rm = TRUE),
                                           S_Cnt_Dur_Sum= sum(Sac_Cnt_Dur, na.rm = TRUE)))
SacTotal <- do.call(rbind,gsTotal)

# AllPage is aggregation of fixation and sacccade data at the page level
AllTotal <- cbind(FixTotal, SacTotal)
# separating Participant, Test and Rec and removing row.names   # the code has written in a way that handles Recording number with 3 digits
AllTotal[,"Participant"] <- substr(row.names(AllTotal),start=1, stop=12)
AllTotal[,"Test"] <- substr(AllTotal$Participant,start=1, stop=5)
AllTotal[,"Rec"] <- substr(AllTotal$Participant,start=11, stop=12)
AllTotal <- AllTotal[,c(28,29,30,1:27)]
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

# finding a text
# strsplit(rownames(m2),"_")
# substr(row.names(AllPage),start=1, stop=12)
