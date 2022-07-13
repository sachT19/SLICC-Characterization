#Sachleen Tuteja, February 17, 2022, creating a demographics table from pcor data
#Mar 7,8 - modified to ensure that subject_id isn't truncated and additional
#analyses added

#packages
require(table1)
require(readr)
require(stringr)
require(dplyr)
require(ggplot2)
require(webr)
require(RColorBrewer)

#files 
demo <- read.csv("/Users/sachleentuteja/Desktop/northwestern/rawFiles/demographics/PCOR_demographics.csv", 
                 head = T, colClasses = c("subject_id" = "character"))
pcor <- read.csv("/Users/sachleentuteja/Desktop/northwestern/rawFiles/PCOR_raw_data.csv", 
                 head = T, colClasses = c("subject_id" = "character"))
slicc <- read.csv("/Users/sachleentuteja/Desktop/northwestern/rawFiles/Phe_211117_PCOR.csv",
                  head = T, colClasses = c("subject_id" = "character"))

#Table 1: demographics data - age, race, ethnicity of cases and control
table1(~ age_bin + race + ethnicity | cohort*sex, 
       data=demo, overall="Total")
#Figure 1: demographics data - ethnicity
ethnicity <- c("Declined","Hispanic or Latino", "Not Hispanic or Latino", "Other")
perEth <- c(11.3, 9.8, 78.5, 0.4)
demEth <- data.frame(ethnicity, perEth)
ethnicity <- ethnicity[order(perEth)]; perEth <- sort(perEth)
ethnicity.factor <- factor(ethnicity, levels = as.character(ethnicity))
p <- ggplot() + theme_bw() + geom_bar(aes(x = "", y = perEth, fill = ethnicity.factor),
                                 stat = "identity", color = "white") + 
  coord_polar("y", start = 0) +
ggtitle("Demographics By Ethnicity") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())
ypos <- cumsum(perEth) - 0.5 * perEth
ypos <- 100 - ypos
p + guides(fill = guide_legend(reverse = TRUE)) + 
  scale_fill_brewer(palette = "Greens", name = "Ethnicity") +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(hjust=0.5, size = 18),
        legend.key.size=unit(1,"cm")) +
  geom_text(aes(x = "", y = ypos, label = paste0(perEth, "%")),
            size = 3.5)
print(p)
  
#Table 2: pcor data - frequency of attribute, type, and vocab in the case
#control, and overall in the sample population
table1(~ attribute + type + vocab | cohort, 
       data=pcor, overall="Total")
#Table 3: pcor data - frequency of attribute, vocab in lab codes in the case
#and control, and overall in the sample population
table1(~ attribute + vocab | cohort*type, 
       data=pcor, overall="Total")
#Table 4: pcor data - frequency of attribute, vocab in lab codes
table1(~ attribute + vocab | type, 
       data=pcor, overall="Total")
#Table 5: pcor data - frequency of attribute, lab codes in vocab
table1(~ attribute + type | vocab, 
       data=pcor, overall="Total")

#merging pcor (raw data) and demo (demographics) by subject_id
mergePD <- merge(pcor, demo, by = "subject_id")

#extracting last two char from date column in mergePD
mergePD$year <- paste("20", str_sub(mergePD$date, -2), sep = "")

#if else statement - if 20LL is in the year column, then replace it with
#null and the rest of the values should be a substring from the date column
mergePD$year <- ifelse(grepl("20LL", mergePD$year), "NULL", 
                       paste("20", str_sub(mergePD$date, -2), sep = ""))

#Table 6: mergePD data - frequency of ... in case/control
table1(~ sex + race + ethnicity + attribute + type + year | cohort.y, 
       data=mergePD, overall = "Total")
#Table 7: mergePD data - frequency of ... in date
table1(~ sex + race + ethnicity + attribute + type | year, 
       data=mergePD, overall = "Total")
#Figure 2: mergePD data - vocab
vocab <- c("Diag","Lab", "Meds", "Proc")
perVoc <- c(10.5, 83.1, 6.3, 0.1)
merVoc <- data.frame(vocab, perVoc)
vocab <- vocab[order(perVoc)]; perVoc <- sort(perVoc)
vocab.factor <- factor(vocab, levels = as.character(vocab))
p <- ggplot() + theme_bw() + geom_bar(aes(x = "", y = perVoc, fill = vocab.factor),
                                      stat = "identity", color = "white") + 
  coord_polar("y", start = 0) +
  ggtitle("Distribution of Codes") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())
ypos <- cumsum(perVoc) - 0.5 * perVoc
ypos <- 100 - ypos
p + guides(fill = guide_legend(reverse = TRUE)) + 
  scale_fill_brewer(palette = "Blues", name = "Codes") +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(hjust=0.5, size = 18),
        legend.key.size=unit(1,"cm")) +
  geom_text(aes(x = "", y = ypos, label = paste0(perVoc, "%")),
            size = 4)
print(p)

#merging slicc criteria data with demographics
mergeSD <- merge(slicc, demo, by = "subject_id")

print("The SLICC criteria for SLE classification requires: 
      1) Fulfillment of at least four criteria, with at least one 
      clinical criterion AND one immunologic criterion OR 2) 
      Lupus nephritis as the sole clinical criterion in the presence 
      of ANA or anti-dsDNA antibodies.")

#sum of all values for one subject_id in mergeSD.
mergeSD$total <- rowSums(mergeSD[ , c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)], 
                       na.rm=TRUE)

#sum of values for clinical criteria
mergeSD$clinSum <- rowSums(mergeSD[ , c(4,5,10,11,13,14,16,17,18,19,20)],
                           na.rm=TRUE)
#sum of values for immunologic criteria
mergeSD$imuSum <- rowSums(mergeSD[ , c(6,7,8,9,12,15)], na.rm=TRUE)

#for each row, if total criteria >= 4, clinical criteria >= 1, immuno 
#criteria >= 1, then add binary value 1 in diag column
for(i in 1:nrow(mergeSD)) { 
  if(mergeSD$total[i] >= 4 && mergeSD$clinSum[i] >= 1 && mergeSD$imuSum[i] >= 1){
    mergeSD$diag[i] <- 1
  } else{
    mergeSD$diag[i] <- 0
  }
} 

class(mergeSD$diag) = "character"
print("64 out of 268 patients fit the clinical and immunological criteria for 
      SLE.")

#Figure 3: mergeSD data - number of patients diagnosed with SLE in relation to
#their sex
for(i in 1:nrow(mergeSD)) { 
  if(mergeSD$total[i] >= 4 && mergeSD$clinSum[i] >= 1 && mergeSD$imuSum[i] >= 1){
    mergeSD$Diagnosis[i] <- "SLE +"
  } else{
    mergeSD$Diagnosis[i] <- "SLE -"
  }
} 
class(mergeSD$Diagnosis) = "character"
class(mergeSD$diag) = "numeric"
sDiag = mergeSD %>% group_by(sex, Diagnosis)
PieDonut(sDiag, aes(Diagnosis, sex),
         r0 = 0.45, r1 = 0.9, 
         title = "SLE: Diagnosis by Sex")
#Table 8: mergeSD data - number of patients diagnosed with SLE in relation
#to their sex
table1(~ cohort.y*sex | Diagnosis, data=mergeSD, overall="Total")
#Figure 4: mergeSD data - number of patients diagnoses with SLE in relation to 
#their ethnicity
sEth = mergeSD %>% group_by(ethnicity, Diagnosis)
PieDonut(sEth, aes(Diagnosis, ethnicity),
         r0 = 0.45, r1 = 0.9,
         title = "SLE: Diagnosis by Ethnicity")
#Table 9: mergeSD data - number of patients diagnosed with SLE in relation 
#to their ethnicity
table1(~ cohort.y*ethnicity | Diagnosis, data=mergeSD, overall="Total")
#Figure 5: mergeSD data - number of patients diagnoses with SLE in relation to 
#their race
sRace = mergeSD %>% group_by(race, Diagnosis)
PieDonut(sRace, aes(Diagnosis, race),
         r0 = 0.45, r1 = 0.9,
         title = "SLE: Diagnosis by Race")
#Table 10: mergeSD data - number of patients diagnosed with SLE in relation 
#to their race
table1(~ cohort.y*race | Diagnosis, data=mergeSD, overall="Total")
#Figure 6: mergeSD data - number of patients diagnoses with SLE in relation to 
#their age group
sAge = mergeSD %>% group_by(age_bin, Diagnosis)
PieDonut(sAge, aes(Diagnosis, age_bin),
         r0 = 0.45, r1 = 0.9,
         title = "SLE: Diagnosis by Age")
#Table 11: mergeSD data - number of patients diagnosed with SLE in relation 
#to their age group
table1(~ cohort.y*age_bin | Diagnosis, data=mergeSD, overall="Total")
#Table 12: mergeSD data - number of patients diagnosed with SLE in relation 
#to the year