# MINI PROJECT ECOMONT 2022


########## loading packages ##########

library(readxl)
library(dplyr)
library(ggplot2)


########## loading data ##########

CTM <- read.csv("Data/PARLAC_CTM.csv")

pigments <- read_excel(path = "Data/PARLAC-Pigments.xlsx")
pigments <- subset(pigments[1:54,])

LC_Fit <- read_excel(path = "Data/PARLAC_LC_Fit.xlsx")


########## data structuration ##########

##### LC_Fit

# selecting only Absolute protocol
LC_Fit <- filter(LC_Fit, Protocol == "Abs")


##### pigments

# changing column names
colnames(pigments) <- c("Project", "Name", "Date", "Sampling", "Treatment", "Replicate", "Chla", "Carot")


##### CTM

# erasing useless columns
CTM <- CTM[,-c(12,13)]

# creating a Name column in CTM
CTM <-  CTM %>% 
  mutate(Name = paste(Replicate, Sampling, sep = "_"))

# creting data frame with means values
Means <- CTM %>%
  group_by(Name) %>%
  summarize(Temp=mean(Temp), Cond=mean(Cond), Turb=mean(Turb), O2_sat=mean(O2_sat), DO.mg=mean(DO.mg))

# creating column Photosynthetic Coefficient
Coef_P <- CTM %>%
  group_by(Replicate) %>%
  summarize(Coef_P=PAR/max(PAR))
# binding to CTM df
CTM <- cbind(CTM, Coef_P[,2])

# selecting PAR at ~50cm
PAR50 <- CTM$depth >= 0.49 & CTM$depth <= 0.51
PAR50sub <- subset(CTM, PAR50)
CoefM <- PAR50sub %>%
  group_by(Name) %>%
  summarize(Coef_P=mean(Coef_P))

CTM <- merge(CTM, Means, by="Name")
CTM <- merge(CTM, CoefM, by="Name")
CTM <- CTM[,-c(6:13)]
  
CTM <- unique(CTM)

rm(PAR50)
rm(PAR50sub)
rm(Means)
rm(Coef_P)
rm(CoefM)



########## visualization ##########

ggplot(pigments, aes(y=Chla, x=Sampling, fill=Treatment)) +
  geom_boxplot()

ggplot(pigments, aes(y=Carot, x=Sampling, fill=Treatment)) +
  geom_boxplot()







