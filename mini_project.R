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
PAR50 <- CTM %>%
  group_by(Replicate) %>%
  mutate(Coef_P = ifelse(depth=0.50,paste0(Coef_P),paste0(Coef_P)))
  
  
PAR50 <- CTM %>%
  group_by(Replicate) %>%
  mutate(Coef_P = if(depth=0.50,paste0(Coef_P))


for(i in 1:nrow(cluster_2001)){
  if(rowSums(cluster_2001[i,c(4:11)])>0){
    cluster_2001$community[i]=cluster_2001$comm[i]
  }else
    cluster_2001$community[i]=NA
}


  cluster_2001= cluster_2001%>% 
  mutate(ligne = ifelse(li<10,paste0("L0",li),paste0("L",li)),
         colone = ifelse(col<10,paste0("C0",col),paste0("C",col)),
         Site = paste0(ligne,colone),.keep="unused") %>%
  dplyr::select(-c(ligne, colone, Plot)) %>% 
  rename(Plot=Site)








########## visualization ##########

ggplot(pigments, aes(y=Chla, x=Sampling, fill=Treatment)) +
  geom_boxplot()

ggplot(pigments, aes(y=Carot, x=Sampling, fill=Treatment)) +
  geom_boxplot()








