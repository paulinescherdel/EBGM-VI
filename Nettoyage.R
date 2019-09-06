############################
# CREATION DE Z-SCORES OMS #
############################

# Chemin de travail 
###################
WD <-"D:/INSERM - PARIS/EBGM VI - construction courbes/"


# Appel des bases de données
###########################
load(file=paste0(WD,"/Donnees/DREES/Bases sauvegardees/drees.rda"))
load(file=paste0(WD,"Donnees/AFPA/AFPA_32/Bases sauvegardees/afpa.rda"))

# Library
#########
library(dplyr)
library(ggplot2)
library(Rmisc)

#################################
# Construction des classes d'age 
#################################

# Jours
#######
afpa$c_age_jour <- ifelse (afpa$age_jour<0,"99", 
                           ifelse (afpa$age_jour==0, "00", as.character(cut(afpa$age_jour,
                                                                            breaks = c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,
                                                                                       4015,4380,4745,5110,5475,5840,6205,6570,6935,7300), 
                                                                            include.lowest = FALSE, right=TRUE, 
                                                                            labels = c ("01","02","03","04","05","06","07","08","09","10",
                                                                                        "11","12","13","14","15","16","17","18","19","20")))))

drees$c_age_jour <- ifelse (drees$age_jour<0,"99", 
                            ifelse (drees$age_jour==0, "00", as.character(cut(drees$age_jour,
                                                                              breaks = c(0,365,730,1095,1460,1825,2190,2555,2920,3285,3650,
                                                                                         4015,4380,4745,5110,5475,5840,6205,6570,6935,7300), 
                                                                              include.lowest = FALSE, right=TRUE, 
                                                                              labels = c ("01","02","03","04","05","06","07","08","09","10",
                                                                                          "11","12","13","14","15","16","17","18","19","20")))))
# Mois
######
afpa$c_age_mois <- ifelse (afpa$age_jour<0,"99", 
                           ifelse (afpa$age_jour==0,"00", as.character (cut(afpa$age_mois,
                                                                            breaks = c(0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,
                                                                                       57,60,63,66,69,72,75,78,81,84,87,90,93,96,99,102,105,
                                                                                       108,111,114,117,120,123,126,129,132,135,138,141,144,
                                                                                       147,150,153,156,159,162,165,168,171,174,177,180,183,
                                                                                       186,189,192,195,198,201,204,207,210,213,216,219,222,
                                                                                       225,228,231,234,237,240),
                                                                            include.lowest = FALSE, right=TRUE, 
                                                                            labels = c("01","02","03","04","05","06","07","08","09","10","11","12",
                                                                                       "13","14","15","16","17","18","19","20","21","22","23","24",
                                                                                       "25","26","27","28","29","30","31","32","33","34","35","36",
                                                                                       "37","38","39","40","41","42","43","44","45","46","47","48",
                                                                                       "49","50","51","52","53","54","55","56","57","58","59","60",
                                                                                       "61","62","63","64","65","66","67","68","69","70","71","72",
                                                                                       "73","74","75","76","77","78","79","80")))))

drees$c_age_mois <- ifelse (drees$age_jour<0,"99", 
                            ifelse (drees$age_jour==0,"00", as.character (cut(drees$age_mois,
                                                                              breaks = c(0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,
                                                                                         57,60,63,66,69,72,75,78,81,84,87,90,93,96,99,102,105,
                                                                                         108,111,114,117,120,123,126,129,132,135,138,141,144,
                                                                                         147,150,153,156,159,162,165,168,171,174,177,180,183,
                                                                                         186,189,192,195,198,201,204,207,210,213,216,219,222,
                                                                                         225,228,231,234,237,240),
                                                                              include.lowest = FALSE, right=TRUE, 
                                                                              labels = c("01","02","03","04","05","06","07","08","09","10","11","12",
                                                                                         "13","14","15","16","17","18","19","20","21","22","23","24",
                                                                                         "25","26","27","28","29","30","31","32","33","34","35","36",
                                                                                         "37","38","39","40","41","42","43","44","45","46","47","48",
                                                                                         "49","50","51","52","53","54","55","56","57","58","59","60",
                                                                                         "61","62","63","64","65","66","67","68","69","70","71","72",
                                                                                         "73","74","75","76","77","78","79","80")))))
# Annees
########
afpa$c_age_annee <- ifelse (afpa$age_jour<0,"99", 
                            ifelse (afpa$age_jour==0,"00", as.character (cut(afpa$age_annee,
                                                                             breaks= c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), 
                                                                             include.lowest = FALSE, right=TRUE,
                                                                             labels = c("01","02","03","04","05","06","07","08","09","10","11",
                                                                                        "12","13","14","15","16","17","18","19","20")))))

drees$c_age_annee <- ifelse (drees$age_jour<0,"99", 
                             ifelse (drees$age_jour==0,"00", as.character (cut(drees$age_annee,
                                                                               breaks= c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), 
                                                                               include.lowest = FALSE, right=TRUE,
                                                                               labels = c("01","02","03","04","05","06","07","08","09","10","11",
                                                                                          "12","13","14","15","16","17","18","19","20")))))






#####################################
# Application critères de sélection #
#####################################

#### AFPA

# Suppression des enfants nés avec un petit poids de naissance
# ------------------------------------------------------------
afpa_pdn  <- subset(afpa, (poids<2.5 & age_jour==0)) #4 278    
afpa_pdn  <- as.data.frame(afpa_pdn[ ,c("pedid")])
names(afpa_pdn)[1] <- "pedid"
afpa_pdn$indic     <- 1
afpa_pdn_1 <- merge(afpa, afpa_pdn, by.x="pedid", by.y="pedid", all=TRUE)

afpa_pdn_1$indic <- replace(afpa_pdn_1$indic,is.na(afpa_pdn_1$indic), 0) # 1 543 085
afpa_pdn_2       <- subset(afpa_pdn_1, indic==0)                         # 1 483 830 
afpa_pdn_2       <- subset(afpa_pdn_2, select=-c(indic))

# Suppression des enfants nés avec une date de naissance > 1990
# -------------------------------------------------------------
afpa_pdn_ddn  <- subset(afpa_pdn_2, ADN >= 1990) 

rm(afpa_pdn, afpa_pdn_1, afpa_pdn_2)



#### DREES

# Suppression des enfants nés avec un petit poids de naissance
# ------------------------------------------------------------
drees_pdn   <- subset(drees, (poids<2.5 & age_jour==0)) # 2 079    
drees_pdn   <- as.data.frame(drees_pdn[ ,c("ID")])
names(drees_pdn)[1] <- "ID"
drees_pdn$indic     <- 1
drees_pdn_1 <- merge(drees, drees_pdn, by.x="ID", by.y="ID", all=TRUE)

drees_pdn_1$indic <- replace(drees_pdn_1$indic,is.na(drees_pdn_1$indic), 0) 
drees_pdn_2       <- subset(drees_pdn_1, indic==0) # 72 331 
drees_pdn_2       <- subset(drees_pdn_2,select=-c(indic))

# Suppression des enfants nés avec une date de naissance > 1990
# -------------------------------------------------------------
drees_pdn_ddn  <- subset(drees_pdn_2, ADN >= 1990) 
rm(drees_pdn, drees_pdn_1, drees_pdn_2)




###############################################
# Programme de nettoyage par les z-scores OMS #
###############################################

# Sous groupes d'age pour les z-scores
######################################

# AFPA 
#######

afpa_05<- subset(afpa_pdn_ddn,
                 afpa_pdn_ddn$age_jour >= 0 & afpa_pdn_ddn$age_jour <= 1856 & 
                  ((afpa_pdn_ddn$poids >= 0 | is.na(afpa_pdn_ddn$poids)) & 
                  (afpa_pdn_ddn$taille >= 0 | is.na(afpa_pdn_ddn$taille)))) 



afpa_519<- subset(afpa_pdn_ddn,
                  afpa_pdn_ddn$age_jour > 1856 & afpa_pdn_ddn$age_jour <= 6935 & 
                   ((afpa_pdn_ddn$poids >= 0 | is.na(afpa_pdn_ddn$poids)) & 
                   (afpa_pdn_ddn$taille >= 0 | is.na(afpa_pdn_ddn$taille)))) 




# DREES
#######
drees_05 <- subset(drees_pdn_ddn,
                   drees_pdn_ddn$age_jour >= 0 & drees_pdn_ddn$age_jour <= 1856 & 
                   ((drees_pdn_ddn$poids >= 0 | is.na(drees_pdn_ddn$poids)) & 
                     (drees_pdn_ddn$taille >= 0 | is.na(drees_pdn_ddn$taille))))

drees_519 <- subset(drees_pdn_ddn,
                    drees_pdn_ddn$age_jour > 1856 & drees_pdn_ddn$age_jour <= 6935 & 
                    ((drees_pdn_ddn$poids >= 0 | is.na(drees_pdn_ddn$poids)) & 
                      (drees_pdn_ddn$taille >= 0 | is.na(drees_pdn_ddn$taille))))






#######################
# Macro des z-score OMS
#######################

# 0-5 ans (en jour)
# -----------------
weianthro<-read.table(paste0(WD,"References OMS/Z_score_oms05/weianthro.txt"),header=T,sep="",skip=0)
lenanthro<-read.table(paste0(WD,"References OMS/Z_score_oms05/lenanthro.txt"),header=T,sep="",skip=0)
bmianthro<-read.table(paste0(WD,"References OMS/Z_score_oms05/bmianthro.txt"),header=T,sep="",skip=0)
hcanthro<-read.table(paste0(WD,"References OMS/Z_score_oms05/hcanthro.txt"),header=T,sep="",skip=0)
acanthro<-read.table(paste0(WD,"References OMS/Z_score_oms05/acanthro.txt"),header=T,sep="",skip=0)
ssanthro<-read.table(paste0(WD,"References OMS/Z_score_oms05/ssanthro.txt"),header=T,sep="",skip=0)
tsanthro<-read.table(paste0(WD,"References OMS/Z_score_oms05/tsanthro.txt"),header=T,sep="",skip=0)
wflanthro<-read.table(paste0(WD,"References OMS/Z_score_oms05/wflanthro.txt"),header=T,sep="",skip=0)
wfhanthro<-read.table(paste0(WD,"References OMS/Z_score_oms05/wfhanthro.txt"),header=T,sep="",skip=0)
source(paste0(WD,"References OMS/Z_score_oms05/igrowup_standard.r"))
source(paste0(WD,"References OMS/Z_score_oms05/igrowup_restricted.r"))

z_score05 <- function(etude, FileLab, mydf, sexe, age_jour, poids, taille, pc)
{
  igrowup.standard(FilePath=paste0(WD,"Donnees/", etude),
                   FileLab=FileLab,
                   mydf=mydf,
                   age=age_jour,
                   sex=sexe,
                   age.month=F,   
                   weight=poids,
                   lenhei=taille,
                   headc=pc)
  
  r <- read.csv2(paste0(WD,"Donnees/",etude, FileLab, "_z_st.csv"), sep=",", dec="." , na.strings = "NA")
  return(r)
}

## AFPA 
afpa_z01 <- z_score05(etude="AFPA/AFPA_32/",
                      FileLab="afpa05",
                      mydf=afpa_05,
                      sexe=sexe,
                      age_jour=age_jour,
                      poids=poids,
                      taille=taille, 
                      pc=pc)

## DREES
drees_05$pc <- NA
drees_z05 <- z_score05(etude="DREES/",
                       FileLab="drees05",
                       mydf=drees_05,
                       sexe=sexe,
                       age_jour=age_jour,
                       poids=poids,
                       taille=taille,
                       pc=pc)






# 5-19 ans (en mois) 
# -----------------
wfawho2007<-read.table(paste0(WD,"References OMS/Z_score_oms519/wfawho2007.txt"),header=T,sep="",skip=0)
hfawho2007<-read.table(paste0(WD,"References OMS/Z_score_oms519/hfawho2007.txt"),header=T,sep="",skip=0)
bfawho2007<-read.table(paste0(WD,"References OMS/Z_score_oms519/bfawho2007.txt"),header=T,sep="",skip=0)
source(paste0(WD,"References OMS/Z_score_oms519/who2007.r"))

z_score519 <- function(etude, FileLab, mydf, sexe, age_mois, poids, taille)
{
  who2007(FilePath=paste0(WD,"Donnees/",etude),
          FileLab=FileLab,
          mydf=mydf, 
          age=age_mois,
          sex=sexe, 
          weight=poids, 
          height=taille)
  
  r <- read.csv2(paste0(WD,"Donnees/",etude, FileLab, "_z.csv"), sep=",", dec="." , na.strings = "NA")
  return(r)
}

## AFPA 
afpa_z519 <- z_score519(etude="AFPA/AFPA_32/",
                        FileLab="afpa519",
                        mydf=afpa_519,
                        sexe=sexe,
                        age_mois=age_mois,
                        poids=poids,
                        taille=taille)

## DREES
drees_519$pc<-NA
drees_z519 <- z_score519(etude="DREES/",
                         FileLab="drees519",
                         mydf=drees_519,
                         sexe=sexe,
                         age_mois=age_mois,
                         poids=poids,
                         taille=taille)




# Ajouter une variable dans la table de 5-19
############################################

## AFPA 
afpa_z05 <- afpa_z05[ ,c("pedid","ped","id","sexe","ADN","age_jour","poids","taille","pc","age_g","age_mois","age_annee","c_age_jour","c_age_mois","c_age_annee","cbmi","zlen","zwei","zwfl","zbmi","zhc")]
colnames(afpa_z05)[16:21] <- c("IMC","z_t_oms","z_p_oms","z_pt_oms","z_b_oms","z_pc_oms")

afpa_z519$z_pt_oms<- NA
afpa_z519$z_pc_oms<- NA
afpa_z519 <- afpa_z519[ ,c("pedid","ped","id","sexe","ADN","age_jour","poids","taille","pc","age_g","age_mois","age_annee","c_age_jour","c_age_mois","c_age_annee","cbmi","zhfa","zwfa","zbfa","z_pt_oms","z_pc_oms")]
colnames(afpa_z519)[16:19] <- c("IMC","z_t_oms","z_p_oms","z_b_oms")


## DREES
drees_z05 <- drees_z05[,c("sexe","ID","ENQ","DDN","ADN","age_g","t", "age_mois","age_jour","age_annee", "c_age_jour", "c_age_mois", "c_age_annee","poids","taille","pc","IMC","zlen","zwei","zwfl","zbmi")]
colnames(drees_z05)[18:21] <- c( "z_t_oms","z_p_oms","z_pt_oms","z_b_oms")
drees_z05$z_pc_oms<- NA
drees_z519$z_pt_oms<- NA

drees_z519 <- drees_z519[,c("sexe","ID","ENQ","DDN","ADN","age_g","t","age_mois","age_jour","age_annee","c_age_jour", "c_age_mois", "c_age_annee","poids","taille","pc","IMC","zhfa","zwfa","zbfa","z_pt_oms")]
colnames(drees_z519)[18:20] <- c( "z_t_oms","z_p_oms","z_b_oms")
drees_z519$z_pc_oms<- NA


# Sauvegarde des bases
######################
save(afpa_z05, file=paste0(WD,"Donnees/AFPA/AFPA_32/Bases sauvegardees/afpa_z05.rda"))
save(afpa_z519, file=paste0(WD,"Donnees/AFPA/AFPA_32/Bases sauvegardees/afpa_z519.rda"))

save(drees_z05, file=paste0(WD,"Donnees/DREES/Bases sauvegardees/drees_z05.rda"))
save(drees_z519, file=paste0(WD,"Donnees/DREES/Bases sauvegardees/drees_z519.rda"))


# Fusionner les deux bases de donnees 
#####################################
fusion <- function(table05, table519)
{
  r <- rbind(table05, table519)
  return(r)
}
afpa_z019  <- fusion(afpa_z05, afpa_z519)
drees_z019 <- fusion(drees_z05, drees_z519)


# Sauvegarde des bases
######################
save(afpa_z019, file=paste0(WD,"/Donnees/AFPA/AFPA_32/Bases sauvegardees/afpa_z019.rda"))
save(drees_z019, file=paste0(WD,"/Donnees/DREES/Bases sauvegardees/drees_z019.rda"))


  
#######################
# NETTOYAGE : Z-SCORE #
#######################

# Créer indicatrice pour les z-scores compris entre -4 et 4 puis -5 et 5
###########################################################################
indic_z_score <- function(table)
{
  # Z_scores de 4
  ###############
  table$z_t_oms_z4<-as.factor(ifelse(is.na(table$z_t_oms)==T   | table$z_t_oms > -4 & table$z_t_oms < 4, 0, 1))
  table$z_p_oms_z4<-as.factor(ifelse(is.na(table$z_p_oms)==T   | table$z_p_oms > -4 & table$z_p_oms < 4, 0, 1))
  table$z_pc_oms_z4<-as.factor(ifelse(is.na(table$z_pc_oms)==T | table$z_pc_oms > -4 & table$z_pc_oms < 4, 0, 1))
  table$z_b_oms_z4<-as.factor(ifelse(is.na(table$z_b_oms)==T   | table$z_b_oms > -4 & table$z_b_oms < 4, 0, 1))
  
  
  # Z_scores de 5
  ###############
  table$z_t_oms_z5<-as.factor(ifelse(is.na(table$z_t_oms)==T   | table$z_t_oms > -5 & table$z_t_oms < 5, 0, 1))
  table$z_p_oms_z5<-as.factor(ifelse(is.na(table$z_p_oms)==T   | table$z_p_oms > -5 & table$z_p_oms < 5, 0, 1))
  table$z_pc_oms_z5<-as.factor(ifelse(is.na(table$z_pc_oms)==T | table$z_pc_oms > -5 & table$z_pc_oms < 5, 0, 1))
  table$z_b_oms_z5<-as.factor(ifelse(is.na(table$z_b_oms)==T   | table$z_b_oms > -5 & table$z_b_oms < 5, 0, 1))
  
  
  return(table)
}
afpa_z019  <- indic_z_score(afpa_z019)
drees_z019 <- indic_z_score(drees_z019)



# Sauvegarde des bases
######################
save(afpa_z019, file=paste0(WD,"/Donnees/AFPA/AFPA_32/Bases sauvegardees/afpa_z019.rda"))
save(drees_z019, file=paste0(WD,"/Donnees/DREES/Bases sauvegardees/drees_z019.rda"))






########################
# NETTOYAGE : DOUBLONS #
########################

# Comptabiliser le nombre maximal de doublons à corriger (Macro applicable pour max 3 doublons)
########################################################
nb_doublon<- function (entree, mesure, indz, ident){

  # Préparation de la table
  tab_d <- subset(entree, eval(parse(text=indz))==0)
  tab_d <- tab_d[!duplicated(tab_d[c(ident,"age_jour", mesure)]),]
  tab_d <- tab_d[order(tab_d[[ident]], tab_d$age_jour, noquote(tab_d[[mesure]])),]  
  tab_d <- data.table(tab_d)
  as.character(tab_d[[ident]])
  setkeyv(tab_d, ident)
  
  # Identification des dates identiques répétées : lag + lead de 1 : pour creer un indicateur = 1 quand l'age est egal au precedent 0 sinon
  tab_d[, age_p1  := shift(age_jour, n=1, fill=NA, type="lag"),  by=ident] 
  tab_d[, age_p_1 := shift(age_jour, n=1, fill=NA, type="lead"), by=ident] 
  
  # Indicatrice
  tab_d$indic_age_tot <- ifelse((is.na(tab_d$age_p1)==F & tab_d$age_jour==tab_d$age_p1) | (is.na(tab_d$age_p_1)==F &tab_d$age_jour==tab_d$age_p_1), 1,0)
  tab_d$indic_age_tot <- replace(tab_d$indic_age_tot,is.na(tab_d$indic_age_tot)==T,0)
  
  # Isoler les valeurs pour calculer le nombre max de repetition 
  tab_d2 <- tab_d[tab_d$indic_age_tot==1,]
  
  # Nombre maximal de doublon
  nb_doublon_maxi <- max(ftable(by(tab_d2$indic_age_tot, list(tab_d2[[ident]], tab_d2$age_jour), sum, na.rm=TRUE)), na.rm=TRUE)
  
  # Nombre de doublons par individu
  nb_doublon_ind <-  data.frame(ftable(by(tab_d2$indic_age_tot, list(tab_d2[[ident]], tab_d2$age_jour), sum, na.rm=TRUE)))
  table(nb_doublon_ind$Freq)
  

  return(nb_doublon_maxi)
  
}

## AFPA 
nb_doublon(afpa_z019, "poids",  "z_p_oms_z5", "pedid") # 3
nb_doublon(afpa_z019, "taille", "z_t_oms_z5", "pedid") # 3
nb_doublon(afpa_z019, "pc",     "z_pc_oms_z5","pedid")# 2

nb_doublon(afpa_z019, "poids",  "z_p_oms_z4", "pedid") # 3
nb_doublon(afpa_z019, "taille", "z_t_oms_z4", "pedid") # 3
nb_doublon(afpa_z019, "pc",     "z_pc_oms_z4","pedid")# 2

## DREES
nb_doublon(drees_z019, "poids",  "z_p_oms_z5", "ID") # 0
nb_doublon(drees_z019, "taille", "z_t_oms_z5", "ID") # 0

nb_doublon(drees_z019, "poids",  "z_p_oms_z4", "ID") # 0
nb_doublon(drees_z019, "taille", "z_t_oms_z4", "ID") # 0



# Supression ponctuel de doublons
#################################
afpa_z019 <- subset(afpa_z019, !(afpa_z019$pedid == 1502914 & afpa_z019$age_jour == 428) & !(afpa_z019$pedid == 1501635 & afpa_z019$age_jour == 582) & !(afpa_z019$pedid == 1601681 & afpa_z019$age_jour == 1833 & afpa_z019$z_b_oms== -0.19))
                    

# Supression automatique de doublons
#####################################
source(paste0(WD,"Programme/AFPA/AFPA_32/Nettoyage doublon simplifie.r"))


drees_z019$doublon_t_z4<- as.factor(0)
drees_z019$doublon_p_z4<- as.factor(0)
drees_z019$doublon_pc_z4<- as.factor(0)
drees_z019$doublon_t_z5<- as.factor(0)
drees_z019$doublon_p_z5<- as.factor(0)
drees_z019$doublon_pc_z5<- as.factor(0)


# Sauvegarde des bases
######################
save(afpa_z019,  file=paste0(WD,"/Donnees/AFPA/AFPA_32/Bases sauvegardees/afpa_z019.rda"))
save(drees_z019, file=paste0(WD,"/Donnees/DREES/Bases sauvegardees/drees_z019.rda"))


##################################################################################################
# NETTOYAGE : VARIATIONS DE Z-SCORES PAR INTERVALLE DE TEMPS 
##################################################################################################
drees_z019$pedid <- drees_z019$ID
drees_z019$ped   <- 99

variation_z_score <-function(table,ind_var_z_t_z4, ind_var_z_p_z4, ind_var_z_b_z4 , ind_var_z_pc_z4, ind_var_z_t_z5, ind_var_z_p_z5, ind_var_z_b_z5 , ind_var_z_pc_z5 )
{
  variation_z <- function(table,ident,mesure,age,z,ind_z,ind_doub,variation_z,ind_var_z,seuil1, seuil2)
  {
    table_v <- table[is.na(table[[z]])==F & table[[ind_z]]==0 & table[[ind_doub]]==0,]
    #table_v <- table_v[table_v[[ind_var_z]]==0,]  # enlever ici
    table_v <- data.table(table_v[!duplicated(table_v[c(ident,age,mesure)]),])
    as.character(table_v[[ident]])
    setkeyv(table_v, ident)
    table_v <- table_v[order(table_v[[ident]], table_v[[age]], table_v[[mesure]]),]
    
    # Calcul des variations de croissance
    table_v[,noquote(paste0(z,"_l")):=shift(eval(parse(text=z)), n=1, fill=NA, type="lag"), by=ident]   # z-scores
    table_v[,noquote(variation_z):=eval(parse(text=z))-eval(parse(text=paste0(z,"_l")))]  # variations z-score
    
    # Création d'une indicatrice des variations de z-scores selon un seuil pré-défini
    table_v[[paste0(ind_var_z,"_s")]]<-as.factor(ifelse(table_v[[variation_z]] < seuil1,"-1",ifelse(table_v[[variation_z]] > seuil2,"1","0")))
    
    # Création d'une indicatrice multiplicative des variations de z-scores
    table_v[,noquote(paste0(ind_var_z,"_s_l")):= shift(eval(parse(text=paste0(ind_var_z,"_s"))), n=1, fill=NA, type="lag"), by=ident] # indicatrice variations
    table_v[,noquote(paste0(ind_var_z,"_m")):= as.factor(as.numeric(as.character(eval(parse(text=paste0(ind_var_z,"_s")))))*as.numeric(as.character(eval(parse(text=paste0(ind_var_z,"_s_l"))))))]
    
    # Création d'une indicatrice mi-finale des variations de z-scores
    table_v[,noquote(paste0(ind_var_z,"_m_l")):= shift(eval(parse(text=paste0(ind_var_z,"_m"))), n=1, fill=NA, type="lead"), by=ident] # indicatrice multiplicative variations
    table_v[[paste0(ind_var_z,"_m_f")]] <- as.factor(ifelse(is.na(table_v[[paste0(ind_var_z,"_m_l")]])==T,0,ifelse(table_v[[paste0(ind_var_z,"_m_l")]]==-1,1,0)))
    table_v <- data.frame(table_v)
    
    return(table_v)
  }
  
  table_vt_oms_z4<-variation_z(table,"pedid","taille","age_annee","z_t_oms","z_t_oms_z4","doublon_t_z4","var_z_ta_oms_z4",ind_var_z_t_z4, -1, 1.3)
  table_vp_oms_z4<-variation_z(table,"pedid","poids","age_annee","z_p_oms","z_p_oms_z4","doublon_p_z4","var_z_pa_oms_z4",ind_var_z_p_z4, -0.5, 1.0)
  table_vpc_oms_z4<-variation_z(table,"pedid","pc","age_annee","z_pc_oms","z_pc_oms_z4","doublon_pc_z4","var_z_pca_oms_z4",ind_var_z_pc_z4, -1, 1.7)
  table_vb_oms_z4<-variation_z(table,"pedid","IMC","age_annee","z_b_oms","z_b_oms_z4","doublon_b_z4","var_z_ba_oms_z4",ind_var_z_b_z4, -1, 1.7)
  
  table_vt_oms_z5<-variation_z(table,"pedid","taille","age_annee","z_t_oms","z_t_oms_z5","doublon_t_z5","var_z_ta_oms_z5",ind_var_z_t_z5, -1, 1.3)
  table_vp_oms_z5<-variation_z(table,"pedid","poids","age_annee","z_p_oms","z_p_oms_z5","doublon_p_z5","var_z_pa_oms_z5",ind_var_z_p_z5, -0.5, 1.0)
  table_vpc_oms_z5<-variation_z(table,"pedid","pc","age_annee","z_pc_oms","z_pc_oms_z5","doublon_pc_z5","var_z_pca_oms_z5",ind_var_z_pc_z5,-1, 1.7)
  table_vb_oms_z5<-variation_z(table,"pedid","IMC","age_annee","z_b_oms","z_b_oms_z5","doublon_b_z5","var_z_ba_oms_z5",ind_var_z_b_z5,-1, 1.7)
  
  table <- merge(table,table_vt_oms_z4[,c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_t_oms", "var_z_ta_oms_z4", paste0(ind_var_z_t_z4,"_m_f"))], by=c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_t_oms"), all.x=T)
  table <- merge(table,table_vp_oms_z4[,c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_p_oms", "var_z_pa_oms_z4", paste0(ind_var_z_p_z4,"_m_f"))], by=c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_p_oms"), all.x=T)
  table <- merge(table,table_vpc_oms_z4[,c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_pc_oms", "var_z_pca_oms_z4", paste0(ind_var_z_pc_z4,"_m_f"))], by=c("pedid", "age_jour", "taille", "poids","pc", "IMC", "z_pc_oms"), all.x=T)
  table <- merge(table,table_vb_oms_z4[,c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_b_oms", "var_z_ba_oms_z4", paste0(ind_var_z_b_z4,"_m_f"))], by=c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_b_oms"), all.x=T)
  
  table <- merge(table,table_vt_oms_z5[,c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_t_oms", "var_z_ta_oms_z5", paste0(ind_var_z_t_z5,"_m_f"))], by=c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_t_oms"), all.x=T)
  table <- merge(table,table_vp_oms_z5[,c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_p_oms", "var_z_pa_oms_z5", paste0(ind_var_z_p_z5,"_m_f"))], by=c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_p_oms"), all.x=T)
  table <- merge(table,table_vpc_oms_z5[,c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_pc_oms", "var_z_pca_oms_z5", paste0(ind_var_z_pc_z5,"_m_f"))], by=c("pedid", "age_jour", "taille", "poids","pc", "IMC", "z_pc_oms"), all.x=T)
  table <- merge(table,table_vb_oms_z5[,c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_b_oms", "var_z_ba_oms_z5", paste0(ind_var_z_b_z5,"_m_f"))], by=c("pedid", "age_jour", "taille", "poids", "pc", "IMC", "z_b_oms"), all.x=T)
  
  
  table[[paste0(ind_var_z_t_z4,"_f_c")]] <- ifelse(is.na(table[[paste0(ind_var_z_t_z4,"_m_f")]])==T | table[[paste0(ind_var_z_t_z4,"_m_f")]]==0,0,1)
  table[[paste0(ind_var_z_p_z4,"_f_c")]] <- ifelse(is.na(table[[paste0(ind_var_z_p_z4,"_m_f")]])==T | table[[paste0(ind_var_z_p_z4,"_m_f")]]==0,0,1)
  table[[paste0(ind_var_z_pc_z4,"_f_c")]] <- ifelse(is.na(table[[paste0(ind_var_z_pc_z4,"_m_f")]])==T | table[[paste0(ind_var_z_pc_z4,"_m_f")]]==0,0,1)
  table[[paste0(ind_var_z_b_z4,"_f_c")]] <- ifelse(is.na(table[[paste0(ind_var_z_b_z4,"_m_f")]])==T | table[[paste0(ind_var_z_b_z4,"_m_f")]]==0,0,1)
  
  table[[paste0(ind_var_z_t_z5,"_f_c")]] <- ifelse(is.na(table[[paste0(ind_var_z_t_z5,"_m_f")]])==T | table[[paste0(ind_var_z_t_z5,"_m_f")]]==0,0,1)
  table[[paste0(ind_var_z_p_z5,"_f_c")]] <- ifelse(is.na(table[[paste0(ind_var_z_p_z5,"_m_f")]])==T | table[[paste0(ind_var_z_p_z5,"_m_f")]]==0,0,1)
  table[[paste0(ind_var_z_pc_z5,"_f_c")]] <- ifelse(is.na(table[[paste0(ind_var_z_pc_z5,"_m_f")]])==T | table[[paste0(ind_var_z_pc_z5,"_m_f")]]==0,0,1)
  table[[paste0(ind_var_z_b_z5,"_f_c")]] <- ifelse(is.na(table[[paste0(ind_var_z_b_z5,"_m_f")]])==T | table[[paste0(ind_var_z_b_z5,"_m_f")]]==0,0,1)
  
  table[[(ind_var_z_t_z4)]] <- as.factor(ifelse(table[[paste0(ind_var_z_t_z4,"_f_c")]]==1,1,0))   # remettre ici
  table[[(ind_var_z_p_z4)]] <- as.factor(ifelse(table[[paste0(ind_var_z_p_z4,"_f_c")]]==1,1,0))   # remettre ici
  table[[(ind_var_z_pc_z4)]]<- as.factor(ifelse(table[[paste0(ind_var_z_pc_z4,"_f_c")]]==1,1,0)) # remettre ici
  table[[(ind_var_z_b_z4)]] <- as.factor(ifelse(table[[paste0(ind_var_z_b_z4,"_f_c")]]==1,1,0))   # remettre ici
  
  table[[(ind_var_z_t_z5)]] <- as.factor(ifelse(table[[paste0(ind_var_z_t_z5,"_f_c")]]==1,1,0))   # remettre ici
  table[[(ind_var_z_p_z5)]] <- as.factor(ifelse(table[[paste0(ind_var_z_p_z5,"_f_c")]]==1,1,0))   # remettre ici
  table[[(ind_var_z_pc_z5)]]<- as.factor(ifelse(table[[paste0(ind_var_z_pc_z5,"_f_c")]]==1,1,0)) # remettre ici
  table[[(ind_var_z_b_z5)]] <- as.factor(ifelse(table[[paste0(ind_var_z_b_z5,"_f_c")]]==1,1,0))   # remettre ici
  
  #table[[(ind_var_z_t_z4)]]  <- as.factor(ifelse(table[[paste0(ind_var_z_t_z4,"_f_c")]]==1 | table[[ind_var_z_t_z4]]==1,1,0))     # enlever ici
  #table[[(ind_var_z_p_z4)]]  <- as.factor(ifelse(table[[paste0(ind_var_z_p_z4,"_f_c")]]==1 | table[[ind_var_z_p_z4]]==1,1,0))     # enlever ici
  #table[[(ind_var_z_pc_z4)]] <- as.factor(ifelse(table[[paste0(ind_var_z_pc_z4,"_f_c")]]==1 | table[[ind_var_z_pc_z4]]==1,1,0))  # enlever ici
  #table[[(ind_var_z_b_z4)]]  <- as.factor(ifelse(table[[paste0(ind_var_z_b_z4,"_f_c")]]==1 | table[[ind_var_z_b_z4]]==1,1,0))     # enlever ici
  
  #table[[(ind_var_z_t_z5)]]  <- as.factor(ifelse(table[[paste0(ind_var_z_t_z5,"_f_c")]]==1 | table[[ind_var_z_t_z5]]==1,1,0))     # enlever ici
  #table[[(ind_var_z_p_z5)]]  <- as.factor(ifelse(table[[paste0(ind_var_z_p_z5,"_f_c")]]==1 | table[[ind_var_z_p_z5]]==1,1,0))     # enlever ici
  #table[[(ind_var_z_pc_z5)]] <- as.factor(ifelse(table[[paste0(ind_var_z_pc_z5,"_f_c")]]==1 | table[[ind_var_z_pc_z5]]==1,1,0))  # enlever ici
  #table[[(ind_var_z_b_z5)]]  <- as.factor(ifelse(table[[paste0(ind_var_z_b_z5,"_f_c")]]==1 | table[[ind_var_z_b_z5]]==1,1,0))    # enlever ici
  
  table <- table[,c("pedid", "age_jour", "age_mois", "age_annee", "c_age_jour","c_age_mois","c_age_annee","sexe","taille", "poids","pc","IMC", "ped", "ADN",
                    "z_t_oms","z_p_oms", "z_pt_oms", "z_b_oms", "z_pc_oms",
                    "z_t_oms_z4","z_p_oms_z4","z_b_oms_z4","z_pc_oms_z4","z_t_oms_z5","z_p_oms_z5","z_b_oms_z5", "z_pc_oms_z5",
                    "doublon_p_z4", "doublon_t_z4", "doublon_pc_z4", "doublon_p_z5", "doublon_t_z5", "doublon_pc_z5",
                    "var_z_t_oms_z4","var_z_p_oms_z4","var_z_b_oms_z4","var_z_pc_oms_z4","var_z_t_oms_z5","var_z_p_oms_z5", "var_z_b_oms_z5", "var_z_pc_oms_z5")]
  return(table)
}
# ATTENTION : 1er macro AFPA et DREES il faut que les "#" soient mis en place aux lignes indiquées dans la macro mais pour les 2nd et 3eme ligne d'appel de la macro a lancer il faut les enlever

## AFPA 
afpa_z019  <- variation_z_score(afpa_z019,"var_z_t_oms_z4","var_z_p_oms_z4","var_z_b_oms_z4","var_z_pc_oms_z4","var_z_t_oms_z5","var_z_p_oms_z5","var_z_b_oms_z5","var_z_pc_oms_z5")
afpa_z019  <- variation_z_score(afpa_z019,"var_z_t_oms_z4","var_z_p_oms_z4","var_z_b_oms_z4","var_z_pc_oms_z4","var_z_t_oms_z5","var_z_p_oms_z5","var_z_b_oms_z5","var_z_pc_oms_z5") # Retirer les # du programme
afpa_z019  <- variation_z_score(afpa_z019,"var_z_t_oms_z4","var_z_p_oms_z4","var_z_b_oms_z4","var_z_pc_oms_z4","var_z_t_oms_z5","var_z_p_oms_z5","var_z_b_oms_z5","var_z_pc_oms_z5") # Pas de variations importantes retrouvées


## DREES
drees_z019 <- variation_z_score(drees_z019,"var_z_t_oms_z4","var_z_p_oms_z4","var_z_b_oms_z4","var_z_pc_oms_z4","var_z_t_oms_z5","var_z_p_oms_z5","var_z_b_oms_z5","var_z_pc_oms_z5")
drees_z019 <- variation_z_score(drees_z019,"var_z_t_oms_z4","var_z_p_oms_z4","var_z_b_oms_z4","var_z_pc_oms_z4","var_z_t_oms_z5","var_z_p_oms_z5","var_z_b_oms_z5","var_z_pc_oms_z5") # Retirer les # du programme
drees_z019 <- variation_z_score(drees_z019,"var_z_t_oms_z4","var_z_p_oms_z4","var_z_b_oms_z4","var_z_pc_oms_z4","var_z_t_oms_z5","var_z_p_oms_z5","var_z_b_oms_z5","var_z_pc_oms_z5") # Pas de variations importantes retrouvées

drees_z019<- subset(drees_z019, select=-c(ped))
names(drees_z019)[1] <-"ID" 


# Sauvegarde des bases
######################
save(afpa_z019, file=paste0(WD,"/Donnees/AFPA/AFPA_32/Bases sauvegardees/afpa_z019.rda"))
save(drees_z019, file=paste0(WD,"/Donnees/DREES/Bases sauvegardees/drees_z019.rda"))




######################################################
# Selection des enfants avec un grand nombre de mesure
######################################################

# creation classe 
# ---------------
afpa_z019[["classe"]] <- ifelse(afpa_z019[["age_mois"]] >=0   & afpa_z019[["age_mois"]] < 6 , "0-6",
                         ifelse(afpa_z019[["age_mois"]] >=6   & afpa_z019[["age_mois"]] <12 , "6-12",
                         ifelse(afpa_z019[["age_mois"]]>=12   & afpa_z019[["age_mois"]] <24 , "12-24",    
                         ifelse(afpa_z019[["age_mois"]]>=24   & afpa_z019[["age_mois"]] <36 , "24-36",
                         ifelse(afpa_z019[["age_mois"]]>=36   & afpa_z019[["age_mois"]] <48 , "36-48",
                         ifelse(afpa_z019[["age_mois"]]>=48   & afpa_z019[["age_mois"]] <60 , "48-60",
                         ifelse(afpa_z019[["age_mois"]]>=60   & afpa_z019[["age_mois"]] <72 , "60-72",
                         ifelse(afpa_z019[["age_mois"]]>=72   & afpa_z019[["age_mois"]] <84 , "72-84",
                         ifelse(afpa_z019[["age_mois"]]>=84   & afpa_z019[["age_mois"]] <96 , "84-96",
                         ifelse(afpa_z019[["age_mois"]]>=96   & afpa_z019[["age_mois"]] <108, "96-108",
                         ifelse(afpa_z019[["age_mois"]]>=108  & afpa_z019[["age_mois"]] <120, "108-120",
                         ifelse(afpa_z019[["age_mois"]]>=120  & afpa_z019[["age_mois"]] <132, "120-132",
                         ifelse(afpa_z019[["age_mois"]]>=132  & afpa_z019[["age_mois"]] <144, "132-144",
                         ifelse(afpa_z019[["age_mois"]]>=144  & afpa_z019[["age_mois"]] <156, "144-156",
                         ifelse(afpa_z019[["age_mois"]]>=156  & afpa_z019[["age_mois"]] <168, "156-168", 
                         ifelse(afpa_z019[["age_mois"]]>=168  & afpa_z019[["age_mois"]] <180, "168-180", 
                         ifelse(afpa_z019[["age_mois"]]>=180  & afpa_z019[["age_mois"]] <192, "180-192", 
                         ifelse(afpa_z019[["age_mois"]]>=192  & afpa_z019[["age_mois"]] <204, "192-204", 
                         ifelse(afpa_z019[["age_mois"]]>=204                                , ">204",NA))))))))))))))))))) 


# Calcul du nombre de mesure par identifiant et par classe
# --------------------------------------------------------
nb_mesure_age <- function(table, mesure, z, var_z, doublon, classe)
{
  table_f <- subset(table, table[[z]]==0 &  table[[var_z]] %in% c(0,1) & table[[doublon]]==0)
  table_f[["cpt"]] <- ifelse(is.na(table_f[[mesure]])==F,1,0)
  resu <- aggregate(table_f[["cpt"]], by=list(pedid=table_f[["pedid"]], classe=table_f[[classe]]), FUN=sum)
  return(resu)
}
tab_cpt_T_z4  <- nb_mesure_age(afpa_z019, "taille", "z_t_oms_z4",  "var_z_t_oms_z4",  "doublon_t_z4",  "classe") 
tab_cpt_P_z4  <- nb_mesure_age(afpa_z019, "poids",  "z_p_oms_z4",  "var_z_p_oms_z4",  "doublon_p_z4",  "classe") 
tab_cpt_PC_z4 <- nb_mesure_age(afpa_z019, "pc",     "z_pc_oms_z4", "var_z_pc_oms_z4", "doublon_pc_z4", "classe")

tab_cpt_T_z5  <- nb_mesure_age(afpa_z019, "taille", "z_t_oms_z5",  "var_z_t_oms_z5",  "doublon_t_z5",  "classe") 
tab_cpt_P_z5  <- nb_mesure_age(afpa_z019, "poids",  "z_p_oms_z5",  "var_z_p_oms_z5",  "doublon_p_z5",  "classe") 
tab_cpt_PC_z5 <- nb_mesure_age(afpa_z019, "pc",     "z_pc_oms_z5", "var_z_pc_oms_z5", "doublon_pc_z5", "classe") 



# Application des seuils pré-établis pour la taille, le poids et le PC (interquatile * 3 = programme descriptif_mesure.R)
# -----------------------------------------------------------------------------------------------------------------------

tab_cpt_T_z4$largefreq_T_z4 <- ifelse((tab_cpt_T_z4$classe=="6-12"  & tab_cpt_T_z4$x>6)   |
                                        (tab_cpt_T_z4$classe=="12-24" & tab_cpt_T_z4$x>10)   |
                                        (tab_cpt_T_z4$classe=="24-36" & tab_cpt_T_z4$x>5)    |
                                        (tab_cpt_T_z4$classe=="36-48" & tab_cpt_T_z4$x>5)    |
                                        (tab_cpt_T_z4$classe=="48-60" & tab_cpt_T_z4$x>5)    |
                                        (tab_cpt_T_z4$classe=="60-72" & tab_cpt_T_z4$x>5)    |
                                        (tab_cpt_T_z4$classe=="72-84" & tab_cpt_T_z4$x>5)    |
                                        (tab_cpt_T_z4$classe=="84-96" & tab_cpt_T_z4$x>5)    | 
                                        (tab_cpt_T_z4$classe=="96-108"  & tab_cpt_T_z4$x>5)  |
                                        (tab_cpt_T_z4$classe=="108-120" & tab_cpt_T_z4$x>5)  |
                                        (tab_cpt_T_z4$classe=="120-132" & tab_cpt_T_z4$x>5)  |
                                        (tab_cpt_T_z4$classe=="132-144" & tab_cpt_T_z4$x>5)  |
                                        (tab_cpt_T_z4$classe=="144-156" & tab_cpt_T_z4$x>5)  |
                                        (tab_cpt_T_z4$classe=="156-168" & tab_cpt_T_z4$x>5)  |
                                        (tab_cpt_T_z4$classe=="168-180" & tab_cpt_T_z4$x>5)  |
                                        (tab_cpt_T_z4$classe=="180-192" & tab_cpt_T_z4$x>5)  |
                                        (tab_cpt_T_z4$classe=="192-204" & tab_cpt_T_z4$x>5)  |
                                        (tab_cpt_T_z4$classe==">204"    & tab_cpt_T_z4$x>5),1,0)


tab_cpt_P_z4$largefreq_P_z4 <- ifelse((tab_cpt_P_z4$classe=="6-12"  & tab_cpt_P_z4$x>10)  |
                                        (tab_cpt_P_z4$classe=="12-24" & tab_cpt_P_z4$x>14)  |
                                        (tab_cpt_P_z4$classe=="24-36" & tab_cpt_P_z4$x>9)   |
                                        (tab_cpt_P_z4$classe=="36-48" & tab_cpt_P_z4$x>9)   |
                                        (tab_cpt_P_z4$classe=="48-60" & tab_cpt_P_z4$x>9)   |
                                        (tab_cpt_P_z4$classe=="60-72" & tab_cpt_P_z4$x>5)   |
                                        (tab_cpt_P_z4$classe=="72-84" & tab_cpt_P_z4$x>5)   |
                                        (tab_cpt_P_z4$classe=="84-96" & tab_cpt_P_z4$x>5)   | 
                                        (tab_cpt_P_z4$classe=="96-108"  & tab_cpt_P_z4$x>5) |
                                        (tab_cpt_P_z4$classe=="108-120" & tab_cpt_P_z4$x>5) |
                                        (tab_cpt_P_z4$classe=="120-132" & tab_cpt_P_z4$x>5) |
                                        (tab_cpt_P_z4$classe=="132-144" & tab_cpt_P_z4$x>5) |
                                        (tab_cpt_P_z4$classe=="144-156" & tab_cpt_P_z4$x>5) |
                                        (tab_cpt_P_z4$classe=="156-168" & tab_cpt_P_z4$x>5) |
                                        (tab_cpt_P_z4$classe=="168-180" & tab_cpt_P_z4$x>5) |
                                        (tab_cpt_P_z4$classe=="180-192" & tab_cpt_P_z4$x>5) |
                                        (tab_cpt_P_z4$classe=="192-204" & tab_cpt_P_z4$x>5) |
                                        (tab_cpt_P_z4$classe==">204"    & tab_cpt_P_z4$x>5),1,0)


tab_cpt_PC_z4$largefreq_PC_z4 <- ifelse((tab_cpt_PC_z4$classe=="6-12"  & tab_cpt_PC_z4$x>6)   |
                                          (tab_cpt_PC_z4$classe=="12-24" & tab_cpt_PC_z4$x>13)  |
                                          (tab_cpt_PC_z4$classe=="24-36" & tab_cpt_PC_z4$x>5)   |
                                          (tab_cpt_PC_z4$classe=="36-48" & tab_cpt_PC_z4$x>5)   |
                                          (tab_cpt_PC_z4$classe=="48-60" & tab_cpt_PC_z4$x>5)   |
                                          (tab_cpt_PC_z4$classe=="60-72" & tab_cpt_PC_z4$x>5),1,0)





tab_cpt_T_z5$largefreq_T_z5 <- ifelse((tab_cpt_T_z5$classe=="6-12"  & tab_cpt_T_z5$x>6)   |
                                        (tab_cpt_T_z5$classe=="12-24" & tab_cpt_T_z5$x>10)   |
                                        (tab_cpt_T_z5$classe=="24-36" & tab_cpt_T_z5$x>5)    |
                                        (tab_cpt_T_z5$classe=="36-48" & tab_cpt_T_z5$x>5)    |
                                        (tab_cpt_T_z5$classe=="48-60" & tab_cpt_T_z5$x>5)    |
                                        (tab_cpt_T_z5$classe=="60-72" & tab_cpt_T_z5$x>5)    |
                                        (tab_cpt_T_z5$classe=="72-84" & tab_cpt_T_z5$x>5)    |
                                        (tab_cpt_T_z5$classe=="84-96" & tab_cpt_T_z5$x>5)    | 
                                        (tab_cpt_T_z5$classe=="96-108"  & tab_cpt_T_z5$x>5)  |
                                        (tab_cpt_T_z5$classe=="108-120" & tab_cpt_T_z5$x>5)  |
                                        (tab_cpt_T_z5$classe=="120-132" & tab_cpt_T_z5$x>5)  |
                                        (tab_cpt_T_z5$classe=="132-144" & tab_cpt_T_z5$x>5)  |
                                        (tab_cpt_T_z5$classe=="144-156" & tab_cpt_T_z5$x>5)  |
                                        (tab_cpt_T_z5$classe=="156-168" & tab_cpt_T_z5$x>5)  |
                                        (tab_cpt_T_z5$classe=="168-180" & tab_cpt_T_z5$x>5)  |
                                        (tab_cpt_T_z5$classe=="180-192" & tab_cpt_T_z5$x>5)  |
                                        (tab_cpt_T_z5$classe=="192-204" & tab_cpt_T_z5$x>5)  |
                                        (tab_cpt_T_z5$classe==">204"    & tab_cpt_T_z5$x>5),1,0)


tab_cpt_P_z5$largefreq_P_z5 <- ifelse((tab_cpt_P_z5$classe=="6-12"  & tab_cpt_P_z5$x>10)  |
                                        (tab_cpt_P_z5$classe=="12-24" & tab_cpt_P_z5$x>14)  |
                                        (tab_cpt_P_z5$classe=="24-36" & tab_cpt_P_z5$x>9)   |
                                        (tab_cpt_P_z5$classe=="36-48" & tab_cpt_P_z5$x>9)   |
                                        (tab_cpt_P_z5$classe=="48-60" & tab_cpt_P_z5$x>9)   |
                                        (tab_cpt_P_z5$classe=="60-72" & tab_cpt_P_z5$x>5)   |
                                        (tab_cpt_P_z5$classe=="72-84" & tab_cpt_P_z5$x>5)   |
                                        (tab_cpt_P_z5$classe=="84-96" & tab_cpt_P_z5$x>5)   | 
                                        (tab_cpt_P_z5$classe=="96-108"  & tab_cpt_P_z5$x>5) |
                                        (tab_cpt_P_z5$classe=="108-120" & tab_cpt_P_z5$x>5) |
                                        (tab_cpt_P_z5$classe=="120-132" & tab_cpt_P_z5$x>5) |
                                        (tab_cpt_P_z5$classe=="132-144" & tab_cpt_P_z5$x>5) |
                                        (tab_cpt_P_z5$classe=="144-156" & tab_cpt_P_z5$x>5) |
                                        (tab_cpt_P_z5$classe=="156-168" & tab_cpt_P_z5$x>5) |
                                        (tab_cpt_P_z5$classe=="168-180" & tab_cpt_P_z5$x>5) |
                                        (tab_cpt_P_z5$classe=="180-192" & tab_cpt_P_z5$x>5) |
                                        (tab_cpt_P_z5$classe=="192-204" & tab_cpt_P_z5$x>5) |
                                        (tab_cpt_P_z5$classe==">204"    & tab_cpt_P_z5$x>5),1,0)


tab_cpt_PC_z5$largefreq_PC_z5 <- ifelse((tab_cpt_PC_z5$classe=="6-12"  & tab_cpt_PC_z5$x>6)   |
                                          (tab_cpt_PC_z5$classe=="12-24" & tab_cpt_PC_z5$x>13)  |
                                          (tab_cpt_PC_z5$classe=="24-36" & tab_cpt_PC_z5$x>5)   |
                                          (tab_cpt_PC_z5$classe=="36-48" & tab_cpt_PC_z5$x>5)   |
                                          (tab_cpt_PC_z5$classe=="48-60" & tab_cpt_PC_z5$x>5)   |
                                          (tab_cpt_PC_z5$classe=="60-72" & tab_cpt_PC_z5$x>5),1,0)

tab_cpt_T_z4_int <- aggregate(tab_cpt_T_z4$largefreq_T_z4,   by=list(pedid=tab_cpt_T_z4$pedid),FUN=sum)
colnames(tab_cpt_T_z4_int) <- c("pedid", "xt_z4")

tab_cpt_P_z4_int <- aggregate(tab_cpt_P_z4$largefreq_P_z4,    by=list(pedid=tab_cpt_P_z4$pedid),FUN=sum)
colnames(tab_cpt_P_z4_int) <- c("pedid", "xp_z4")

tab_cpt_PC_z4_int<- aggregate(tab_cpt_PC_z4$largefreq_PC_z4,  by=list(pedid=tab_cpt_PC_z4$pedid),FUN=sum)
colnames(tab_cpt_PC_z4_int) <- c("pedid", "xpc_z4")

tab_cpt_T_z5_int <- aggregate(tab_cpt_T_z5$largefreq_T_z5,    by=list(pedid=tab_cpt_T_z5$pedid),FUN=sum)
colnames(tab_cpt_T_z5_int) <- c("pedid", "xt_z5")

tab_cpt_P_z5_int <- aggregate(tab_cpt_P_z5$largefreq_P_z5,    by=list(pedid=tab_cpt_P_z5$pedid),FUN=sum)
colnames(tab_cpt_P_z5_int) <- c("pedid", "xp_z5")

tab_cpt_PC_z5_int<- aggregate(tab_cpt_PC_z5$largefreq_PC_z5,  by=list(pedid=tab_cpt_PC_z5$pedid),FUN=sum)
colnames(tab_cpt_PC_z5_int) <- c("pedid", "xpc_z5")


# Indice de supression d'enfants si le nombre de mesure est trop important
# ------------------------------------------------------------------------
afpa_z019 <- merge(x=afpa_z019, y=tab_cpt_T_z4_int, by="pedid", all.x=T)
afpa_z019 <- merge(x=afpa_z019, y=tab_cpt_P_z4_int, by="pedid", all.x=T)
afpa_z019 <- merge(x=afpa_z019, y=tab_cpt_PC_z4_int, by="pedid", all.x=T)

afpa_z019 <- merge(x=afpa_z019, y=tab_cpt_T_z5_int, by="pedid", all.x=T)
afpa_z019 <- merge(x=afpa_z019, y=tab_cpt_P_z5_int, by="pedid", all.x=T)
afpa_z019 <- merge(x=afpa_z019, y=tab_cpt_PC_z5_int, by="pedid", all.x=T)

afpa_z019$largefreq_t_z4   <- as.factor(ifelse(is.na(afpa_z019$xt_z4)==T  | afpa_z019$xt_z4==0, 0, 1))
afpa_z019$largefreq_p_z4   <- as.factor(ifelse(is.na(afpa_z019$xp_z4)==T  | afpa_z019$xp_z4==0, 0, 1))
afpa_z019$largefreq_pc_z4  <- as.factor(ifelse(is.na(afpa_z019$xpc_z4)==T | afpa_z019$xpc_z4==0, 0, 1))

afpa_z019$largefreq_t_z5   <- as.factor(ifelse(is.na(afpa_z019$xt_z5)==T  | afpa_z019$xt_z5==0, 0, 1))
afpa_z019$largefreq_p_z5   <- as.factor(ifelse(is.na(afpa_z019$xp_z5)==T  | afpa_z019$xp_z5==0, 0, 1))
afpa_z019$largefreq_pc_z5  <- as.factor(ifelse(is.na(afpa_z019$xpc_z5)==T | afpa_z019$xpc_z5==0, 0, 1))

afpa_z019 <- subset(afpa_z019,select=-c(xt_z4, xp_z4, xpc_z4, xt_z5, xp_z5, xpc_z5))


# Sauvegarde des bases
######################
save(afpa_z019, file=paste0(WD,"/Donnees/AFPA/AFPA_32/Bases sauvegardees/afpa_z019.rda"))



#############################################################
# Selection des mesures dans les deux premières annees de vie
#############################################################

# tirage aléatoire
# ----------------
supp_mesure <- function(table, mesure, z, var_z, doublon,largefreq_t,largefreq_p, largefreq_pc,  echant_0_6, echant_6_12, echant_12_24, echant_p24)
{
  echant_0_6 <- subset(table, table[["classe"]]=="0-6"    & is.na(table[[mesure]])==F & table[[z]]==0 &  table[[var_z]]==0 & table[[doublon]]==0 & table[[largefreq_t]]== 0 & table[[largefreq_p]]== 0 & table[[largefreq_pc]]== 0)
  echant_0_6<- echant_0_6[!duplicated(echant_0_6[c("pedid","age_jour",mesure)]),]
  echant_0_6$unif <- runif(nrow(echant_0_6), 0,1)
  echant_0_6 <- echant_0_6 %>% group_by(pedid) %>% top_n(5, unif)
 
  echant_6_12 <-subset(table, table[["classe"]]=="6-12"   & is.na(table[[mesure]])==F & table[[z]]==0 &  table[[var_z]]==0 & table[[doublon]]==0 & table[[largefreq_t]]== 0 & table[[largefreq_p]]== 0 & table[[largefreq_pc]]== 0)
  echant_6_12<- echant_6_12[!duplicated(echant_6_12[c("pedid","age_jour",mesure)]),]
  echant_6_12$unif <- runif(nrow(echant_6_12), 0,1)
  echant_6_12 <- echant_6_12 %>% group_by(pedid) %>% top_n(3, unif)
  
  echant_12_24 <-subset(table, table[["classe"]]=="12-24" & is.na(table[[mesure]])==F & table[[z]]==0 &  table[[var_z]]==0 & table[[doublon]]==0 & table[[largefreq_t]]== 0 & table[[largefreq_p]]== 0 & table[[largefreq_pc]]== 0)
  echant_12_24<- echant_12_24[!duplicated(echant_12_24[c("pedid","age_jour",mesure)]),]
  echant_12_24$unif <- runif(nrow(echant_12_24), 0,1)
  echant_12_24 <- echant_12_24 %>% group_by(pedid) %>% top_n(3, unif)
  
  echant_p24 <- subset(table, !(table[["classe"]] %in% c("0-6", "6-12", "12-24")) & is.na(table[[mesure]])==F & table[[z]]== 0 & table[[var_z]]== 0 & table[[doublon]]== 0  & table[[largefreq_t]]== 0 & table[[largefreq_p]]== 0 & table[[largefreq_pc]]== 0)
  
  afpa <- as.data.frame(rbind(echant_0_6, echant_6_12,echant_12_24))[,-c(49)]
  afpa <- as.data.frame(rbind(afpa,echant_p24))
  
  return(afpa)
}
afpa_T_z4  <- supp_mesure(afpa_z019, "taille", "z_t_oms_z4", "var_z_t_oms_z4",  "doublon_t_z4",  "largefreq_t_z4", "largefreq_p_z4", "largefreq_pc_z4", echant_T_0_6,  echant_T_6_12,  echant_T_12_24,  echant_T_p24)
afpa_P_z4  <- supp_mesure(afpa_z019, "poids",  "z_p_oms_z4", "var_z_p_oms_z4",  "doublon_p_z4",  "largefreq_t_z4", "largefreq_p_z4", "largefreq_pc_z4", echant_P_0_6,  echant_P_6_12,  echant_P_12_24,  echant_P_p24)
afpa_PC_z4 <- supp_mesure(afpa_z019, "pc",     "z_pc_oms_z4","var_z_pc_oms_z4", "doublon_pc_z4", "largefreq_t_z4", "largefreq_p_z4", "largefreq_pc_z4", echant_PC_0_6, echant_PC_6_12, echant_PC_12_24, echant_PC_p24)

afpa_T_z5  <- supp_mesure(afpa_z019, "taille", "z_t_oms_z5", "var_z_t_oms_z5",  "doublon_t_z5",  "largefreq_t_z5", "largefreq_p_z5", "largefreq_pc_z5", echant_T_0_6,  echant_T_6_12,  echant_T_12_24,  echant_T_p24)
afpa_P_z5  <- supp_mesure(afpa_z019, "poids",  "z_p_oms_z5", "var_z_p_oms_z5",  "doublon_p_z5",  "largefreq_t_z5", "largefreq_p_z5", "largefreq_pc_z5", echant_P_0_6,  echant_P_6_12,  echant_P_12_24,  echant_P_p24)
afpa_PC_z5 <- supp_mesure(afpa_z019, "pc",     "z_pc_oms_z5","var_z_pc_oms_z5", "doublon_pc_z5", "largefreq_t_z5", "largefreq_p_z5", "largefreq_pc_z5", echant_PC_0_6, echant_PC_6_12, echant_PC_12_24, echant_PC_p24)

afpa_T_z4$mest_z4   <- 0
afpa_P_z4$mesp_z4   <- 0
afpa_PC_z4$mespc_z4 <- 0

afpa_T_z5$mest_z5   <- 0
afpa_P_z5$mesp_z5   <- 0
afpa_PC_z5$mespc_z5 <- 0


# Indice de selection d'enfants dans les deux premieres année de vie
# ------------------------------------------------------------------

afpa_z019 <- merge(x=afpa_z019, y=afpa_T_z4[,c(1:8,9,49)],    by= c("pedid","age_jour","age_mois","age_annee","c_age_jour","c_age_mois","c_age_annee","sexe","taille"),  all.x=T)
afpa_z019 <- merge(x=afpa_z019, y=afpa_P_z4[,c(1:8,10,49)],   by= c("pedid","age_jour","age_mois","age_annee","c_age_jour","c_age_mois","c_age_annee","sexe","poids"),   all.x=T)
afpa_z019 <- merge(x=afpa_z019, y=afpa_PC_z4[,c(1:8,11,49)],  by= c("pedid","age_jour","age_mois","age_annee","c_age_jour","c_age_mois","c_age_annee","sexe","pc"),      all.x=T)

afpa_z019 <- merge(x=afpa_z019, y=afpa_T_z5[,c(1:8,9,49)],    by= c("pedid","age_jour","age_mois","age_annee","c_age_jour","c_age_mois","c_age_annee","sexe","taille"),  all.x=T)
afpa_z019 <- merge(x=afpa_z019, y=afpa_P_z5[,c(1:8,10,49)],   by= c("pedid","age_jour","age_mois","age_annee","c_age_jour","c_age_mois","c_age_annee","sexe","poids"),   all.x=T)
afpa_z019 <- merge(x=afpa_z019, y=afpa_PC_z5[,c(1:8,11,49)],  by= c("pedid","age_jour","age_mois","age_annee","c_age_jour","c_age_mois","c_age_annee","sexe","pc"),      all.x=T)

afpa_z019$nb_mesure_t_z4   <- as.factor(ifelse(is.na(afpa_z019$taille)==F & is.na(afpa_z019$mest_z4)==T,  1,0))
afpa_z019$nb_mesure_p_z4   <- as.factor(ifelse(is.na(afpa_z019$poids)==F  & is.na(afpa_z019$mesp_z4)==T,  1,0))
afpa_z019$nb_mesure_pc_z4  <- as.factor(ifelse(is.na(afpa_z019$pc)==F     & is.na(afpa_z019$mespc_z4)==T, 1,0))

afpa_z019$nb_mesure_t_z5   <- as.factor(ifelse(is.na(afpa_z019$taille)==F & is.na(afpa_z019$mest_z5)==T,  1,0))
afpa_z019$nb_mesure_p_z5   <- as.factor(ifelse(is.na(afpa_z019$poids)==F  & is.na(afpa_z019$mesp_z5)==T,  1,0))
afpa_z019$nb_mesure_pc_z5  <- as.factor(ifelse(is.na(afpa_z019$pc)==F     & is.na(afpa_z019$mespc_z5)==T, 1,0))

afpa_z019 <- afpa_z019[!duplicated(afpa_z019),]
afpa_z019 <- subset(afpa_z019,select=-c(mest_z4, mesp_z4, mespc_z4, mest_z5, mesp_z5, mespc_z5))


# Sauvegarde des bases
######################
save(afpa_z019, file=paste0(WD,"/Donnees/AFPA/AFPA_32/Bases sauvegardees/afpa_z019.rda"))




# Supprimer des tables inutiles
###############################
rm(acanthro, bfawho2007, IMCanthro, hcanthro, hfawho2007, lenanthro, matprev, matz, ssanthro, tsanthro, weianthro, wfawho2007, wfhanthro, 
   wflanthro, g_p_z4, g_p_z5, g_t_z4, g_t_z5, afpa_05, afpa_519, drees_05, drees_519, 
   afpa_T_z5, afpa_P_z5, afpa_PC_z5, id1,id2,id3,id4,id5,id5,id6,id7,id7,id8,id9)
