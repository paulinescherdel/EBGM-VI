##################################################
# Macro programme nettoyage simplifie des doublons
##################################################
  
  
  # Librairies des programmes
  ###########################
  library(boot)
  library(pastecs)
  library(doBy)
  library(tidyr)
  library(dplyr)
  library(data.table)
  library(psych)
  library(prettyR)
  library(descr)
  library(questionr)
  library(Hmisc)
  library(ggplot2)
  
  options(scipen=100, digits=3)
  
  
  # Chemin des sorties
  ####################
   WD_d <- "D:/INSERM - PARIS/EBGM VI - construction courbes/Resultats/AFPA/AFPA_32/Nettoyage_doublons/"
  
  
  # Tables de depart (pedid, age_jour, la mesure (taille/pds/pc), le z-score(z_t_oms...) et l'indicatrice du z-score (z_t_oms_z4...))
  ###################################################################################################################################
  
  # z_score 5
  ###########
  # Poids
  tab_p_z5 <- subset(afpa_z019,select=c(pedid,age_jour,poids,z_p_oms,z_p_oms_z5))
  tab_p_z5 <- na.omit(tab_p_z5)
  
  # Taille
  tab_t_z5 <- subset(afpa_z019,select=c(pedid,age_jour,taille,z_t_oms,z_t_oms_z5))
  tab_t_z5 <- na.omit(tab_t_z5) 
  
  # PC
  tab_pc_z5 <- subset(afpa_z019,select=c(pedid,age_jour,pc,z_pc_oms,z_pc_oms_z5))
  tab_pc_z5 <- na.omit(tab_pc_z5)
  
  
  # z_score 4
  ###########
  # Poids
  tab_p_z4 <- subset(afpa_z019,select=c(pedid,age_jour,poids,z_p_oms,z_p_oms_z4))
  tab_p_z4 <- na.omit(tab_p_z4) 
  
  # Taille
  tab_t_z4 <- subset(afpa_z019,select=c(pedid,age_jour,taille,z_t_oms,z_t_oms_z4))
  tab_t_z4 <- na.omit(tab_t_z4)
  
  # PC
  tab_pc_z4 <- subset(afpa_z019,select=c(pedid,age_jour,pc,z_pc_oms,z_pc_oms_z4))
  tab_pc_z4 <- na.omit(tab_pc_z4)
  
  
  
  
  # Macro 
  #######
  nettoyage_doublon <- function (entree, mesure, indz,  z, valeur)  
  {
    
  # Suppression des z-scores selon le seuil
    entree <- subset(entree, eval(parse(text=indz))==0)
    
  # Préparation de la table
  tab1 <- entree[!duplicated(entree[c("pedid","age_jour", mesure)]),]
  tab1 <- tab1[order(tab1[["pedid"]], tab1[["age_jour"]], tab1[[mesure]], decreasing=F), ]
  tab1 <- data.table(tab1)
  as.character(tab1$pedid)
  setkeyv(tab1, "pedid")
  

  # Identification des dates identiques répétées : lag + lead de 1 : pour creer un indicateur = 1 quand l'age est egal au precedent 0 sinon
  tab1[, age_p1  := shift(age_jour, n=1, fill=NA, type="lag"), by='pedid'] 
  tab1[, age_p_1 := shift(age_jour, n=1, fill=NA, type="lead"), by='pedid'] 
  
  # Indicatrice de doublon
  tab1$indic_age_tot <- ifelse(tab1$age_jour==tab1$age_p1 | tab1$age_jour==tab1$age_p_1, 1, 0)
  
  # Suppression des variables inutiles
  tab1<-subset(tab1, select = -c(age_p1,age_p_1))
  
  
  # Selectionner les sujets pour lesquels il y a un problème de doublons
  ######################################################################
  tab1b <-tab1[tab1$indic_age_tot==1,]
  tab2  <-tab1[which(tab1$pedid %in% tab1b$pedid),]


  
  
  # Mesure chez les personnes avec deux enregistrements
  #####################################################
  tab2$indicateur<-1
  nb_mesure_id_2<-as.data.frame(table(tab2$pedid, tab2$indicateur))
  names(nb_mesure_id_2)[1]<-"pedid" 
  names(nb_mesure_id_2)[3]<-"nb_visite"
  nb_mesure_id_2<-nb_mesure_id_2[,-2]
  
  # Fusionner les deux bases
  tab2$pedid <-as.factor(tab2$pedid)
  tab2       <- tab2[order(tab2$pedid),]
  nb_mesure_id_2 <- nb_mesure_id_2[order(nb_mesure_id_2$pedid),]
  
  tab2 <- merge(tab2,nb_mesure_id_2, by.x="pedid", by.y = "pedid")
  
  
  # Selection aleatoire
  # -------------------
  tabm2 <- subset(tab2, nb_visite<=2)
  tabm2$unif <- runif(nrow(tabm2), 0,1)
  tabm2a <- tabm2 %>% group_by(pedid) %>% top_n(1, unif)
  tabm2a$indicateur_final_2m<-0
  tabm2a <- subset(tabm2a, select=-c(unif))
  tabm2b <- tabm2 %>% group_by(pedid) %>% top_n(-1, unif)
  tabm2b$indicateur_final_2m<-1
  tabm2b <- subset(tabm2b, select=-c(unif))
  tabp2 <- subset(tab2, nb_visite>2)
  tabp2$indicateur_final_2m<-0
  
  tab2 <- rbind(data.frame(tabm2a),data.frame(tabm2b),data.frame(tabp2))
 
  
  # Mesure chez les personnes > deux enregistrements
  ##################################################
  
  # Creation des indicateurs
  # -----------------------
  
  # Enlever les lignes pour lesquelles indicateur_final_2m=1
  tab3<-as.data.frame(subset(tab2, indicateur_final_2m==0))
  
    
  # Trier la table sur les 2 variables
  tab3 <- tab3[order(tab3$pedid, tab3$age_jour),]
  tab3 <- data.table(tab3)
  as.character(tab3$pedid)
  setkeyv(tab3, 'pedid')
  
  tab3$FIRST<-ifelse((!duplicated(tab3$pedid)),1,0)
  tab3$LAST <-ifelse(rev(!duplicated(rev(tab3$pedid))),1,0)
  
  tab3[, age_avant := shift(age_jour, n=1, fill=NA, type="lag"), by='pedid'] 
  tab3[, age_apres := shift(age_jour, n=1, fill=NA, type="lead"), by='pedid']
  tab3$indice_age <- ifelse((is.na(tab3$age_avant)==F & tab3$age_jour==tab3$age_avant) | (is.na(tab3$age_apres)==F & tab3$age_jour==tab3$age_apres), 1,0)
  tab4 <- tab3[(indice_age==1 & LAST==1)|(indice_age==1 & FIRST==1)]
  
    # Suppression des variables inutiles
    tab4 <- subset(tab4, select = -eval(parse(text=mesure)))
    tab4 <- subset(tab4, select = -eval(parse(text=z)))
    tab4 <- remove.vars(tab4, c("indic_age_tot","FIRST","LAST","age_avant","age_apres", "indicateur", "nb_visite",indz,"indicateur_final_2m"))
    tab4 <- merge(tab2, tab4, by=c("pedid","age_jour"), all.x=T)
  
    
  # Construction indicateur doublons 2 mesures
    tab4$indicateur_final_2m<- ifelse(is.na(tab4$indicateur_final_2m)==T | tab4$indicateur_final_2m==0, 0, 1)
    
  # Construction indicateur doublons externes
    tab4$indice_doublon_ext<- ifelse(is.na(tab4$indice_age)==T, 0, 1)
  

  # Construction indicateur doublons centraux
   tab4$indice_doublon_centr <- tab4$indic_age_tot-tab4$indice_doublon_ext
   tab4$indice_doublon_centr <- ifelse(is.na(tab4$indice_doublon_centr)==T, 0, tab4$indice_doublon_centr)
  
  
  # Suppression des variables inutiles
  tab4 <- remove.vars(tab4, c("indice_age"))
  #tab2 <- remove.vars(tab2, c("indic_age_tot", "indicateur", "nb_visite"))
  
  
  

  
  
  ##################################################################################################################################################
  #  ######################################################## Partie nettoyage  ####################################################################
  ##################################################################################################################################################
  
  ############################## 
  # CHEZ LES DOUBLONS EXTERNES #
  ##############################
  
tab5<-as.data.frame(subset(tab4, indicateur_final_2m==0))
tab5 <- tab5[order(tab5[["pedid"]], tab5[["age_jour"]], tab5[[mesure]], decreasing=F), ]
tab5$FIRST<-ifelse((!duplicated(tab5$pedid)),1,0) 
tab5$LAST <-ifelse(rev(!duplicated(rev(tab5$pedid))),1,0)
  
  ### Obtenir age precedent
  #########################
  
tab5 <- tab5[order(tab5[["pedid"]], tab5[["age_jour"]], tab5[[mesure]], decreasing=F), ]


  # Codage pour ensuite faire le LAG/LEAD
  tab5 <- data.table(tab5)
  as.character(tab5$pedid)
  setkeyv(tab5,'pedid')
  

  # Boucle qui initialise la variable compteur a 1 si c'est un des doublons et zero sinon
  for(i in 1:nrow(tab5)){
    if(tab5$indice_doublon_ext[i]==1){
       tab5$cpt[i]<-1}
  else{tab5$cpt[i]<-0}
  }
  
  # LAG et LEAD du compteur qui permettra de faire les actions suivantes selon chaque individu separement
  tab5[, `:=`(prev.val = c(NA, head(cpt, -1)), 
              next.val = c(tail(cpt, -1), NA)), by=pedid]
  
  tab5$prev.val <- replace(tab5$prev.val,is.na(tab5$prev.val), 0)
  tab5$next.val <- replace(tab5$next.val,is.na(tab5$next.val), 0)
  
  
  # Lag et LEAD de firt et last
  tab5[, `:=`(FIRST.prev.val = c(NA, head(FIRST, -1)), 
              FIRST.next.val = c(tail(FIRST, -1), NA)), by=pedid]
  
  tab5[, `:=`(LAST.prev.val = c(NA, head(LAST, -1)), 
              LAST.next.val = c(tail(LAST, -1), NA)), by=pedid]
  
  tab5$FIRST.prev.val <- replace(tab5$FIRST.prev.val,is.na(tab5$FIRST.prev.val), 0)
  tab5$FIRST.next.val <- replace(tab5$FIRST.next.val,is.na(tab5$FIRST.next.val), 0)
  
  tab5$LAST.prev.val <- replace(tab5$LAST.prev.val,is.na(tab5$LAST.prev.val), 0)
  tab5$LAST.next.val <- replace(tab5$LAST.next.val,is.na(tab5$LAST.next.val), 0)
  
  
  # Créer la variable qcomptage qui va s'incrémenter
  tab5$cpt_ens  <- tab5$cpt
  
  
  # Boucle qui incremente le compteur des qu il y a une ligne doublon qui suit
  for(i in 1:nrow(tab5)){
    if(tab5$cpt[i]==1 & tab5$prev.val[i]==1){
       tab5$cpt_ens[i]=tab5$cpt_ens[i] + tab5$cpt_ens[i-1]}
      }
 

  # Boucle pour l'age, la mesure et le z_score precedent
  boucle_val1 <- function(nom_variable, variable) {
    for(i in 1:nrow(tab5)){
      
      if(tab5$indice_doublon_ext[i]==1 & tab5$prev.val[i]==0 & tab5$next.val[i]==1 & tab5$FIRST[i]==1) {
         tab5[[nom_variable]][i] = NA} 
      
      else if(tab5$indice_doublon_ext[i]==1 & tab5$prev.val[i]==1 & tab5$next.val[i]==0 & tab5$FIRST.prev.val[i]==1) {
              tab5[[nom_variable]][i] = NA} 
     
      else if(tab5$indice_doublon_ext[i]==1 & tab5$prev.val[i]==1 & tab5$next.val[i]==1 & tab5$FIRST.prev.val[i]==1 & tab5$cpt_ens[i]>=2) {
              tab5[[nom_variable]][i] = NA} 
      
      else if(tab5$indice_doublon_ext[i]==1 & tab5$prev.val[i]==1 & tab5$next.val[i]==0 & tab5$FIRST.prev.val[i]==0 & tab5$cpt_ens[i]>=3) {
              tab5[[nom_variable]][i] = noquote(tab5[[nom_variable]][i-1])} 
      
      else if(tab5$indice_doublon_ext[i]==1 & tab5$FIRST.prev.val[i]==1 & tab5$cpt_ens[i]>=2 ) {
              tab5[[nom_variable]][i] = noquote(tab5[[variable]][i])} 
      
      else if(tab5$indice_doublon_ext[i]==1 & tab5$cpt_ens[i]>2) {
              tab5[[nom_variable]][i] = noquote(tab5[[variable]][i-tab5$cpt_ens[i]])} 
      
      else if(tab5$indice_doublon_ext[i]==1) {
              tab5[[nom_variable]][i] = noquote(tab5[[variable]][i-tab5$cpt_ens[i]])} 
      
      else {tab5[[nom_variable]][i] = NA}
    }
  
    return(tab5)
  }
  
  tab5<-boucle_val1("age_avant_ext", "age_jour")
  tab5<-boucle_val1("mesure_avant_ext", mesure)
  tab5<-boucle_val1("zscore_avant_ext", z)
  
  # Supprimer les colonnes inutiles
  tab5 <- remove.vars(tab5, c("cpt","prev.val","next.val","cpt_ens","FIRST.prev.val", "FIRST.next.val","LAST.prev.val","LAST.next.val"))
  
  
  
  
  ### Obtenir age suivant
  #######################
 
  tab5 <- tab5[order(tab5[["pedid"]], tab5[["age_jour"]], tab5[[mesure]], decreasing=T), ]
  
  # Codage pour ensuite faire le LAG/LEAD
  tab5 <- data.table(tab5)
  as.character(tab5$pedid)
  setkeyv(tab5,'pedid')
  
  # Boucle qui initialise la variable compteur a 1 si c'est un des doublons et zero sinon
  for(i in 1:nrow(tab5)){
    if(tab5$indice_doublon_ext[i]==1){
      tab5$cpt[i]<-1
    }else{
      tab5$cpt[i]<-0}
  }
  
  # LAG et LEAD du compteur qui permettra de faire les actions suivantes selon chaque individu separement
  tab5[, `:=`(prev.val = c(NA, head(cpt, -1)), 
                  next.val = c(tail(cpt, -1), NA)), by=pedid]
  
  tab5$prev.val <- replace(tab5$prev.val,is.na(tab5$prev.val), 0)
  tab5$next.val <- replace(tab5$next.val,is.na(tab5$next.val), 0)
  
  
  # LAG et LEAD de FIRST et LAST
  tab5[, `:=`(FIRST.prev.val = c(NA, head(FIRST, -1)), 
                  FIRST.next.val = c(tail(FIRST, -1), NA)), by=pedid]
  
  tab5[, `:=`(LAST.prev.val = c(NA, head(LAST, -1)), 
                  LAST.next.val = c(tail(LAST, -1), NA)), by=pedid]
  
  tab5$FIRST.prev.val <- replace(tab5$FIRST.prev.val,is.na(tab5$FIRST.prev.val), 0)
  tab5$FIRST.next.val <- replace(tab5$FIRST.next.val,is.na(tab5$FIRST.next.val), 0)
  
  tab5$LAST.prev.val <- replace(tab5$LAST.prev.val,is.na(tab5$LAST.prev.val), 0)
  tab5$LAST.next.val <- replace(tab5$LAST.next.val,is.na(tab5$LAST.next.val), 0)
  
  # Créer la variable qcomptage qui va s'incrémenter
  tab5$cpt_ens  <- tab5$cpt
  
  # Boucle qui incremente le compteur des qu il y a une ligne doublon qui suit
  for(i in 1:nrow(tab5)){
    if(tab5$cpt[i]==1 & tab5$prev.val[i]==1){
      tab5$cpt_ens[i]=tab5$cpt_ens[i] + tab5$cpt_ens[i-1]}
  }
  
  # Boucle pour l'age, la mesure et le z_score suivant
  boucle_val2 <- function(nom_variable, variable) {
    
    for(i in 1:nrow(tab5)){
      
      if(tab5$indice_doublon_ext[i]==1 & tab5$prev.val[i]==0 & tab5$next.val[i]==1 & tab5$LAST[i]==1) {
         tab5[[nom_variable]][i]= NA} 
      
      else if(tab5$indice_doublon_ext[i]==1 & tab5$prev.val[i]==1 & tab5$next.val[i]==0 & tab5$LAST.prev.val[i]==1) {
              tab5[[nom_variable]][i] = NA} 
      
      else if(tab5$indice_doublon_ext[i]==1 & tab5$prev.val[i]==1 & tab5$next.val[i]==1 & tab5$LAST.prev.val[i]==1 & tab5$cpt_ens[i]>=2) {
              tab5[[nom_variable]][i]= NA} 
      
      else if(tab5$indice_doublon_ext[i]==1 & tab5$prev.val[i]==1 & tab5$next.val[i]==0 & tab5$LAST.prev.val[i]==0 & tab5$cpt_ens[i]>=3) {
              tab5[[nom_variable]][i]= noquote(tab5[[nom_variable]][i-1])} 
      
      else if(tab5$indice_doublon_ext[i]==1 & tab5$LAST.prev.val[i]==1 & tab5$cpt_ens[i]>=2 ) {
              tab5[[nom_variable]][i]= noquote(tab5[[variable]][i])} 
      
      else if(tab5$indice_doublon_ext[i]==1 & tab5$cpt_ens[i]>2) {
              tab5[[nom_variable]][i]= noquote(tab5[[variable]][i-tab5$cpt_ens[i]])} 
      
      else if(tab5$indice_doublon_ext[i]==1) {
              tab5[[nom_variable]][i]= noquote(tab5[[variable]][i-tab5$cpt_ens[i]])} 
      
      else {tab5[[nom_variable]][i]=NA} 
    }
  
  return(tab5)
  }
  
  tab5<-boucle_val2("age_apres_ext", "age_jour")
  tab5<-boucle_val2("mesure_apres_ext", mesure)
  tab5<-boucle_val2("zscore_apres_ext", z)
  
  
  # Trier a nouveau la table dans le bon ordre
  tab5 <- tab5[order(tab5[["pedid"]], tab5[["age_jour"]], tab5[[mesure]], decreasing=F), ]
  
  # Table propre tab_d4 : Suppression de variables inutiles de tab4
  tab5 <- remove.vars(tab5, c("cpt","prev.val","next.val","cpt_ens", "FIRST.prev.val", "FIRST.next.val","LAST.prev.val","LAST.next.val"))
 

  
  
  ##################################################################
  # Calcul de la différence minimale absolue pour les deux mesures
  ##################################################################
  
  # Cas ou le doublon est la premiere mesure ou derniere mesure: soustraire les zscore (pas les poids) car besoin de tenir compte de l'âge
  for(i in 1:nrow(tab5)){
    
    if (tab5$indice_doublon_ext[i]==1 & (is.na(tab5$age_apres_ext[i]==T))){
        tab5$diff_zscore_ext[i] <- abs(tab5$zscore_avant_ext[i]-noquote(tab5[[z]])[i])}
    
    else if (tab5$indice_doublon_ext[i]==1 & (is.na(tab5$age_avant_ext[i]==F))){
             tab5$diff_zscore_ext[i] <- abs(tab5$zscore_apres_ext[i]-noquote(tab5[[z]])[i])}
    
    else {tab5$diff_zscore_ext[i] <-NA}
  }
  
 

  #  Calcul de la difference minimale entre les doublons
  Min_difference <-function(mesure_diff, sortie)
  {
    Tab_min <- as.data.frame(aggregate(abs(mesure_diff), by=list(tab5$pedid, tab5$age_jour), FUN=min))   
    Tab_min <- Tab_min[order(Tab_min$Group.1, Tab_min$Group.2, Tab_min$x), ]
    
    names(Tab_min)[1]<-"pedid" 
    names(Tab_min)[2]<-"age_jour"
    names(Tab_min)[3]<-"diff_zscore_ext"
    
    write.csv2(Tab_min, paste0(file=paste0(WD_d, sortie)), row.names=TRUE)
    Tab_min = na.omit(Tab_min)
    return(Tab_min)
  }
  
  Tab_min_z_ext <- Min_difference (tab5$diff_zscore_ext, "Min_valeur_difference_score_ext.csv")
 
  
  # Creation indicatrice qui donne : 1 = différence la plus petite et 0 = différences plus grandes
  tab5$ind1    <-0
  Tab_min_z_ext$ind2 <-1
  
  # Merge des deux tables pour pouvoir comparer les deux indicatrices
  tab5 <- tab5[order(tab5[["pedid"]], tab5[["age_jour"]]), ]
  Tab_min_z_ext <- Tab_min_z_ext[order(Tab_min_z_ext[["pedid"]], Tab_min_z_ext[["age_jour"]]), ]
  tab6 <- merge(tab5, Tab_min_z_ext, by.x=c("pedid","age_jour"), by.y = c("pedid","age_jour"), all.x = T) 
  
  # Creer un indicateur final pour la variable indic_doublon
  tab6$indicateur_final_ext<-0
  tab6$indicateur_final_ext[tab6$diff_zscore_ext.x == tab6$diff_zscore_ext.y]<-0  
  tab6$indicateur_final_ext[tab6$diff_zscore_ext.x != tab6$diff_zscore_ext.y]<-1   
  
  # Table finale: suppression des variables inutiles
  tab6 <- remove.vars(tab6, c("ind1","ind2","diff_zscore_ext.y","diff_zscore_ext.x", "FIRST", "LAST",
                              "mesure_avant_ext", "zscore_avant_ext", "age_avant_ext",
                              "mesure_apres_ext", "zscore_apres_ext", "age_apres_ext"))
  
  
  
  

  
  
  ##############################
  # CHEZ LES DOUBLONS CENTRAUX #
  ##############################
  
  tab7<-as.data.frame(subset(tab6, indicateur_final_2m==0 & indicateur_final_ext==0))
  tab7 <- tab7[order(tab7[["pedid"]], tab7[["age_jour"]], tab7[[mesure]], decreasing=F), ]
  tab7$FIRST<-ifelse((!duplicated(tab7$pedid)),1,0) 
  tab7$LAST <-ifelse(rev(!duplicated(rev(tab7$pedid))),1,0)
  
  ### Obtenir age precedent
  #########################
  tab7 <- tab7[order(tab7[["pedid"]], tab7[["age_jour"]], tab7[[mesure]], decreasing=F), ]
  
  # Codage pour ensuite faire le LAG
  tab7 <- data.table(tab7)
  as.character(tab7$pedid)
  setkeyv(tab7,'pedid')
  
  # Boucle qui initialise la variable compteur a 1 si c'est un des doublons et zero sinon
  for(i in 1:nrow(tab7)){
    if(tab7$indice_doublon_centr[i]==1){
       tab7$cpt[i]<-1}
    else{tab7$cpt[i]<-0}
  }
  
  # LAG et LEAD du compteur qui permettra de faire les actions suivantes selon chaque individu separement
  tab7[, `:=`(prev.val = c(NA, head(cpt, -1)), 
              next.val = c(tail(cpt, -1), NA)), by=pedid]
  
  tab7$prev.val <- replace(tab7$prev.val,is.na(tab7$prev.val), 0)
  tab7$next.val <- replace(tab7$next.val,is.na(tab7$next.val), 0)
  
  # Lag et LEAD de firt et last
  tab7[, `:=`(FIRST.prev.val = c(NA, head(FIRST, -1)), 
              FIRST.next.val = c(tail(FIRST, -1), NA)), by=pedid]
  
  tab7[, `:=`(LAST.prev.val = c(NA, head(LAST, -1)), 
              LAST.next.val = c(tail(LAST, -1), NA)), by=pedid]
  
  tab7$FIRST.prev.val <- replace(tab7$FIRST.prev.val,is.na(tab7$FIRST.prev.val), 0)
  tab7$FIRST.next.val <- replace(tab7$FIRST.next.val,is.na(tab7$FIRST.next.val), 0)
  tab7$LAST.prev.val  <- replace(tab7$LAST.prev.val,is.na(tab7$LAST.prev.val), 0)
  tab7$LAST.next.val  <- replace(tab7$LAST.next.val,is.na(tab7$LAST.next.val), 0)
  
  # Créer la variable qcomptage qui va s'incrémenter
  tab7$cpt_ens  <- tab7$cpt
  
  # Boucle qui incremente le compteur des qu il y a une ligne doublon qui suit
  for(i in 1:nrow(tab7)){
    if(tab7$cpt[i]==1 & tab7$prev.val[i]==1){
       tab7$cpt_ens[i]=tab7$cpt_ens[i] + tab7$cpt_ens[i-1]}
  }
  
  # Boucle qui va corriger les cas specifique de doublons (valeurs qui se suivent, doublons au premier et dernier enregistrements d une personne)
  boucle_val3 <- function(nom_variable, variable) {
    for(i in 1:nrow(tab7)){
      
      if(tab7$indice_doublon_centr[i]==1 & tab7$prev.val[i]==0 & tab7$next.val[i]==1 & tab7$FIRST[i]==1) {
         tab7[[nom_variable]][i] = NA} 
      
      else if(tab7$indice_doublon_centr[i]==1 & tab7$prev.val[i]==1 & tab7$next.val[i]==0 & tab7$FIRST.prev.val[i]==1) {
              tab7[[nom_variable]][i] = NA} 
      
      else if(tab7$indice_doublon_centr[i]==1 & tab7$prev.val[i]==1 & tab7$next.val[i]==1 & tab7$FIRST.prev.val[i]==1 & tab7$cpt_ens[i]>=2) {
              tab7[[nom_variable]][i] = NA} 
      
      else if(tab7$indice_doublon_centr[i]==1 & tab7$prev.val[i]==1 & tab7$next.val[i]==0 & tab7$FIRST.prev.val[i]==0 & tab7$cpt_ens[i]>=3) {
              tab7[[nom_variable]][i] = noquote(tab7[[nom_variable]][i-1])} 
      
      else if(tab7$indice_doublon_centr[i]==1 & tab7$FIRST.prev.val[i]==1 & tab7$cpt_ens[i]>=2 ) {
              tab7[[nom_variable]][i] = noquote(tab7[[variable]][i])} 
      
      else if(tab7$indice_doublon_centr[i]==1 & tab7$cpt_ens[i]>2) {
              tab7[[nom_variable]][i] = noquote(tab7[[variable]][i-tab7$cpt_ens[i]])} 
      
      else if(tab7$indice_doublon_centr[i]==1) {
              tab7[[nom_variable]][i] = noquote(tab7[[variable]][i-tab7$cpt_ens[i]])} 
      
      else {tab7[[nom_variable]][i] = NA}
    }
    
    return(tab7)
  }
  
  tab7<-boucle_val3("age_avant_centr", "age_jour")
  tab7<-boucle_val3("mesure_avant_centr", mesure)
  tab7<-boucle_val3("zscore_avant_centr", z)
  
  # Supprimer les colonnes inutiles
  tab7 <- remove.vars(tab7, c("cpt","prev.val","next.val","cpt_ens", "FIRST.prev.val", "FIRST.next.val","LAST.prev.val","LAST.next.val"))
  
  

  
  
  #### Obtenir age suivant
  ########################
  
  tab7 <- tab7[order(tab7[["pedid"]], tab7[["age_jour"]], tab7[[mesure]], decreasing=T), ]
  
  # Codage pour ensuite faire le LAG/LEAD
  tab7 <- data.table(tab7)
  as.character(tab7$pedid)
  setkeyv(tab7,'pedid')
  
  # Boucle qui initialise la variable compteur a 1 si c'est un des doublons et zero sinon
  for(i in 1:nrow(tab7)){
    if(tab7$indice_doublon_centr[i]==1){
       tab7$cpt[i]<-1}
    else{tab7$cpt[i]<-0}
  }
  
  # LAG et LEAD du compteur qui permettra de faire les actions suivantes selon chaque individu separement
  tab7[, `:=`(prev.val = c(NA, head(cpt, -1)), 
              next.val = c(tail(cpt, -1), NA)), by=pedid]
  
  tab7$prev.val <- replace(tab7$prev.val,is.na(tab7$prev.val), 0)
  tab7$next.val <- replace(tab7$next.val,is.na(tab7$next.val), 0)
  
  # Lag et LEAD de firt et last
  tab7[, `:=`(FIRST.prev.val = c(NA, head(FIRST, -1)), 
              FIRST.next.val = c(tail(FIRST, -1), NA)), by=pedid]
  
  tab7[, `:=`(LAST.prev.val = c(NA, head(LAST, -1)), 
              LAST.next.val = c(tail(LAST, -1), NA)), by=pedid]
  
  tab7$FIRST.prev.val <- replace(tab7$FIRST.prev.val,is.na(tab7$FIRST.prev.val), 0)
  tab7$FIRST.next.val <- replace(tab7$FIRST.next.val,is.na(tab7$FIRST.next.val), 0)
  tab7$LAST.prev.val  <- replace(tab7$LAST.prev.val,is.na(tab7$LAST.prev.val), 0)
  tab7$LAST.next.val  <- replace(tab7$LAST.next.val,is.na(tab7$LAST.next.val), 0)
  
  # Créer la variable qcomptage qui va s'incrémenter
  tab7$cpt_ens  <- tab7$cpt
  
  # Boucle qui incremente le compteur des qu il y a une ligne doublon qui suit
  for(i in 1:nrow(tab7)){
    if(tab7$cpt[i]==1 & tab7$prev.val[i]==1){
       tab7$cpt_ens[i]=tab7$cpt_ens[i] + tab7$cpt_ens[i-1]}
  }
  
  # Boucle pour l'age, la mesure et le z_score suivant
  boucle_val4 <- function(nom_variable, variable) {
    
    for(i in 1:nrow(tab7)){
      
      if(tab7$indice_doublon_centr[i]==1 & tab7$prev.val[i]==0 & tab7$next.val[i]==1 & tab7$LAST[i]==1) {
        tab7[[nom_variable]][i]= NA} 
      
      else if(tab7$indice_doublon_centr[i]==1 & tab7$prev.val[i]==1 & tab7$next.val[i]==0 & tab7$LAST.prev.val[i]==1) {
        tab7[[nom_variable]][i] = NA} 
      
      else if(tab7$indice_doublon_centr[i]==1 & tab7$prev.val[i]==1 & tab7$next.val[i]==1 & tab7$LAST.prev.val[i]==1 & tab7$cpt_ens[i]>=2) {
        tab7[[nom_variable]][i]= NA} 
      
      else if(tab7$indice_doublon_centr[i]==1 & tab7$prev.val[i]==1 & tab7$next.val[i]==0 & tab7$LAST.prev.val[i]==0 & tab7$cpt_ens[i]>=3) {
        tab7[[nom_variable]][i]= noquote(tab7[[nom_variable]][i-1])} 
      
      else if(tab7$indice_doublon_centr[i]==1 & tab7$LAST.prev.val[i]==1 & tab7$cpt_ens[i]>=2 ) {
        tab7[[nom_variable]][i]= noquote(tab7[[variable]][i])} 
      
      else if(tab7$indice_doublon_centr[i]==1 & tab7$cpt_ens[i]>2) {
        tab7[[nom_variable]][i]= noquote(tab7[[variable]][i-tab7$cpt_ens[i]])} 
      
      else if(tab7$indice_doublon_centr[i]==1) {
        tab7[[nom_variable]][i]= noquote(tab7[[variable]][i-tab7$cpt_ens[i]])} 
      
      else {tab7[[nom_variable]][i]=NA} 
    }
    
    return(tab7)
  }
  
  tab7<-boucle_val4("age_apres_centr", "age_jour")
  tab7<-boucle_val4("mesure_apres_centr", mesure)
  tab7<-boucle_val4("zscore_apres_centr", z)
  
  # Trier a nouveau la table dans le bon ordre
  tab7 <- tab7[order(tab7[["pedid"]], tab7[["age_jour"]], tab7[[mesure]], decreasing=F), ]
  
  # Table propre tab5 : Suppression de variables inutiles de tab7
  tab7 <- remove.vars(tab7, c("cpt","prev.val","next.val","cpt_ens", "FIRST.prev.val", "FIRST.next.val","LAST.prev.val","LAST.next.val"))
                              
 
  
  #####################################
  # Calcul de l interpolation lineaire
  #####################################
  
  # Calcul
  tab7$interp_mesure  <- round((((tab7$mesure_apres_centr-tab7$mesure_avant_centr)/(tab7$age_apres_centr-tab7$age_avant_centr))*(tab7$age_jour-tab7$age_avant_centr)+tab7$mesure_avant_centr), 2)
  tab7$diff_mesure    <- round(abs(tab7$interp_mesure-(noquote(tab7[[mesure]]))), 2)
  
 
  #  Calcul de la difference minimale entre les doublons
  Min_difference <-function(mesure_diff, sortie)
  {
    Tab_min <- as.data.frame(aggregate(abs(mesure_diff), by=list(tab7$pedid, tab7$age_jour), FUN=min))
    Tab_min <- Tab_min[order(Tab_min$Group.1, Tab_min$Group.2, Tab_min$x),]
    
    names(Tab_min)[1]<-"pedid" 
    names(Tab_min)[2]<-"age_jour"
    names(Tab_min)[3]<-"diff_mesure"
    
    write.csv2(Tab_min,paste0(file=paste0(WD_d, sortie)), row.names=TRUE)
    Tab_min = na.omit(Tab_min)
    return(Tab_min)
  }
  Tab_min_p_centr <- Min_difference (tab7$diff_mesure, "Min_valeur_difference_score_centr.csv")
  
  # Poursuite pour indicatrice plus petit doublon
  tab7$ind1    <-0
  Tab_min_p_centr$ind2 <-1
  
  # Merge des deux tables pour pouvoir comparer les deux indicatrices
  tab7 <- tab7[order(tab7[["pedid"]], tab7[["age_jour"]]), ]
  Tab_min_p_centr <- Tab_min_p_centr[order(Tab_min_p_centr$pedid, Tab_min_p_centr$age_jour), ]
  tab8 <- merge(tab7, Tab_min_p_centr, by.x=c("pedid","age_jour"), by.y = c("pedid","age_jour"), all.x = T) 
  
  
  
  tab8 <- tab8[order(tab8[["pedid"]], tab8[["age_jour"]], tab8[[mesure]], decreasing=T), ]
  
  
  # Codage pour ensuite faire le LAG/LEAD
  tab8 <- data.table(tab8)
  as.character(tab8$pedid)
  setkeyv(tab8,'pedid')
  
  # Boucle qui initialise la variable compteur a 1 si c'est un des doublons et zero sinon
  for(i in 1:nrow(tab8)){
    if(tab8$indice_doublon_centr[i]==1){
      tab8$cpt[i]<-1}
    else{tab8$cpt[i]<-0}
  }
  
  # LAG et LEAD du compteur qui permettra de faire les actions suivantes selon chaque individu separement
  tab8[, `:=`(prev.val = c(NA, head(cpt, -1)), 
              next.val = c(tail(cpt, -1), NA)), by=pedid]
  
  tab8$prev.val <- replace(tab8$prev.val,is.na(tab8$prev.val), 0)
  tab8$next.val <- replace(tab8$next.val,is.na(tab8$next.val), 0)
  
  # Lag et LEAD de firt et last
  tab8[, `:=`(FIRST.prev.val = c(NA, head(FIRST, -1)), 
              FIRST.next.val = c(tail(FIRST, -1), NA)), by=pedid]
  
  tab8[, `:=`(LAST.prev.val = c(NA, head(LAST, -1)), 
              LAST.next.val = c(tail(LAST, -1), NA)), by=pedid]
  
  tab8$FIRST.prev.val <- replace(tab8$FIRST.prev.val,is.na(tab8$FIRST.prev.val), 0)
  tab8$FIRST.next.val <- replace(tab8$FIRST.next.val,is.na(tab8$FIRST.next.val), 0)
  tab8$LAST.prev.val  <- replace(tab8$LAST.prev.val,is.na(tab8$LAST.prev.val), 0)
  tab8$LAST.next.val  <- replace(tab8$LAST.next.val,is.na(tab8$LAST.next.val), 0)
  
  # Créer la variable qcomptage qui va s'incrémenter
  tab8$cpt_ens  <- tab8$cpt
  
  # Boucle qui incremente le compteur des qu il y a une ligne doublon qui suit
  for(i in 1:nrow(tab8)){
    if(tab8$cpt[i]==1 & tab8$prev.val[i]==1){
      tab8$cpt_ens[i]=tab8$cpt_ens[i] + tab8$cpt_ens[i-1]}
  }
  tab8 <- tab8[order(tab8[["pedid"]], tab8[["age_jour"]], tab8[[mesure]], decreasing=F), ]

  
  # Creer un indicateur final pour la variable indic_doublon
  tab8$indicateur_final_centr<-0
  tab8$indicateur_final_centr[tab8$diff_mesure.x == tab8$diff_mesure.y & tab8$cpt_ens==1]<-1
  tab8$indicateur_final_centr[tab8$diff_mesure.x == tab8$diff_mesure.y & tab8$cpt_ens>=2]<-0
  tab8$indicateur_final_centr[tab8$diff_mesure.x != tab8$diff_mesure.y]<-1
  
  
  # Table propre tab5 : Suppression de variables inutiles de tab8
  tab8 <- remove.vars(tab8, c("cpt","prev.val","next.val","cpt_ens", "FIRST.prev.val", "FIRST.next.val","LAST.prev.val","LAST.next.val"))
  
 
  # Table finale
  tab8 <- remove.vars(tab8, c("interp_mesure", "diff_mesure.x", "diff_mesure.y", "ind1", "ind2", "FIRST", "LAST",
                              "mesure_avant_centr", "zscore_avant_centr", "age_avant_centr",  
                              "mesure_apres_centr", "zscore_apres_centr", "age_apres_centr"))
  

  
  ####################################################################################################################
  ####################################################################################################################
  
  # Rassembler toutes les lignes + mise en forme 
  ##############################################
  
  # Table des doublons avec 2 mesures "exclus"
  tab9<-as.data.frame(subset(tab2, indicateur_final_2m==1))
  tab9$indicateur_final_ext<-0
  tab9$indicateur_final_centr<-0
  tab9$indice_doublon_centr<-0
  tab9$indice_doublon_ext<-0
  
  # Table des doublons externes "exclus"
  tab10<-as.data.frame(subset(tab6, indicateur_final_ext==1))
  tab10$indicateur_final_centr<-0
  

  tab8  <- tab8[order(tab8[["pedid"]],   tab8[["age_jour"]]), ]
  tab9  <- tab9[order(tab9[["pedid"]],   tab9[["age_jour"]]), ]
  tab10 <- tab10[order(tab10[["pedid"]], tab10[["age_jour"]]), ]

  tab_doub_final_mesure<-rbind(tab8,tab9,tab10)
  
  tab_doub_final_mesure[,noquote(paste0("indicateur_doublon", "_", mesure, valeur))] <- tab_doub_final_mesure$indicateur_final_2m + tab_doub_final_mesure$indicateur_final_centr + tab_doub_final_mesure$indicateur_final_ext
  

  tab_doub_final_mesure<-subset(tab_doub_final_mesure, select = -eval(parse(text=z)))
  tab_doub_final_mesure <- remove.vars(tab_doub_final_mesure, c("indicateur_final_2m","indicateur_final_ext", "indicateur_final_centr", "indice_doublon_ext", "indice_doublon_centr"))
  
  
  tab_doub_final_mesure <- tab_doub_final_mesure[order(tab_doub_final_mesure[["pedid"]], tab_doub_final_mesure[["age_jour"]], tab_doub_final_mesure[[mesure]]),]
  afpa_z019             <- afpa_z019[order(afpa_z019[["pedid"]], afpa_z019[["age_jour"]], afpa_z019[[mesure]]),]
  
  return(tab_doub_final_mesure)
  } 
  
  # Application macro : taille/poids/pc et z4/z5
  tab_doub_final_p_z5<-nettoyage_doublon (tab_p_z5, "poids", "z_p_oms_z5",  "z_p_oms", "_z5")
  tab_doub_final_p_z4<-nettoyage_doublon (tab_p_z4, "poids", "z_p_oms_z4",  "z_p_oms", "_z4")
  
  tab_doub_final_t_z5<-nettoyage_doublon (tab_t_z5, "taille", "z_t_oms_z5", "z_t_oms", "_z5")
  tab_doub_final_t_z4<-nettoyage_doublon (tab_t_z4, "taille", "z_t_oms_z4",  "z_t_oms", "_z4")
  
  tab_doub_final_pc_z5<-nettoyage_doublon (tab_pc_z5, "pc", "z_pc_oms_z5",  "z_pc_oms", "_z5")
  tab_doub_final_pc_z4<-nettoyage_doublon (tab_pc_z4, "pc", "z_pc_oms_z4",  "z_pc_oms", "_z4")
  
  
  
  ###############################################################################################################################
  ###############################################################################################################################
  # Fusionner les 4 bases
  
  #### Poids
  # Z5
  afpa_z019 <- merge(afpa_z019, tab_doub_final_p_z5[,c("pedid", "age_jour", "poids","z_p_oms_z5","indicateur_doublon_poids_z5")], by=c("pedid", "age_jour", "poids","z_p_oms_z5"), all.x=T)
  # Z4
  afpa_z019 <- merge(afpa_z019, tab_doub_final_p_z4[,c("pedid", "age_jour", "poids","z_p_oms_z4","indicateur_doublon_poids_z4")], by=c("pedid", "age_jour", "poids","z_p_oms_z4"), all.x=T)
  
  
  #### Taille
  # Z5
  afpa_z019 <- merge(afpa_z019, tab_doub_final_t_z5[,c("pedid", "age_jour", "taille","z_t_oms_z5","indicateur_doublon_taille_z5")], by=c("pedid", "age_jour", "taille","z_t_oms_z5"), all.x=T)
  # Z4
  afpa_z019 <- merge(afpa_z019, tab_doub_final_t_z4[,c("pedid", "age_jour", "taille","z_t_oms_z4","indicateur_doublon_taille_z4")], by=c("pedid", "age_jour", "taille","z_t_oms_z4"), all.x=T)
  
  
  #### PC
  # Z5
  afpa_z019 <- merge(afpa_z019, tab_doub_final_pc_z5[,c("pedid", "age_jour", "pc","z_pc_oms_z5","indicateur_doublon_pc_z5")], by=c("pedid", "age_jour", "pc","z_pc_oms_z5"), all.x=T)
  # Z4
  afpa_z019 <- merge(afpa_z019, tab_doub_final_pc_z4[,c("pedid", "age_jour", "pc","z_pc_oms_z4","indicateur_doublon_pc_z4")], by=c("pedid", "age_jour", "pc","z_pc_oms_z4"), all.x=T)
  
  
  
  # Creer les indicatrices de doublon de taille, poids et pc
  afpa_z019$doublon_p_z5  <- as.factor(ifelse(is.na(afpa_z019$indicateur_doublon_poids_z5)==T | afpa_z019$indicateur_doublon_poids_z5==0, 0, 1))
  afpa_z019$doublon_p_z4  <- as.factor(ifelse(is.na(afpa_z019$indicateur_doublon_poids_z4)==T | afpa_z019$indicateur_doublon_poids_z4==0, 0, 1))
  
  afpa_z019$doublon_t_z5  <- as.factor(ifelse(is.na(afpa_z019$indicateur_doublon_taille_z5)==T  |  afpa_z019$indicateur_doublon_taille_z5==0, 0, 1))
  afpa_z019$doublon_t_z4  <- as.factor(ifelse(is.na(afpa_z019$indicateur_doublon_taille_z4)==T  |  afpa_z019$indicateur_doublon_taille_z4==0, 0, 1))
  
  afpa_z019$doublon_pc_z5  <- as.factor(ifelse(is.na(afpa_z019$indicateur_doublon_pc_z5)==T  |  afpa_z019$indicateur_doublon_pc_z5==0, 0, 1))
  afpa_z019$doublon_pc_z4  <- as.factor(ifelse(is.na(afpa_z019$indicateur_doublon_pc_z4)==T  |  afpa_z019$indicateur_doublon_pc_z4==0, 0, 1))
  
  
  
  # Supprimer les 6 variables inutiles
  afpa_z019 <- remove.vars(afpa_z019, c("indicateur_doublon_poids_z5", "indicateur_doublon_poids_z4", "indicateur_doublon_taille_z5", "indicateur_doublon_taille_z4", "indicateur_doublon_pc_z5", "indicateur_doublon_pc_z4"))
  
  # Sauver la base
  save(afpa_z019, file=paste0(WD,"/Donnees/AFPA/AFPA_40/Bases sauvegardees/afpa_z019_indic_doublon_macro.rda"))
 
  # Suppression des bases inutiles
  rm(tab_doub_final_p_z4, tab_doub_final_p_z5, tab_doub_final_t_z4, tab_doub_final_t_z5, tab_doub_final_pc_z4, tab_doub_final_pc_z5, tab_p_z4, tab_p_z5, tab_t_z4, tab_t_z5, tab_pc_z4, tab_pc_z5)
  