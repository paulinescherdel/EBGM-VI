############################################################
# Modelisation de la taille chez les garcons de 0 - 18 ans #
############################################################

# Creation d un age directement mis a la puissance lambda (estimation of 0.65 in a subsample)
Taille_G_0_18_pop$age_jour2 <- Taille_G_0_18_pop$age_jour^0.65


# Modele 0 : application de pb de mu et d'une lineaire sur sigma
################################################################
M0_Taille_G_1990       <- gamlss(data=Taille_G_0_18_pop,
                                 family="BCPE",
                                 taille~pb(age_jour2, df = 10 , inter = 10),                               
                                 
                                 # Fonction de lissage
                                 sigma.formula=~age_jour2,             
                                 nu.formula=~1,                   
                                 tau.formula=~1,                                 
                                 
                                 # Fonction de lien
                                 mu.link=identity,
                                 sigma.link=log,
                                 nu.link=identity,
                                 tau.link=log,
                                 
                                 # Fixation des valeurs initiales
                                 mu.fix=F,
                                 sigma.fix=F,
                                 nu.fix=T,
                                 tau.fix=T,
                                 
                                 # Valeurs initiales               
                                 mu.start=1,
                                 sigma.start=1,
                                 nu.start=1,
                                 tau.start=2,
                                 
                                 # Methode de maximation de vraisemblances penalisees (RS ou CG)               
                                 method=RS(100)               
) 
M0_Taille_G_1990 


# Modele 1 : application de pb de sigma
#######################################
M1_Taille_G_1990       <- gamlss(data=Taille_G_0_18_pop,
                                 family="BCPE",
                                 taille~pb(age_jour2 , df=10 , inter = 10),                                
                                 
                                 # Fonction de lissage
                                 sigma.formula=~pb(age_jour2 , df = 10 , inter = 10),             
                                 nu.formula=~1,                   
                                 tau.formula=~1,                                 
                                 
                                 # Fonction de lien
                                 mu.link=identity,
                                 sigma.link=log,
                                 nu.link=identity,
                                 tau.link=log,
                                 
                                 # Fixation des valeurs initiales
                                 mu.fix=F,
                                 sigma.fix=F,
                                 nu.fix=T,
                                 tau.fix=T,
                                 
                                 # Valeurs initiales               
                                 start.from = M0_Taille_G_1990,
                                 
                                 # Methode de maximation de vraisemblances penalisees (RS ou CG)               
                                 method=RS(100)               
) 
M1_Taille_G_1990 



# Modele 2 : variation df/inter de mu 
#####################################
M2_Taille_G_1990_function <- function(inter)
{
  
  M2_Taille_G_1990       <- gamlss(data=Taille_G_0_18_pop,
                                   family="BCPE",
                                   taille~pb(age_jour2 , df= inter , inter = inter),                                
                                   
                                   # Fonction de lissage
                                   sigma.formula=~pb(age_jour2 , df = 10 , inter = 10),             
                                   nu.formula=~1,                   
                                   tau.formula=~1,                                 
                                   
                                   # Fonction de lien
                                   mu.link=identity,
                                   sigma.link=log,
                                   nu.link=identity,
                                   tau.link=log,
                                   
                                   # Fixation des valeurs initiales
                                   mu.fix=F,
                                   sigma.fix=F,
                                   nu.fix=T,
                                   tau.fix=T,
                                   
                                   # Valeurs initiales               
                                   start.from = M1_Taille_G_1990,
                                   
                                   # Methode de maximation de vraisemblances penalisees (RS ou CG)               
                                   method=RS(100)               
  ) 
  return(M2_Taille_G_1990)
  
}
for (i in seq(12,20,by = 2))
{
  assign(paste0("Mod_M2_Taille_G_1990_",i), M2_Taille_G_1990_function(inter=i))
}

if (Mod_M2_Taille_G_1990_14$G.deviance > Mod_M2_Taille_G_1990_12$G.deviance)  {d2 = 12} else if 
(Mod_M2_Taille_G_1990_16$G.deviance > Mod_M2_Taille_G_1990_14$G.deviance) {d2 = 14} else if 
(Mod_M2_Taille_G_1990_18$G.deviance > Mod_M2_Taille_G_1990_16$G.deviance) {d2 = 16} else if
(Mod_M2_Taille_G_1990_20$G.deviance > Mod_M2_Taille_G_1990_18$G.deviance) {d2 = 18} else {d2 = 20}

start2 <- eval(parse(text=paste0("Mod_M2_Taille_G_1990_",d2)))

# Supression modeles intermediaires inutiles
remove(Mod_M2_Taille_G_1990_12)
remove(Mod_M2_Taille_G_1990_14)
remove(Mod_M2_Taille_G_1990_16)
remove(Mod_M2_Taille_G_1990_18)
remove(Mod_M2_Taille_G_1990_20)



# Modele 3 : fixation des df/inter de mu et variation df/inter de sigma 
#######################################################################
M3_Taille_G_1990_function <- function(inter)
{
  M3_Taille_G_1990       <- gamlss(data=Taille_G_0_18_pop,
                                   family="BCPE",
                                   taille~pb(age_jour2 , df=d2 , inter = d2),                                
                                   
                                   # Fonction de lissage
                                   sigma.formula=~pb(age_jour2 , df = inter , inter = inter),             
                                   nu.formula=~1,                   
                                   tau.formula=~1,                                 
                                   
                                   # Fonction de lien
                                   mu.link=identity,
                                   sigma.link=log,
                                   nu.link=identity,
                                   tau.link=log,
                                   
                                   # Fixation des valeurs initiales
                                   mu.fix=F,
                                   sigma.fix=F,
                                   nu.fix=T,
                                   tau.fix=T,
                                   
                                   # Valeurs initiales               
                                   start.from = start2,
                                   
                                   # Methode de maximation de vraisemblances penalisees (RS ou CG)               
                                   method=RS(100)               
  ) 
  return(M3_Taille_G_1990)
  
}
for (i in seq(10,20,by = 2))
{
  assign(paste0("Mod_M3_Taille_G_1990_",i), M3_Taille_G_1990_function(inter=i))
}

if (Mod_M3_Taille_G_1990_12$G.deviance > Mod_M3_Taille_G_1990_10$G.deviance)  {d3 = 10} else if 
(Mod_M3_Taille_G_1990_14$G.deviance > Mod_M3_Taille_G_1990_12$G.deviance) {d3 = 12} else if
(Mod_M3_Taille_G_1990_16$G.deviance > Mod_M3_Taille_G_1990_14$G.deviance) {d3 = 14} else if
(Mod_M3_Taille_G_1990_18$G.deviance > Mod_M3_Taille_G_1990_16$G.deviance) {d3 = 16} else if
(Mod_M3_Taille_G_1990_20$G.deviance > Mod_M3_Taille_G_1990_18$G.deviance) {d3 = 18} else {d3 = 20}

start3 <- eval(parse(text=paste0("Mod_M3_Taille_G_1990_",d3)))

# Supression modeles intermediaires inutiles
remove(Mod_M3_Taille_G_1990_10)
remove(Mod_M3_Taille_G_1990_12)
remove(Mod_M3_Taille_G_1990_14)
remove(Mod_M3_Taille_G_1990_16)
remove(Mod_M3_Taille_G_1990_18)
remove(Mod_M3_Taille_G_1990_20)


# Modele 5 : fixation des df/inter de mu et sigma et variation df/inter de tau
##############################################################################
M5_Taille_G_1990_function <- function(inter)
{
  M5_Taille_G_1990                         <-gamlss(data=Taille_G_0_18_pop,
                                                    family="BCPE",
                                                    taille~pb(age_jour^0.65, df = d2 ,  inter= d2), 
                                                    
                                                    # Fonction de lissage                   
                                                    sigma.formula=~pb(age_jour^0.65, df = d3 , inter= d3),  #10       
                                                    nu.formula=~1,                                
                                                    tau.formula=~pb(age_jour^0.65, df = inter ,  inter=inter),                               
                                                    
                                                    # Fonction de lien                   
                                                    mu.link=identity,
                                                    sigma.link=log,
                                                    nu.link=identity,
                                                    tau.link=log,
                                                    
                                                    # Fixation des valeurs initiales                   
                                                    mu.fix=F,
                                                    sigma.fix=F,
                                                    nu.fix=T,
                                                    tau.fix=F,
                                                    
                                                    # Valeurs initiales
                                                    start.from = start3, 
                                                    
                                                    # Methode de maximation de vraisemblances penalisees (RS ou CG)                   
                                                    method=RS(100)                   
  ) 
  return(M5_Taille_G_1990)
  
}
for (i in seq(4,8,by = 1))
{
  assign(paste0("Mod_M5_Taille_G_1990_",i), M5_Taille_G_1990_function(inter=i))
}

if (Mod_M5_Taille_G_1990_5$G.deviance  > Mod_M5_Taille_G_1990_4$G.deviance) {d5 = 4 } else if 
(Mod_M5_Taille_G_1990_6$G.deviance  > Mod_M5_Taille_G_1990_5$G.deviance) {d5 = 5 } else if 
(Mod_M5_Taille_G_1990_7$G.deviance  > Mod_M5_Taille_G_1990_6$G.deviance) {d5 = 6 } else if 
(Mod_M5_Taille_G_1990_8$G.deviance  > Mod_M5_Taille_G_1990_7$G.deviance) {d5 = 7 } else {d5 = 8}

start5 <- eval(parse(text=paste0("Mod_M5_Taille_G_1990_",d5)))

# Supression modeles intermediaires inutiles
remove(Mod_M5_Taille_G_1990_4)
remove(Mod_M5_Taille_G_1990_5)
remove(Mod_M5_Taille_G_1990_6)
remove(Mod_M5_Taille_G_1990_7)
remove(Mod_M5_Taille_G_1990_8)



# Modele 6 : recherche dfs mu, sigma, tau
#########################################
M6_Taille_G_1990                         <-gamlss(data=Taille_G_0_18_pop,
                                                  family="BCPE",
                                                  taille~pb(age_jour^0.65, inter= d2 ), 
                                                  
                                                  # Fonction de lissage                   
                                                  sigma.formula=~pb(age_jour^0.65,  inter= d3 ),  #10       
                                                  nu.formula=~1,                                
                                                  tau.formula=~pb(age_jour^0.65,  inter= d5),                               
                                                  
                                                  # Fonction de lien                   
                                                  mu.link=identity,
                                                  sigma.link=log,
                                                  nu.link=identity,
                                                  tau.link=log,
                                                  
                                                  # Fixation des valeurs initiales                   
                                                  mu.fix=F,
                                                  sigma.fix=F,
                                                  nu.fix=T,
                                                  tau.fix=F,
                                                  
                                                  # Valeurs initiales
                                                  start.from = start5, 
                                                  
                                                  # Methode de maximation de vraisemblances penalisees (RS ou CG)                   
                                                  method=RS(100)                   
) 
M6_Taille_G_1990

mu6.df    <- round(M6_Taille_G_1990$mu.df - 2)
sigma6.df <- round(M6_Taille_G_1990$sigma.df - 2)
tau6.df   <- round(M6_Taille_G_1990$tau.df - 2)



# Modele 7 : fixation des inter de mu, sigma et tau et recherche df de mu, sigma et tau
#######################################################################################
M7_Taille_G_1990                         <-gamlss(data=Taille_G_0_18_pop,
                                                  family="BCPE",
                                                  taille~pb(age_jour^0.65, inter= mu6.df), 
                                                  
                                                  # Fonction de lissage                   
                                                  sigma.formula=~pb(age_jour^0.65, inter= sigma6.df),      
                                                  nu.formula=~1,                                
                                                  tau.formula=~pb(age_jour^0.65, inter = tau6.df),                               
                                                  
                                                  # Fonction de lien                   
                                                  mu.link=identity,
                                                  sigma.link=log,
                                                  nu.link=identity,
                                                  tau.link=log,
                                                  
                                                  # Fixation des valeurs initiales                   
                                                  mu.fix=F,
                                                  sigma.fix=F,
                                                  nu.fix=T,
                                                  tau.fix=F,
                                                  
                                                  # Valeurs initiales
                                                  start.from = M6_Taille_G_1990, 
                                                  
                                                  # Methode de maximation de vraisemblances penalisees (RS ou CG)                   
                                                  method=RS(100)                   
) 
M7_Taille_G_1990

gamlss_final_AFPA_SFMG_Taille_G <- M7_Taille_G_1990
rm(M7_Taille_G_1990)

