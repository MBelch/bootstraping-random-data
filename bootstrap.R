###################### Libraries###################################
library(ggplot2)
################################ Question 1 ####################################
set.seed(2190903)
n <- 100 ## on change n = 1000 pour l'étape 7
datagen <- rexp(n, rate = 2)


## Plot histogramme des données:

ggplot(data = as.data.frame(datagen), aes(x = datagen)) + 
  geom_histogram(colour = 'black', fill = 'orange') +
  ylab("Fréquence") +
  xlab("datagen") +
  labs(title = "Histogramme des données Datagen")

################################ Question 2 ####################################

bsample <- datagen[sample(n, n, replace = TRUE)]


ggplot(as.data.frame(bsample), aes(x = bsample)) + 
  geom_histogram(colour = 'black', fill = 'lightblue') +
  ylab("Fréquence") +
  xlab("bsample") +
  labs(title = "Histogramme des données bsample")

################################ Question 3 ####################################

## génération de l'échantillon bootstrap
m = 100000
bmatrix_sample <- matrix(NA,nrow = m, ncol =n)
for(i in 1:m){
  bsample <- datagen[sample(n, n, replace = TRUE)]
  bmatrix_sample[i,] <- bsample
}

################################ 3.b ###########################################

## la moyenne de l'échantillon bootstrap
mean_bms <- rep(NA,n)
for(i in 1:n){
  mean_bms[i] <- mean(bmatrix_sample[,i])
}

## histogram des moyennes calculées:

ggplot(as.data.frame(mean_bms), aes(x = mean_bms)) + 
  geom_histogram(colour = 'black', fill = 'grey')+
  ylab("Fréquence") +
  xlab("bootstrap de l'échantillon") +
  labs(title = "Histogramme des données après bootstraping")
  



################################ Question 4 ####################################

## calcul des quantiles des moyennes des échantillons bootstrap

quantile(mean_bms, probs = c(0.025, 0.975))

ggplot(as.data.frame(mean_bms), aes(x = mean_bms)) + 
  geom_histogram(aes(y = ..density..),colour = 'black', fill = 'grey') +
  geom_density(colour = 'red') +
  geom_vline(xintercept = quantile(mean_bms, probs = 0.025), colour = 'blue') +
  geom_vline(xintercept = quantile(mean_bms, probs = 0.975), colour = 'blue') +
  ylab("Fréquence") +
  xlab("bootstrap de l'échantillon") +
  labs(title = "Histogramme des données après bootstraping")

################################ Question 5 ####################################

## courbe des quantiles empiriques 
##plot(ppoints(1000),sort(datagen), pch = 20)


## bootstraping des quatiles empiriques
bootstrap_qe <- matrix(NA,nrow = m, ncol = n)
for(i in 1:m){
  bootstrap_qe[i,] <- sort(bmatrix_sample[i,])
}
bootstrap_qe

## interval de confiance pour chaque quantile empirique:
## on crée une matrice de 2 colonnes, une contenant les quantile de niveau 2.5%
## l'autre contenant les quantiles de niveau 97.5%

interval_confiance_qe <- matrix(NA,nrow = n, ncol = 2)


for(i in 1:n){
  interval_confiance_qe[i,1] <- quantile(bootstrap_qe[,i], probs = 0.025)
  interval_confiance_qe[i,2] <- quantile(bootstrap_qe[,i], probs = 0.975)
}
colnames(interval_confiance_qe) <- c("quantile_2.5%", "quatile_97.5%")



################################ Question 6 ####################################

plot(ppoints(n),sort(datagen), pch = 20)

ggplot(data = as.data.frame(datagen), aes(x = ppoints(n), y = sort(datagen)))+
  geom_point()+
  geom_line(y = interval_confiance_qe[,1], color = "red")+
  geom_line(y = interval_confiance_qe[,2], color ='red')+
  ylab("Quantile empirique") +
  xlab("Fréquence empirique") +
  labs(title = "Courbe des quantiles empiriques")
  

############################### Question 7 #####################################

## on répète le tout mais pour n = 1000



