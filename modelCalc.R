library(magrittr)
library(quanteda.textstats)
library(quanteda)
library(topicmodels)


# Sitzungen wird in eine Document term Matrix umgewandelt 
DTM <- sitzungen_tokens %>%
  dfm() 
  # Tokens die mindestens 10 und Maximal 50 mal enthalten sind werden beibehalten.
DTM <- dfm_trim(DTM, min_termfreq = 10, max_termfreq = 50)



#filtert leere reihen heraus
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]

# Aufruf des LDA Algorithmus mit 20 Themen.

K <- 20
topicModel <- LDA(DTM, K, method = "Gibbs", control = list(

  iter = 1000,
  seed = 1,
  verbose = 25,
  alpha = 0.05))
tmResult <- posterior(topicModel)


# Ausgabe des Topic Models auf die Konsole.
attributes(tmResult) 
ncol(DTM) 
beta <- tmResult$terms 
dim(beta) 
rowSums(beta) 
nrow(DTM) 

theta <- tmResult$topics
dim(theta) 
rowSums(theta)[1:10] 
terms(topicModel, 100)
top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")
sink("/Users/nico/TestProjektDHSemesterarbeit/Ausgabe.TopicModel/TMjaehrlicheAuswertung/2021.txt")
terms(topicModel, 100)
sink()


  