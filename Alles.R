library(tm)
#options(stringsAsFactors = False)
require(topicmodels)
library(quanteda)
library(XML)

# erstelleån einer Liste aus files, um automatisch auf diese zugreifen zu können.

files <- list.files(full.names = TRUE, path= "sitzungen17.21/2020"
                    , pattern = "xml$")
files
# xmlDatei <- xmlParse("sitzungen/20043-data.xml")

# reden <- xpathSApply(xmlDatei,  "//rede//p", xmlValue)
# reden



combineListsAsOne <-function(list1, list2){
  n <- c()
  for(x in list1){
    n<-c(n, x)
  }
  for(y in list2){
    n<-c(n, y)
  }
  return(n)
}




sitzungen = list()
for (file in files){
  sitzung <- file %>%
    xmlParse() %>%
    xpathSApply("//rede//p | //DOKUMENT//TEXT", xmlValue )
  sitzungen <- combineListsAsOne(sitzungen, sitzung)
}

stopwords_de <- quanteda::stopwords("de")  


stopwords_sitzung <- c("dass", "mehr", "ja", "gut", "viele", "vielen", "gibt", "mal","ganz", "cdu", "spd", "csu", "fdp", "afd","abgeordneten","grünen","grünen", "g","bündnis","sitzung", "müssen" ,"berlin","menschen" ,"a","linken", "afd", "linke","cdu",     
                       "a","beifall","grü-",    
                       "fraktion","c",   
                       "abgeordneten","präsident",  
                       "c", "b", 
                       "sowie", "frau",
                       "b", "fdp",
                       "damen","d",
                       "wer", "deutscher",
                       "schon","wahlperiode" ,
                       "sagen","wolfgang",
                       "d","petra",
                       "herren" ,"spd",
                       "deutschland","linke",
                       "liebe","bitte",
                       "land","csu",
                       "geht","michael",
                       "beim","sowie",
                       "bundestag","deutschen",
                       "kolleginnen",   "beim"            ,
                       "abg"               ,  "pau"             ,
                       "ausschuss"          , "vizepräsidentin" ,
                       "dank"   ,             "matthias"        ,
                       "immer"   ,            "kollege"         ,
                       "drucksache",          "thomas"          ,
                       "deutscher"  ,         "schäuble"        ,
                       "brauchen"    ,        "wort"            ,
                       "wahlperiode"  ,       "parl"            ,
                       "donnerstag"    ,      "antwort"         ,
                       "beratung"       ,     "staatssekretär"  ,
                       "dr"              ,    "müller"          ,
                       "euro"             ,   "jahr"            ,
                       "antrag"            ,  "stefan"          ,
                       "berlin"             , "nen"             ,
                       "deshalb", "dank",            
                       "sitzung",             "stephan"         ,
                       "gesetzentwurf",       "christian"      , 
                       "gerade"        ,      "fragen"          ,
                       "nen"            ,     "martin"          ,
                       "unsere"          ,    "antrag"          ,
                       "stimmt"           ,   "de"              ,
                       "ver-"              ,  "linken"          ,
                       "recht"              , "deutschland"     ,
                       "gesetz"              ,"kollegin"        ,
                       "kommen",             "bundes-"         ,
                       "tun"    ,             "jens"            ,
                       "wurde"   ,            "andreas"         ,
                       "genau"    ,           "alexander"       ,
                       "frau"      ,          "katja"           ,
                       "zuruf"      ,         "schon"           ,
                       "richtig"     ,        "oliver"          ,
                       "seit"         ,       "friedrich"       ,
                       "lassen"        ,      "bun-"            ,
                       "märz"           ,     "nehmen"          ,
                       "wäre"            ,    "müssen"          ,
                       "jahren"           ,   "bündnisses"      ,
                       "leben"             ,  "uwe"             ,
                       "klar"               , "be-"             ,
                       "beschlussempfehlung", "prozent"         ,
                       "möchte"   ,           "wahl"            ,
                       "gen"       ,          "minister"        ,
                       "kollege"    ,         "frauen"          ,
                       "wirklich"    ,        "stimmen"         ,
                       "gesagt"       ,       "januar"          ,
                       "november"      ,      "februar"         ,
                       "deutlich"       ,     "markus"          ,
                       "vizepräsident"   ,    "kolleginnen"     ,
                       "eben"             ,   "johannes"        ,
                       "letzten"           ,  "freitag"         ,
                       "zeit"               , "präsidentin"     ,
                       "fraktionen" ,         "krischer"        ,
                       "debatte"     ,        "bundesministerin",
                       "ten"          ,       "darauf"          ,
                       "jahre"         ,      "maßnahmen"       ,
                       "einfach"        ,     "ten"             ,
                       "stehen"          ,    "danke"           ,
                       "eigentlich"       ,   "beispiel"        ,
                       "bürger"            ,  "wissen"          ,
                       "gar"                , "ge-"             ,
                       "wichtig"             ,"immer"           ,
                       "prozent",             "stellt"          ,
                       "dabei",               "ersten"          ,
                       "bildung",             "bernd"            ,"weiterer",            "bundestages"  ,   
                       "bereits" ,            "norbert"         ,
                       "tagesordnungspunkt" , "peter"           ,
                       "gehen",               "geht"            ,
                       "lage"  ,              "rufe", "geben", "christoph")


sitzungen_tokens <- sitzungen %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%    
  tokens_remove(pattern = stopwords_de, padding = T)%>%
  tokens_remove(pattern = stopwords_sitzung, padding = T)%>%
  tokens_remove("")

library(magrittr)
library(quanteda.textstats)
library(quanteda)
library(topicmodels)



DTM <- sitzungen_tokens %>%
  
  # DTM = Document term Matrix 
  dfm() 

DTM <- dfm_trim(DTM, min_termfreq = 10, max_termfreq = 50)



top_terms <- c("kollegen", "heute", "dafür",
               "herr", "deswegen")
DTM <- DTM[, !(colnames(DTM) %in% top_terms)]
#filtert leere reihen heraus
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]

#Besides estimating each topic as a mixture of words, 
#LDA also models each document as a mixture of topics.
K <- 20
topicModel <- LDA(DTM, K, method = "Gibbs", control = list(
  
  iter = 1000,
  seed = 1,
  verbose = 25,
  alpha = 0.05))
tmResult <- posterior(topicModel)



attributes(tmResult)
ncol(DTM) # lengthOfVocab
# topics are probability distribtions over the entire
# vocabulary
beta <- tmResult$terms # get beta from results
dim(beta) # K distributions over ncol(DTM) terms
rowSums(beta) # rows in beta sum to 1
nrow(DTM) # size of collection
# for every document we have a probability distribution of
# its contained topics
theta <- tmResult$topics
dim(theta) # nDocs(DTM) distributions over K topics
rowSums(theta)[1:10] # rows in theta sum to 1
terms(topicModel, 100)
top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")

library(LDAvis)
library(tsne)
svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   vocab = colnames(DTM), term.frequency = colSums(DTM), mds.method = svd_tsne,
                   plot.opts = list(xlab = "", ylab = ""))
serVis(json)


