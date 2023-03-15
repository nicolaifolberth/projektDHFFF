library(tm)
require(topicmodels)
library(quanteda)
library(XML)
#fdrre
# erstellen einer Liste aus files, um im weiteren Verlauf auf diese zugreifen zu können.

files <- list.files(full.names = TRUE, path= "sitzungen18.21/2020", pattern = "xml$")
files


# Funktion, um Listen Zusammenzufügen
  
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





# Sitzungen ist die Liste aller Redebeiträge der betrachteten Sitzungen. 
sitzungen = list()

# iteriert durch alle Dateien, Liest sie als XML ein und filtert die Redebeiträge heraus.
for (file in files){
  sitzung <- file %>%
    xmlParse() %>%
    xpathSApply("//rede//p | //DOKUMENT//TEXT", xmlValue )
    sitzungen <- combineListsAsOne(sitzungen, sitzung)
}

stopwords_de <- quanteda::stopwords("de")  

# Selbstdefinierte zusätzliche stopwords.
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

# entfernen aller Satzzeichen, Zahlen, Sonderzeichen, stopwords, individuelle stopwords und leere Zeichenketten. 
sitzungen_tokens <- sitzungen %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%    
  tokens_remove(pattern = stopwords_de, padding = T)%>%
  tokens_remove(pattern = stopwords_sitzung, padding = T)%>%
  tokens_remove("")

        











