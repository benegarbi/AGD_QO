## ----packages,warning=FALSE,results='hide',message=FALSE----
library(R.temis)


## ----import----------------------------------------------
corpus <- import_corpus("data/PEE_K1_extract.csv", format="csv",textcolumn=15,language="fr")


## ----tle-------------------------------------------------
dtm <-build_dtm(corpus, remove_stopwords=F,min_length=2)
dtm


## ----voir, eval=FALSE------------------------------------
## # des metadonnées
## View(meta(corpus))
## 
## # des réponses à la QO
## View(sapply(corpus,as.character))
## 
## # d'un extrait du Tableau lexical
## inspect(dtm)
## #as.matrix(dtm[1:10, c("de", "abus")])


## ----freqT-----------------------------------------------
dic<-dictionary(dtm,remove_stopwords = F)

frequent_terms(dtm, n=20)


## ----nuage-----------------------------------------------
cloud<-word_cloud(dtm, color="black", n=100, min.freq=20,remove_stopwords=T)


## ----concordances----------------------------------------
concordances(corpus,dtm,"enquêté")
concordances(corpus,dtm,"alentour")
#concordances(corpus,dtm,"sur")
#concordances(corpus,dtm,"que")


## ----ccoc_t----------------------------------------------
cooc_terms(dtm,"logement", n=10)


## ----AFC_TLE---------------------------------------------
resTLE <-corpus_ca(corpus,dtm, sparsity=0.985)
#explor(resTLE)
res <- explor::prepare_results(resTLE)
explor::CA_var_plot(res, xax = 1, yax = 2, lev_sup = TRUE, var_sup = FALSE,
    var_sup_choice = , var_hide = "Row", var_lab_min_contrib = 0, col_var = "Position",
    symbol_var = "Type", size_var = "Contrib", size_range = c(23.4375, 312.5),
    labels_size = 10, point_size = 25, transitions = TRUE, labels_positions = NULL,
    xlim = c(-3, 2.91), ylim = c(-2.42, 3.49))


## ----repart----------------------------------------------
library (questionr)

table(meta(corpus)$sexe)
table(meta(corpus)$aget)
table(meta(corpus)$matrim)
table(meta(corpus)$pratique)
table(meta(corpus)$enf)
table(meta(corpus)$activite)
table(meta(corpus)$prof)
table(meta(corpus)$dipl)
table(meta(corpus)$vote)
table(meta(corpus)$habitat)
table(meta(corpus)$localite)
table(meta(corpus)$revenu)
table(meta(corpus)$region)



## ----BilanLex--------------------------------------------
lexical_summary(dtm, corpus,"region", unit = "global")           


## ----Mspec-----------------------------------------------
# selon le région
specific_terms(dtm,meta(corpus)$aget, n=10)


## ----Rspec-----------------------------------------------
#Réponses spécifiques selon la région
characteristic_docs(corpus,dtm,meta(corpus)$region, ndocs=5)


## ----AFC_TLA---------------------------------------------
resTLA<-corpus_ca(corpus,dtm, variables =c("sexe","aget",	"matrim","pratique","enf","activite","prof","dipl","vote",	"habitat"	,"localite","revenu"),sparsity=0.98)
#explor(resTLA)
res <- explor::prepare_results(resTLA)
explor::CA_var_plot(res, xax = 1, yax = 2, lev_sup = FALSE, var_sup = FALSE,
    var_sup_choice = , var_hide = "None", var_lab_min_contrib = 2, col_var = "Position",
    symbol_var = "Type", size_var = "Contrib", size_range = c(26.25, 350), labels_size = 10,
    point_size = 28, transitions = TRUE, labels_positions = NULL, xlim = c(-0.393,
        0.447), ylim = c(-0.413, 0.426))


## ----extr_doc--------------------------------------------
#Documents les plus illustratifs par axe
extreme_docs(corpus,resTLA,axis=1,ndocs=3)
extreme_docs(corpus,resTLA,axis=2,ndocs=3)
extreme_docs(corpus,resTLA,axis=3,ndocs=3)


## ----nettoyage-------------------------------------------
#concordances(corpus,dtm,"sur")
# liste des mots à supprimer
asupp <- c("sur","que","qu") 
# on enlève les mots de la liste dans le tableau lexical
dtm2 <-dtm[, !colnames(dtm) %in% asupp]


## ----dico, include=FALSE---------------------------------
dic<-dictionary(dtm,remove_stopwords = T)
# on enlève les mots de la liste dans le dictionnaire
dic2 <- dic[!rownames(dic) %in% asupp,]


## ----Export_dic, eval=FALSE, include=FALSE---------------
## write.csv2(dic2, file="dic2_lem.csv")


## ----import_lem1-----------------------------------------
dic_lem1 <- read.csv2("data/Pee_dic_lem_extract.csv",row.names=1)

#setdiff(rownames(dic2), rownames(dic_lem1))

dtmlem1 <-combine_terms(dtm2, dic_lem1)


## ----freq_lem1-------------------------------------------
frequent_terms(dtmlem1, n=30)


## ----gram------------------------------------------------
library(dplyr)
lexique3 <- read.csv("data/Lexique383_simplifie.csv", fileEncoding="UTF-8")
lexique3 <- arrange(lexique3, desc(freqlivres))
lexique3 <- lexique3[!duplicated(lexique3$ortho),]
voc_actif <- lexique3[lexique3$cgram %in% c("ADV", "VER", "ADJ", "NOM", "PRO:per","PRO:pos"),]
dic_total <- merge(dic, voc_actif, by.x="row.names", by.y="ortho", all.x=TRUE)
dic_total <- mutate(dic_total, Term=coalesce(lemme, Term))
rownames(dic_total) <- dic_total$Row.names

# Lister les mots non reconnus par Lexique 3  
nr <- filter(dic_total, is.na(lemme))



## ----lem_lex3--------------------------------------------
dtmlem2 <- combine_terms(dtm, dic_total)


## ----freq_lex3-------------------------------------------
frequent_terms(dtmlem2, n=30)


## ----tree,, eval=FALSE-----------------------------------
## arbre <- terms_graph(dtmlem2, min_occ = 30, interactive=F)


## ----AFC_TLElem2-----------------------------------------
resTLElem2 <-corpus_ca(corpus,dtmlem2,sparsity=0.985 )
#explor(resTLElem2)

res <- explor::prepare_results(resTLElem2)
explor::CA_var_plot(res, xax = 1, yax = 2, lev_sup = FALSE, var_sup = FALSE,
    var_sup_choice = , var_hide = "Row", var_lab_min_contrib = 0, col_var = "Position",
    symbol_var = "Type", size_var = "Contrib", size_range = c(23.4375, 312.5),
    labels_size = 10, point_size = 25, transitions = TRUE, labels_positions = NULL,
    xlim = c(-2.55, 9.92), ylim = c(-4.66, 7.81))


## ----AFC_TLAlem2-----------------------------------------
resTLAlem2<-corpus_ca(corpus,dtmlem2, variables =c("sexe","aget",	"matrim","pratique","enf","activite","prof","dipl","vote",	"habitat"	,"localite","revenu"),sparsity=0.98)
#explor(resTLAlem2)
res <- explor::prepare_results(resTLAlem2)
explor::CA_var_plot(res, xax = 1, yax = 2, lev_sup = FALSE, var_sup = FALSE,
    var_sup_choice = , var_hide = "None", var_lab_min_contrib = 0, col_var = "Position",
    symbol_var = "Type", size_var = "Contrib", size_range = c(22.5, 300), labels_size = 10,
    point_size = 24, transitions = TRUE, labels_positions = NULL, xlim = c(-0.544,
        0.517), ylim = c(-0.482, 0.579))


## ----corr_lemm2------------------------------------------
dic_total_c <- dic_total

dic_total_c$Term[dic_total_c$Term == "bretagn"] <- "bretagne"
dic_total_c$Term[dic_total_c$Term == "tranquil"] <- "tranquilité"

dtmlem22 <- combine_terms(dtm, dic_total_c)



## ----sscorpus, eval=FALSE, include=FALSE-----------------
## 
## #Ici par exemple par les réponses des enquêtes dans la région Bretagne
## corpus_bret <- corpus[meta(corpus,"region") == "Bretagne"]
## # Ici les questions ouvertes contenant le mot entourage
## corpus_entourage <- subset_corpus(corpus,dtm,"entourage")


## ----clus------------------------------------------------
clusTLE <- corpus_ca(corpus, dtmlem2,variables = NULL, ncp =  6, sparsity = 0.98)

#Afficher le dendrogramme
#plot(clusTLE$call$tree)
#clusTLE$desc.var


## ----cah_cut---------------------------------------------
clus <- corpus_clustering(clusTLE,7)


## ----add_clus--------------------------------------------
corpus_cl <- add_clusters(corpus,clus)

#View(meta(corpus_cl))

#clusTLE$desc.var



## ----cah_desc--------------------------------------------
specific_terms(dtmlem2,meta(corpus_cl)$clus, n=5)


## ----doc_char2-------------------------------------------
characteristic_docs(corpus_cl,dtmlem2,meta(corpus_cl)$clu, ndocs=3)


## ----reinert, eval=FALSE,message=FALSE-------------------
## library(rainette)
## corpus
## 
## # On adapte le TLE pour quanteda
## dfm <- quanteda::as.dfm(dtm)
## quanteda::docvars(dfm, "doc_id") <- 1:nrow(dfm)
## 
## # On exécute rainette
## 
## resrai <- rainette(dfm, min_uc_size = 3, k = 10, doc_id = "doc_id", cc_test = 0.3,
##   tsj = 3)


## ----rai_explo, eval=FALSE,message=FALSE-----------------
## rainette_explor(resrai,dfm)
## 
## groups <-cutree_rainette(resrai, k = 10)


## ----rai_stat, eval=FALSE,message=FALSE------------------
## rainette_stats(groups, dfm, n_terms = 5)


## ----rain_v_cl , eval=FALSE,message=FALSE----------------
## meta(corpus, "classe") <- cutree_rainette(resrai, k = 10)
## #View(meta(corpus))

