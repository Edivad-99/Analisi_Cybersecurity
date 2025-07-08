#---------------------------TERZA PARTE DELL'ANALISI----------------------------

#------TEXT MINING sul dataset di Kaggle "CyberIncidents from 2005 to 2020"-----

# Breve info sul dataset:
# Il dataset contiene incidenti ICT avvenuti tra il 2005 e il 2020. La raccolta 
# dei dati è stata fatta dal CFR (Concilio forense delle relazioni internazionali,
# una think tank statunitese)

# Librerie necessarie per questa analisi
library(readxl)       # Lettura di file Excel (.xlsx)
library(readr)        # Lettura di file CSV e altre operazioni di input/output
library(writexl)      # Scrittura di file Excel
library(tm)           # Text Mining (funzioni per corpus, tokenizzazione, rimozione di stopwords, ecc.)
library(tidyverse)    # Collezione di pacchetti per data manipulation e visualizzazione (dplyr, ggplot2, tidyr, etc.)
library(wordcloud)    # Generazione di word clouds per visualizzare la frequenza delle parole
library(quanteda)     # Text mining avanzato e creazione di matrici documento-termine (DFM)
library(topicmodels)  # Modelli di topic, in particolare per LDA e altre tecniche di topic modeling
library(lubridate)    # Lavorare con date e orari
library(RColorBrewer) # Palette di colori per visualizzazioni più professionali
library(text2vec)     # Creazione di modelli vettoriali, analisi LSA, word embeddings e altro
library(ggplot2)      # Creazione di grafici e visualizzazioni avanzate
library(cluster)      # Clustering: per eseguire clustering gerarchico e k-means
library(Matrix)       # Rappresentare e manipolare dati organizzati in tabelle bidimensionali

# Apriamo il dataset di kaggle  
#dataset<-read.csv("cyber-operations-incidents.csv")

# Lo trasformiamo in excel 
#write_xlsx(dataset,"IncidentiCyber.xlsx")

# Lo apriamo nuovamente nel formato desiderato 

IncidentiCyber<-read_excel("IncidentiCyber.xlsx")

# Ora procediamo a rimuovere variabili inutili alla nostra indagine di text mining 

IncidentiCyber_Filtered<-IncidentiCyber%>%select(Description)

# Abbiamo lasciato tre variabili utili:

# Category --> Indica la categoria di settore colpito(Privato, Governatico, Pubblico)
# Title --> Indica il nome dell'incidente 
# Description --> Fornisce una descrizione degli attacchi ossia di cosa è avvenuto

# Dopo aver completato questa fase di preparazione del dataset, possiamo
# iniziare a condurre le operazioni di text mining

#---------------------------FASE 1: Importazione e Pulizia----------------------

# Pulizia del testo: 
# dobbiamo fare una pulizia dei dati 
# per rimuovere rumore, come stopwords, punteggiatura, numeri, maiuscole 
# e spazi extra.

# Il dataset nuovo è gia caricato, procediamo col codice 

# Questa è una funzione creata appositamente
# dal nome "clean_text" che andra a processare e pulire il testo 
clean_text <- function(text) {
  text <- tolower(text)                          # trasformo dal maiuscolo al minuscolo
  text <- removePunctuation(text)                # rimozioni punteggiatura
  text <- removeNumbers(text)                    # rimozioni numeri 
  text <- removeWords(text, stopwords("en"))    # rimozione stopword ad esempio (the, an, of)
  text <- stripWhitespace(text)                 # rimozione spazi bianchi 
  return(text)                                  # ricorda sempre che in programmazione con funzioni, return va sempre messo
                                                # perché deve richiamare la funzione inziale
}

#Applichiamo la funzione sulla colonna Description
IncidentiCyber_Filtered$clean_description <- 
  sapply(IncidentiCyber_Filtered$Description, clean_text)

# Ora guardando al nostro dataset, vedremo che c'è una colonna nuova, con il testo
# pulito 
 

#---------------------------FASE 2: Tokenizzazione------------------------------
# TOKENIZZAZIONE: Breve ripasso 

# Tokenizzare significa suddividere un testo in unità più piccole, chiamate token. 
# Nella maggior parte dei casi, un token corrisponde a una parola, ma può anche 
# essere una frase, carattere o altra unità di testo, a seconda del
# livello di dettaglio desiderato. Tokenizzare è il primo passo fondamentale nel 
# text mining e nell'elaborazione del linguaggio naturale (NLP). 

# Senza tokenizzazione, il testo è solo una stringa continua, e non possiamo 
# analizzare parole, conteggi, frequenze, o costruire modelli basati sul significato


# In questa seconda fase tokenizzeremo il testo 

# Prima di tutto creiamo un CORPUS:

# Creare un corpus è un passaggio fondamentale prima della tokenizzazione, 
# soprattutto nelle librerie come tm o quanteda, perché:

# Un corpus è una collezione di testi 
# (ad esempio, le descrizioni degli attacchi nel dataset) 
# organizzata in una struttura che può essere analizzata.

# Può contenere:
# - Documenti (es. una descrizione per ogni attacco)
# - Metadati (es. data, vittima, tipo di attacco, ecc.)

# il corpus è lo "spazio di lavoro" del testo su cui poi si applicano:

#1. Tokenizzazione
#2. Pulizia (rimozione di stopwords, punteggiatura, stemming, ecc.)
#3. Costruzione di matrici (es. DTM o TDM)
#4. Analisi semantica, topic modeling, ecc.


# Prima di creare il corpus dobbiamo rendere il documento unique, sennò
# R restituirà questo errore:
# Errore in corpus.character(IncidentiCyber_Filtered$clean_description) : 
# docnames must be unique

# Creiamo i nomi univoci per i documenti
doc_names <- paste0("doc", seq_len(nrow(IncidentiCyber_Filtered)))

# Creiamo il corpus
corpus_descriptions <- corpus(IncidentiCyber_Filtered$clean_description, 
                              docnames = doc_names)

# In sostanza quello che stiamo facendo è dire a R di 
# prendere tutti questi testi puliti, metterli insieme in un contenitore 
# che è chiamato corpus, e associare a ciascuno un nome (doc1, doc2, doc3,ecc..)

# Tokenizziamo 
tokens <- tokens(corpus_descriptions)

# Rimuoviamo parole inutili o troppo generiche 
custom_stopwords <- c("attack", "hacking", "security", 
                      "incident", "actor", "threat","actors",
                      "used","targeted","target", 
                      "targets","south","organizations")
tokens <- tokens_remove(tokens, stopwords("en"))
tokens <- tokens_remove(tokens, custom_stopwords)

#---------------------------FASE 3: Creazione DFM-------------------------------

# Creaiamo una matrice documento-termine (DTM)

# La DTM è una rappresentazione numerica del nostro testo, dove ogni riga rappresenta 
# un documento (in questo caso, ogni incidente) e ogni colonna una parola/token. 
# La cella contiene il numero di occorrenze di quella parola nel documento

# In parole povere:
# immaginiamo di avere tre documenti che dicono:
# doc1 -> "cyber attack on company
# doc2 -> "company data breach"
# doc3 -> "cyber criminals steal data"

#La DTM sarà 
#       attack	breach	company	criminals	cyber	data	on	steal
# doc1	  1	      0	       1	      0	      1 	 0	  1	   0
# doc2	  0	      1	       1	      0	      0	   1	  0	   0
# doc3	  0	      0	       0	      1	      1	   1 	  0    1

# La DTM è necessaria perché rende il testo lavorabile da algoritmi 

# Nel nostro caso abbiamo utilizzato una DFM e non una DTM, ma sono la stessa cosa
# Quello che cambia è la nomenclatura o la struttura a seconda della libreria che 
# utilizziamo. La differenza principale tra DFM e DTM è che mentre, la DTM si concentra
# esplicitamente su parole (o termini), la DFM può essere un pò più generica e flessibile 
# può includere  anche altre caratteristiche legate al testo, come ad esempio
# - Frasi o gruppi di parole (chiamate n-grams) 
#    N.B Si chiamano n-grams perché derivano da n che sta per numero e gram che sta per segno,
#    in linguistica e statistica "gram" è usato per riferirsi ad una sequenza di 
#    elementi (parole o lettere)
#    gli n-gram possono essere 1-gram, 2-gram, 3-gram ecc 
#    ESEMPIO 1-gram: "il gatto corre" -> "il" "gatto" "corre"
#    ESEMPIO 2-gram: "il gatto corre" -> "il gatto" "gatto corre"
#    ESEMPIO 3-gram: "il gatto corre veloce" -> "il gatto corre" "gatto corre veloce"
# - Caratteristiche linguiste (lemma, parte del discorso ecc)
# - Metadati aggiuntivi (lunghezza del documento o altre misure)

# Ma perché la usiamo nel progetto rispetto alla DTM?
# Usiamo la DFM perché è una tecnica più flessibile rispetto alla DTM come visto 
# dalla spiegazione di sopra, inoltre nel nostro caso avendo descrizioni ampie
# permette di catturare significati più complessi o pattern che vanno oltre le singole
# parole (ad esempio gurppi di parole come "cyber attack" o "cyber incident" ecc)


# Tornando allo script...

# Punto 1 -> Creazione della Document-Feature Matrix (DFM)
dfm_tokens <- dfm(tokens)

# Punto 2 -> Rimozione delle parole rare (meno di 5 occorrenze) 
# qua è utile rimuoverle perché generano rumore inutile 
dfm_tokens <- dfm_trim(dfm_tokens, min_termfreq = 5)

# Nel caso vogliamo esportare la matrice in un documento esterno ad esempio
# un file excel per una visualizzazione migliore, 
# runna -> write.xlsx(as.matrix(dfm_tokens), "dfm_output.xlsx")
# (Per farlo ti serve questa libreria library(openxlsx))
# NOTA: salverà il file nella directory che stai usando)

#-------------------FASE 4: LSA (Latent Semantic Analysis)----------------------

# Ora che abbiamo la nostra DFM applicchiamo la LSA

# Cos'è la Latent Semantic Analysis?
# La LSA è una tecnica di riduzione della dimensionalità utilizzata nell'analisi 
# del testo per estrarre significati sottostanti o latenti in un corpus di documenti
# LSA è utilizzato per scoprire le relazioni semantiche tra le parole nei documenti
# e per trovare "topics" (temi) che sono latenti, ovvero non esplicitamente definiti 
# ma che emergono attraverso l'analisi 

# Quando abbiamo tanti documenti le parole non sono isolate, ma sono spesso collegate
# tra loro in modi che non sono evidenti solo guardando le singole parole
# ESEMPIO: "incidente" e "sinistro" potrebbero significare la stessa cosa in un documento, ma 
# non sono la stessa parola. LSA aiuta a capire che queste parole, anche se diverse,
# sono semanticamente simili perché appaiano spesso in contesti simili 

# Prima di tutto, dobbiamo trasformare la DFM in una matrice di dati numerici
dfm_matrix <- as.matrix(dfm_tokens)
# Se lanci view(dfm_matrix) puoi visualizzarla come label: sono 394 colonne e 481 righe

# Eseguiamo la decomposizione SVD (Singular Value Decomposition)

# Cos'è la SVD?
# è un processo matematico che scompone la matrice in tre parti: 
# matrice U: Rappresenta i documenti, mostrando quali topic sono più presenti in ciascun documento
# matrice D: è una matrice diagonale che contiene i valori singolari. Questi valori in sostanza dicono quanto
# ogni topic sia importante (maggiore valore = maggiore significato )
# matrice V: rappresenta i termini, mostrando quanto ciascuna parola è associata ai vari topic 

# U: rappresenta i documenti (matrice dei vettori singolari a sinistra)
# D: è una matrice diagonale dei valori singolari (rappresenta la forza di ogni dimensione latente)
# V: rappresenta i termini (matrice dei vettori singolari a destra)

# Lanciando questo codice avviamo la SVD

lsa <- svd(dfm_matrix)

# Se provi a runnare -> write.xlsx(lsa, "LSA_output.xlsx") vedrai che ti salverà
# un documento che con le famose matrici U D V

# Dunque la matrice originale nostra, viene scomposta in tre matrici   
# Questa è la formula -> A = U per D per V elevato a T 
# T indica una TRASPOSIZIONE, ossia un'operazione che scambia righe e colonne di 
# una matrice. Una matrice trasposta trasformerà le righe in colonne e le colonne
# in righe

# Che senso ha farla?
# Serve per un puro scopo matematico, la matrice trasposta serve per far funzionare
# la moltiplicazione tra le matrici in modo coerente, in sostanza, la trasposizione
# serve per far "incastrare" le dimensioni delle matrici nella moltiplicazione 

# Tornando allo script...
# Dopo aver fatto la SDV è necessario ridurre la dimensionalità, questo perché la 
# matrice iniziale può avere migliaia di parole e molti rumori

# Riduciamo il numero di dimensioni mantenendo solo i primi k valori singolari
k <- 5 # k indica il numero di dimensioni latenti -> questo snippet mantiene i 5 temi più forti nel corpus

# Nello specifico questo codice prende le prime 5 colonne di U
# Le moltiplica per i primi 5 valori singolari (i pesi)

lsa_reduced <- lsa$u[, 1:k] %*% diag(lsa$d[1:k]) # Quello che stiamo ottenendo è una nuova matrice
                                                 # molto più compatta, ma significativa

# Ora `lsa_reduced` è una matrice di dimensione ridotta che possiamo usare per il topic modeling
# Ogni colonna della matrice rappresenta un "topic" latente

# Esploriamo i risultati di LSA
# Creiamo una matrice di termini per ogni topic
terms_per_topic <- data.frame(lsa_reduced)
colnames(terms_per_topic) <- paste("Topic", 1:k)

# Visualizziamo i primi 10 termini per ogni topic
head(terms_per_topic, 10)

#---------------------------FASE 5: Topic Modelling-----------------------------

# In questa quinta fase ci occupiamo del topic modelling

# Cos'è il Topic Modelling? 

# Il Topic Modelling è una tecnica di NLP (Natural Language Process) 
# per scoprire automaticamente gli argomenti latenti (i "topic") 
# all’interno di un insieme di documenti.
# Il topic modelling cerca di rispondere ala domanda "Quali sono i temi principali 
# che emergono dai testi?"
# La sfida sta nel fatto che gli argomenti non sono esplicitamente etichettati 
# nei documenti: non c'è un'indicazione chiara che dica 
# "questo paragrafo parla di economia", "questo parla di salute", ecc. 
# Per farlo, il Topic Modelling cerca di trovare pattern ricorrenti di parole che 
# si manifestano frequentemente insieme nei documenti, e li raggruppa in topic.

#Ad esempio:

# Un gruppo di documenti potrebbe contenere frequentemente parole come "cane", "gatto", 
# "animale", "zoologia": questi documenti potrebbero essere associati al topic 
# relativo agli animali.

# Un altro gruppo potrebbe avere parole come "email", "phishing", "password", 
# "login", suggerendo un topic relativo alla sicurezza informatica.

# Il Topic Modelling è molto utile quando abbiamo una grande quantità di testo e 
# vogliamo esplorare in modo automatico i principali argomenti senza dover 
# leggere ogni singolo documento.



# Nel topic modelling l'algoritmo più usato è LDA (Latent Dirichlet Allocation)

# Cos'è LDA?
# Si tratta di un modello probabilistico che cerca di identificare argomenti latenti
# in un corpus di documenti 
# LDA parte dall'idea che ogni documento nel corpus sia una combinazione di vari topic,
# e che ogni topic sia a sua volta una distribuzione di parole 

# In parole povere...
# Ogni documento è una miscela di vari argomenti (topic)
# Ogni topic è una distribuzione di parole che tendono ad apparire insieme nei documenti 

# LDA lavora cercando di inviduare questi topic e di attribuire ad ogni documento
# una combinazione di topic, mentre ogni topic è definito dalle parole che appaiono
# frequentemente insieme 

# PASSAGGI DEL FUNZIONAMENTO:

# Assunzioni iniziali:
  # Ogni documento è una combinazione di vari topic ((ad esempio,
  #un articolo di giornale potrebbe trattare sia di politica che di economia, 
  #quindi sarebbe una combinazione dei topic "politica" e "economia")
 
  # Ogni topic è una combinazione di parole (ad esempio, il topic "economia",
  # potrebbe includere parole come "mercato","banche", "inflazione")

# Processo di inferenza
  # LDA cerca di indovinare quali topic sono associati a ciascun documento e quali 
  # parole sono associate a ciascun topic. Questo avviene tramite un processo 
  # iterativo che cerca di ottimizzare la probabilità che i documenti possano
  # essere rappresentati come una combinazione di topic e che le parole in ogni 
  # topic abbiano una probabilità alta di essere scelte 

# Output del modello LDA
  # LDA restituisce un set di topic, dove ogni topic è rappresentato da una lista 
  # di parole con una probabilità associate e un'assegnazione di topic ai documenti,
  # cioè per ciascun documento LDA fornirà la probabilità che quel documento 
  # tratti ciascuno dei topic 

# FUNZIONAMENTO NEL DETTAGLIO 

# Immaginiamo di avere un gruppo di documenti e di voler scoprire k topic latenti 
# LDA cerca di modellare il corpus seguendo questi passaggi 

# Passaggio 1: inizializzazione casuale
  # Per ogni documento, LDA inizia assegnando randomicamente uno o più topic
  # Per ogni topic, vengono assegnate parole casuali 

# Passaggio 2: Risposta iterativa - LDA esegue una serie di iterazioni in cui:
  # Per ogni parola in ogni documento, LDA calcola la probabilità che quella 
  # parola appartenga a ciascun topic 
  # LDA riassegna la parola al topic che ha la probabilità più alta, e aggiorna
  # la distribuzione di parole per ogni topic 

# Passaggio 3: Convergenza 
  # Dopo cicli di iterazione, LDA si "converge" cioè raggiunge una distribuzione 
  # stabile di topic per ogni documento e una lista stabile di parole per 
  # ciascun topic 

# COMPONENTI PRINCIPALI DI LDA 
  # Distribuzione dei topic nei documenti: ogni documento è visto come una combinazione
  # di topic, e LDA stima la probabilità che ciascun documento tratti determinati topic.
  # Queste probabilità vengono assegnate a ciascun documento 

  # Distribuzione delle parole per ogni topic: ogni topic è visto come una 
  # distribuzione di parole. Ad esempio, un topic può avere una probabilità 
  # molto alta per parole come "politica", "elezione", "governo", ecc


# Tornando allo script...


# Convertiamo la dfm in una DocumentTermMatrix del pacchetto tm perchè serve
# alla LDA per leggerla
dtm <- convert(dfm_tokens, to = "topicmodels")

# Impostiamo il numero di topic (è modificabile: è meglio iniziare con un valore basso
# inizierò con 5)
num_topics <- 5

# Applichiamo LDA
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
# k = num_topics -> indica il numero di topic che voglio estrarre
# control = list(seed = 1234) -> imposta un seme per la riproducibilità dei risultati

# Cos'è un seed e perché è importante?
# n R, molte funzioni che coinvolgono processi casuali 
# (come l'inizializzazione casuale nei modelli LDA) possono produrre risultati 
# diversi ogni volta che vengono eseguite. Per garantire che i risultati siano 
# riproducibili, si utilizza un "seed" (seme) per inizializzare il generatore di 
# numeri casuali. Impostando lo stesso seed, si ottiene la stessa sequenza di numeri 
# casuali, e quindi risultati coerenti tra le esecuzioni.

# Impostando control = list(seed = 1234), si assicura che l'inizializzazione 
# casuale del modello sia la stessa ogni volta che viene eseguito. 


# Visualizzo i termini più rappresentativi per ciascun topic
topics_terms <- terms(lda_model, 20)  # Top 20 parole per topic
print(topics_terms)

# Visualizzo la distribuzione dei topic nei documenti
topics_documents <- topics(lda_model)
head(topics_documents)

# FACCIAMO UNA VISUALIZZAZIONE

# Librerie necessarie
install.packages("tidytext")
library(tidytext)
library(ggplot2)

# Estraimo i beta term 

# Cosa sono i beta term?
  # Abbiamo detto che ogni topic è rappresentato da una distribuzione di parole
  # la beta è la probabilità che una data parola appartenga a un dato topic
  # la beta dice quanto una parola è rappresentativa di un topic
  # più è alta, più quella parola è tipica per quel topic 
lda_tidy <- tidy(lda_model)

#Prendiamo le top parole per topic
top_terms <- lda_tidy %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Visualizziamo con ggplot per vederne la distribuzione
ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 20 Termsper Topic", x = NULL, y = "Beta (Term Probability)")

# ogni topic è una distribuzione di parole: cioè, una lista di parole che tendono 
# a comparire frequentemente insieme in un gruppo di documenti.

# Ad esempio se nel primo topic, abbiamo questi valori 
# Topic 1:  phishing, email, credentials, password, link, spoofing, fraud, login, scam, account  

# Il significato stimato potrebbe essere che riguarda phishing e furti di credenziali

# Nel nostro caso i topic sono 5 e contengono 20 parole (guarda al grafico o lancia
# questa funzione nel terminale, per visionarli "terms(lda_model, 20)" )

# N.B: Inizialmente erano solo 10 le parole prese in considerazione, ma più parole 
# porta maggior valore informativo 

#------------------------INTERPRETAZIONE DEI RISULTATI--------------------------

# TOPIC 1
# Parole principali: "group", "companies", "government", "computer", "defense", 
# "compromised", "espionage", "networks", "targeting", "us", "believed", "malware", 
# "purpose", "tool", "china", "united", "data", "hackers", "network", "apt"

# Interpretazione:Questo topic sembra riferirsi a attacchi informatici sofisticati 
# e mirati, probabilmente da parte di gruppi APT (Advanced Persistent Threat), 
# che colpiscono aziende e governi in settori critici come la difesa e la tecnologia. 
# Le parole come "compromised", "espionage", e "malware" suggeriscono che gli attacchi 
# sono finalizzati al furto di dati sensibili o alla spionaggio industriale o politico, 
# con attori statali (come la Cina). Questi attacchi sembrano essere altamente mirati e 
# prolungati nel tempo.


# TOPIC 2
# Parole principali: "us", "russian", "espionage", "cyber", "also", "compromised", 
# "states", "entities", "north", "information", "service", "individuals", "data", 
# "networks", "companies", "malware", "emails", "sectors", "united"

# Interpretazione: Questo topic riflette le dinamiche di spionaggio informatico 
# tra Stati-nazione, con un forte focus su attori come Russia, Corea del Nord e USA. 
# Le parole "espionage", "cyber", e "malware" indicano attacchi finalizzati al furto di 
# informazioni riservate o all'accesso a reti e sistemi di governi e aziende. 
# L'enfasi su email e compromised suggerisce che questi attacchi possano avvenire 
# tramite phishing o altre tecniche di intrusione per raccogliere dati o comprometterli.

# TOPIC 3:
# Parole principali: "government", "compromised", "united", "companies", "military", 
# "us", "networks", "entities", "chinese", "compromise", "group", "states", "email",
# "information", "east", "access", "accounts", "hackers", "department", "purposes"

# Interpretazione: In questo topic, il focus è su attacchi informatici diretti contro 
# governi, aziende, e settori militari. Le parole chiave come "compromised", "hackers",
# "email", e "access" suggeriscono che il topic è incentrato su campagne di phishing 
# o altre modalità di compromissione delle credenziali e dei sistemi. In particolare, 
# la presenza di parole come "chinese" e "east" indica attacchi provenienti da attori asiatici,
# e i termini come "military" e "government" suggeriscono obiettivi strategici di spionaggio o sabotaggio.

# TOPIC 4
# Parole principali: "espionage", "intelligence", "campaign", "us", "officials", 
# "russian", "compromise", "department", "malware", "believed", "military", "accounts", 
# "united", "information", "spearphishing", "purposes", "systems", "data", 
# "government", "hackers"

# Interpretazione: Questo topic riguarda operazioni di spionaggio informatico molto 
# mirate, che spesso utilizzano tecniche come spearphishing per compromettere account 
# o sistemi governativi. Le parole come "espionage", "intelligence", e "officials" 
# indicano un obiettivo strategico per raccogliere informazioni sensibili tramite 
# attacchi contro funzionari governativi o militari. I termini "malware" e "hackers" 
# suggeriscono che queste campagne sono supportate da tecniche avanzate di intrusione 
# nformatica e spionaggio politico.

#TOPIC 5 
# Parole principali: "government", "purposes", "compromised", "networks", "us",
# "espionage", "defense", "hackers", "iranian", "foreign", "purpose", "chinese", 
# "operations", "energy", "states", "korean", "network", "officials", "computers", "power"

# Interpretazione: Questo topic sembra riguardare attacchi contro infrastrutture 
# critiche, come quelle nel settore dell'energia e della difesa. Le parole chiave 
# come "power", "energy", "defense", "network", e "computers" suggeriscono che gli 
# attacchi sono indirizzati a sistemi vitali (centrali elettriche, impianti energetici, 
# reti di difesa) con obiettivi sabotatori o di spionaggio. Gli attori esterni come Iran, 
# Corea del Nord, e Cina sembrano essere i principali sospettati dietro questi 
# attacchi, con scopi legati alla sovversione o al controllo delle risorse strategiche.


#In sintesi...

# Topic 1: Attacchi APT (Advanced Persistent Threat) mirati contro governi e aziende, 
# con un forte focus su spionaggio informatico e furto di dati sensibili 
# (soprattutto tecnologici e militari).

# Topic 2: Spionaggio cibernetico tra stati-nazione, con focus su Russia e 
# Corea del Nord, attacchi a sistemi governativi e aziendali per raccogliere 
# informazioni riservate.

# Topic 3: Attacchi mirati a sistemi governativi e settori militari attraverso 
# tecniche di phishing e compromissione delle credenziali, con un focus su attori asiatici.

# Topic 4: Spionaggio informatico altamente mirato tramite tecniche come spearphishing,
# con l'obiettivo di raccogliere informazioni di intelligence da funzionari 
# governativi e settori militari.

# Topic 5: Attacchi contro infrastrutture critiche (come energia e difesa), 
# con attori esterni come Iran, Corea del Nord, e Cina dietro tentativi di sabotaggio 
#o spionaggio strategico.


#---------------------------FASE 6: La cluster Analysis-------------------------

# In questa ultima fase utilizziamo la cluster analysis per raggruppare 
# i 481 documenti in gruppi omogenei, ossia cluster, basandosi su caratteristiche
# semantiche estratte nella fase precedente, ossia l'LSA

# Usiamo i dati ridotti dimensionalmente tramite LSA (lsa_reduced)
# Ogni riga rappresenta un documento, ogni colonna un topic latente

# Standardizziamo i dati per migliorare le performance del clustering
lsa_scaled <- scale(lsa_reduced)


# Il clustering (specialmente K-means) funziona meglio se tutte le dimensioni hanno la stessa scala.
  # Per questo usiamo scale(), che:
  # centra i dati (media = 0) 
  # li normalizza (deviazione standard = 1)

# Decidiamo il numero di cluster 
set.seed(123)  # il seed va impostato sempre per una questione di riproducibilità
num_clusters <- 5  # Ho messo 2 perché 5 cluster non aveva molto senso

# Applichiamo il clustering K-means
# Il K-Means è un algoritmo di clustering non supervisionato 

# Obiettivo del k-means è quello di raggruppare i documenti in k gruppi (in questo caso 5),
# minimizzando la distanza interna ai cluster (cioè rendere i gruppi più "coesi" possibile)
# l'algoritmo assegna ogni punto al centroide più vicino, e poi ricalcola i centroidi iterativamente 
# nstart = 25 vuol dire che l'algoritmo prova 25 inizializzazioni diverse per evitare che la soluzione sia sub-ottimale
kmeans_result <- kmeans(lsa_scaled, centers = num_clusters, nstart = 25)

# Aggiungiamo i cluster al dataset originale
IncidentiCyber_Filtered$Cluster <- kmeans_result$cluster

# Visualizziamo il numero di documenti per ogni cluster
table(IncidentiCyber_Filtered$Cluster)

# Visualizziamo i cluster con PCA per proiezione bidimensionale
library(factoextra)

fviz_cluster(kmeans_result, 
             data = lsa_scaled,
             geom = "point", 
             ellipse.type = "convex",
             palette = "Set2",
             main = "Cluster Analysis degli Incidenti Cyber (LSA + KMeans)")

# Analisi qualitativa: vediamo qualche descrizione esemplificativa per ogni cluster
for (i in 1:num_clusters) {
  cat(paste("\n\n=== ESEMPI CLUSTER", i, "===\n"))
  print(head(IncidentiCyber_Filtered$Description[IncidentiCyber_Filtered$Cluster == i],10))}
  
  
#-------------------------------ANALISI DEI CLUSTER-----------------------------
# CLUSTER 1
# Numero documenti: 1
# Contenuto: un incidente enorme e complesso connesso a moltissimi episodi di attacchi 
# (GRU, hacking di app militari ucraine, agenzie antidoping, TV europee, ecc.).

# Temi: spionaggio statale, attacchi sofisticati, geopolitica.

# Topic LDA corrispondenti:
  # Topic 1 (APT, stati, governi, attacchi mirati)
  # Topic 2 (Russia, spionaggio, malware, compromissione reti)

# Coerenza: ALTISSIMA. Sembra un caso particolarmente ricco che tocca più topic 
# insieme, ma perfettamente in linea con gli argomenti centrali dell’analisi.

################################################################################

# CLUSTER 2
# Numero documenti: 34

# Contenuto: attacchi con forte connotazione geopolitica (Cyber Command USA, GRU, 
# abotaggio, attacchi elettorali, spionaggio contro Westinghouse, FIFA, università).

# Temi: guerra cibernetica, interferenze elettorali, controspionaggio USA vs 
# Russia/Iran/NK.

# Topic LDA corrispondenti:
  
  #Topic 2 (Russia/USA/NK, spionaggio statale)
  #Topic 4 (campagne mirate, spearphishing, intelligence)
# Coerenza: MOLTO ALTA. Cluster pienamente in linea con i topic su cyber warfare e intelligence geopolitica.

################################################################################

# CLUSTER 3
# Numero documenti: 2

# Contenuto: Caso DNC e WADA. Entrambi collegati alla Russia, attacchi mirati 
# per interferire politicamente o danneggiare reputazione.

# Temi: spionaggio, disinformazione, manipolazione politica.
# Topic LDA corrispondenti:
  
  #Topic 2 e Topic 4 (entrambe parlano di compromissione governativa, spearphishing, 
  #intelligence)

# Coerenza: ELEVATA. Cluster piccolo ma molto focalizzato sul tema della 
# manipolazione/influenza geopolitica (e.g. elezioni, doping).

################################################################################

# CLUSTER 4
# Numero documenti: 117

# Contenuto: operazioni APT globali, campagne in Asia e Medio Oriente, Iran/Cina/Russia, 
# attacchi a enti religiosi, diplomatici e settori privati.

# Temi: campagne APT mirate, spesso con un mix di spionaggio e criminalità informatica.

# Topic LDA corrispondenti:
  
  #Topic 1 (APT, targeting, malware)

  #Topic 3 (phishing, credenziali, aziende/governi)

  #Topic 5 (attacchi infrastrutturali, difesa, energia)

# Coerenza: MOLTO BUONA. Cluster misto ma tutto su attacchi sofisticati e persistenti a target critici.

################################################################################

# CLUSTER 5
# Numero documenti: 327 (cluster più grande)

# Contenuto: moltissimi episodi diversificati: attacchi ad infrastrutture, 
# sabotaggio, attacchi politici e giornalisti, social engineering, malware, Corea, Iran, Hamas, Cina, ecc.

# Temi: il cluster più "generalista", ma sempre focalizzato su attori statali o 
# quasi, con motivazioni strategiche (sabotaggio, disinformazione, spionaggio).

# Topic LDA corrispondenti:
  
  #Topic 5 (infrastrutture critiche, sabotaggio, attori come Iran/Cina/Korea)

  #Un po’ di Topic 1/2/4 anche, per via della varietà dei casi

# Coerenza: BUONA, anche se è un cluster "contenitore", ha una forte coerenza 
# con i topic principali. I documenti sono uniti dalla strategicità degli obiettivi 
# (governi, energia, giornalisti, aziende hi-tech).


