
library(rtweet)
library(dplyr)
library(stringr)
library(tm)
library(textclean)
library(stringi)
library(qdap)

consumer_key = "XXXXXXXXXX"
consumer_secret = "XXXXXXXXXX"
access_token <- "XXXXXXXXXX"
access_secret <- "XXXXXXXXXX"

token <- create_token(app = "name_app", consumer_key, consumer_secret,access_token,access_secret)


df <- search_tweets(
  "#5G OR #5g", n = 250000, retryonratelimit = TRUE,include_rts = FALSE
)

#trim -> Elimina espacios iniciales y finales de una cadena
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

save_as_csv(df, "df_5g.csv", prepend_ids = FALSE, na = "",
            fileEncoding = "Windows-1252")

View(df)

#Tamaño del dataframe
dim(df)

#Guardar data en archivo csv
save_as_csv(df, "datos_analisis_sentimiento\\df.csv", prepend_ids = TRUE, na = "",
            fileEncoding = "Windows-1252")

#Leer archivo
df <- read.csv("datos_analisis_sentimiento\\df.csv",header = TRUE, sep = ",",check.names = TRUE, encoding="Windows-1252")

#Visualizar archivo
View(df)

#Seleccionar tweets en inglés.
df_filter <- df %>% filter(lang == "en")

#Tamaño del dataframe
dim(df_filter)

#Depurar tweets sin portada.
df_filter <- df_filter %>% filter(profile_banner_url != "")
df_filter <- df_filter %>% filter(profile_banner_url != " ")

#Depurar tweets sin imagen de perfil.
df_filter <- df_filter %>% filter(profile_image_url !="")
df_filter <- df_filter %>% filter(profile_image_url !=" ")

#Depurar location null
df_filter <- df_filter %>% filter(location !="")
df_filter <- df_filter %>% filter(location !=" ")

#Depurar tweets null
df_filter <- df_filter %>% filter(text !="")
df_filter <- df_filter %>% filter(text !=" ")

#Tamaño del dataframe
dim(df_filter)

#Visualizar archivo
View(df_filter)

#Se ordenó por alfabeto
location_filter <- df_filter[order(df_filter$location),]

#Datos únicos de location
unique_location <- data.frame(unique(location_filter$location))

#Se ordeno por alfabeto
unique_location <- unique_location[order(unique_location),]

#Se convierte a dataframe
unique_location <- data.frame(unique_location)

#Visualizar archivo
View(unique_location)

#Decodificar letras con formato unicode.
"<U+041C><U+043E><U+0441><U+043A><U+0432><U+0430>, <U+0411><U+043E><U+043B><U+044C><U+0448><U+043E><U+0439> <U+041A><U+0430><U+0440><U+0435><U+0442><U+043D><U+044B><U+0439> 21\1
"%>% 
  stri_replace_all_regex("<U\\+([[:alnum:]]+)>", "\\\\u$1") %>% 
  stri_unescape_unicode() %>% 
  stri_enc_toutf8()

#Reemplazar formato unicode por su nombre.
i <- 1
continue <- TRUE
while(continue){
  a <- trim(df_filter[i,74]) #74 -> columna location
  if (is.na(a)) {
    #TRUE
    i = i + 1
  }
  else if(a == "<U+0391>tt<U+03B9><U+03BA><U+03AE>, <U+0395><U+03BB><U+03BB><U+03AC><U+03C2>"){
    df_filter[i,74] <- "greece"
    i <-  i + 1
  }
  else if(a == "<U+041C><U+043E><U+0441><U+043A><U+0432><U+0430>, <U+0411><U+043E><U+043B><U+044C><U+0448><U+043E><U+0439> <U+041A><U+0430><U+0440><U+0435><U+0442><U+043D><U+044B><U+0439> 21\1" ||
          a == "<U+041C><U+043E><U+0441><U+043A><U+0432><U+0430>, <U+0411><U+043E><U+043B><U+044C><U+0448><U+043E><U+0439> <U+041A><U+0430><U+0440><U+0435><U+0442><U+043D><U+044B><U+0439> 21\001" ||
          a == "<U+041C><U+043E><U+0441><U+043A><U+0432><U+0430>, <U+0420><U+043E><U+0441><U+0441><U+0438><U+044F>" ||
          a == "<U+0423><U+043B><U+044C><U+044F><U+043D><U+043E><U+0432><U+0441><U+043A>, <U+0420><U+043E><U+0441><U+0441><U+0438><U+044F>"){
    df_filter[i,74] <- "russia"
    i <-  i + 1
  }
  else if(a == "<U+0627><U+0633><U+0644><U+0627><U+0645> <U+0622><U+0628><U+0627><U+062F>, <U+062F><U+0628><U+0626>,"){
    df_filter[i,74] <- "united arab emirates"
    i <-  i + 1
  }
  else if(a == "<U+0906><U+0927><U+093E> <U+0926><U+093F><U+0932><U+094D><U+0932><U+0940> <U+0935><U+093E><U+0932><U+093E> ,<U+0939><U+093E><U+092B> <U+092E><U+0941><U+0902><U+092C><U+0908><U+0915><U+0930>" ||
          a == "<U+0907><U+0902><U+0926><U+093F>, <U+092D><U+093E><U+0930><U+0924>" ||
          a == "<U+092D><U+093E><U+0930><U+0924>"){
    df_filter[i,74] <- "india"
    i <-  i + 1
  }
  else if(a == "<U+0E01><U+0E23><U+0E38><U+0E07><U+0E40><U+0E17><U+0E1E><U+0E21><U+0E2B><U+0E32><U+0E19><U+0E04><U+0E23>, <U+0E1B><U+0E23><U+0E30><U+0E40><U+0E17><U+0E28><U+0E44><U+0E17><U+0E22>"){
    df_filter[i,74] <- "tailandia"
    i <-  i + 1
  }
  else if(a == "<U+4E0A><U+6D77>, <U+4E2D><U+534E><U+4EBA><U+6C11><U+5171><U+548C><U+56FD>" ||
          a == "<U+4E2D><U+534E><U+4EBA><U+6C11><U+5171><U+548C><U+56FD>" ||
          a == "<U+4E2D><U+56FD> <U+5E7F><U+5DDE>" || 
          a == "<U+5317><U+4EAC>, <U+4E2D><U+534E><U+4EBA><U+6C11><U+5171><U+548C><U+56FD>" ||
          a == "<U+6DF1><U+5733>"){
    df_filter[i,74] <- "China"
    i <-  i + 1
  }
  else if(a == "<U+65E5><U+672C>"){
    df_filter[i,74] <- "Japan"
    i <-  i + 1
  }
  else if(a == "<U+9999><U+6E2F>"){
    df_filter[i,74] <- "Hong Kong"
    i <-  i + 1
  }
  else if(a == "/ <U+2606>CURRENT DAY, USA<U+2606> /"){
    df_filter[i,74] <- "usa"
    i <-  i + 1
  }
  else{
    i <-  i + 1
  }
  if(i > nrow(df_filter)){
    continue=FALSE
  }
  
}

#Se ordenó por alfabeto
location_filter <- df_filter[order(df_filter$location),]

#Datos únicos del location
unique_location <- data.frame(unique(location_filter$location))

#Se ordeno por alfabeto
unique_location <- unique_location[order(unique_location),]

#Se convierte a dataframe
unique_location <- data.frame(unique_location)

#Visualizar archivo
View(unique_location)

View(df_filter)


#Eliminar caracteres especiales a location
#trash <- c("http","<U","#","&amp",'…','£','”','•','‘','–','@')
trash <- c("http","<U","#","&amp",'@')

i <- 1
continue <- TRUE
while(continue) {
  v <- trash[i]
  v <- paste(v,'\\S+\\s*',sep = "")
  df_filter$location <-  str_trim(gsub(v,"", df_filter$location))
  i <- i + 1
  if(i > length(trash)){
    continue <- FALSE
  }
}

View(df_filter)

#Función limpiar
clean <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[!]+', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[.]+', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[?]+', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[,]+', replacement = '')
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

#Se convierte a corpus
corpus_text <- Corpus(VectorSource(df_filter$location))

#Se aplica la funciónn clean
corpus_text <- clean(corpus_text)

#Se convierte a dataframe
corpus_text <- data.frame(corpus_text)

#Se elimina primera columna
corpus_text <- corpus_text[,-1]

#Se convierte a dataframe
corpus_text <- data.frame(corpus_text)

#Se cambia el nombre a la columna
colnames(corpus_text)[1]<-'location'

#Se elimina columna location del df_filter 
df_filter <- df_filter[,-74]

#Se añade nueva columna preprocesada
df_filter <- cbind(df_filter,corpus_text)

#Visualizar archivo
View(df_filter)

#Depurar location null
df_filter <- df_filter %>% filter(location !="")
df_filter <- df_filter %>% filter(location !=" ")

#Visualizar archivo
View(df_filter)

#Depurar tweets duplicados.
df_filter <- distinct(df_filter)

#__________________________________________
#Reemplazar nombre de localizacion por su similar: ejemplo: Estados Unidos -> usa

i <- 1

continue <- TRUE
while(continue){
  texto <- trim(df_filter[i,90])
  #eliminar tildes
  texto <- chartr('ãáéíóúüé','aaeiouue', texto)
  
  if (is.na(texto)) {
    #TRUE
    i = i + 1
  }
  
  else if (texto == "caracas venezuela") {
    df_filter[i,90] <- "venezuela"
    i = i + 1
  }
  else if (texto == "new delhi" || texto == "hyderabad india" || texto == "pune india" || 
           texto == "chennai india" || texto == "mumbai india" || texto == "new delhi india" || 
           texto == "noida india" || texto == "new delhi delhi" || texto =="ahmadabad city india" || 
           texto == "ahmedabad" || texto == "ahmedabad gujarat" || texto == "amravati india"
           || texto == "bengaluru india" || texto =="jalandhar india" || texto == "gurgaon india" ||
           texto == "delhi india"){
    df_filter[i,90] <- "india"
    i = i + 1
  }
  else if (texto == "united kingdom" || texto == "london uk" || texto == "london united kingdom" || 
           texto == "london england" || texto == "london" || texto == "london enland"
           || texto == "united kindom" || texto == "uk" || texto == "manchester enland"|| texto == "england united kingdom"||
           texto == "sheffield" || texto == "hertfordshire" || texto == "south east england" ||
           texto == "cambridge england" || texto == "manchester england" || texto == "hull england"){
    df_filter[i,90] <- "england"
    i = i + 1
  }
  else if (texto == "united states" || texto == "new york ny" || texto == "carol stream il" || 
           texto == "boston ma" || texto == "los angeles ca" || texto == "chicago il" || 
           texto == "new york usa" || texto == "washington dc" || texto == "denver co" || 
           texto == "new york" || texto == "seattle wa" || texto == "sickerville nj" ||
           texto == "new york city" || texto == "houston tx" || texto == "california usa" ||
           texto == "los angeles" || texto == "minnesota usa" || texto == "miami fl" 
           || texto == "arizona usa" || texto == "charlotte harbor fl " || texto == "seattle usa"
           || texto == "las vegas nv" || texto == "ohio usa" || texto == "santa clara ca"
           || texto == "michigan usa" || texto == "silicon valley" || texto == "georgia usa"
           || texto == "pennsylvania usa" || texto == "boston" || texto == "texas" || texto == "texas usa"
           || texto == "massachusetts usa" || texto == "charlotte harbor fl" || texto == "brooklyn ny"
           || texto == "michigan" || texto == "virginia usa" || texto == "charlotte nc" 
           || texto == "denver" || texto == "kansas city mo" || texto == "manhattan ny" || texto == "chicago"
           || texto == "estados unidos" || texto == "usa"  || texto == "new york ny usa"
           || texto == "new york new york" || texto == "new york london" || texto == "new york city ny"
           || texto == "new york city new york" || texto == "new york chicago" || texto == "new york london"
           || texto == "anaheim ca" || texto == "arlington va" || texto == "arlington virginia"
           || texto == "austin tx" || texto == "austintx" || texto == "boston massachusetts" || texto == "boston ma usa"
           || texto == "bothell wa usa" || texto == "brattleboro vt usa" || texto == "carol stream il"
           || texto == "washinton dc" || texto == "san francisco" || texto == "sw michian usa" || texto == "usa"
           || texto == "san francisco ca" || texto == "san dieo ca" || texto == "atlanta ga" || texto == "san francisco nyc"
           || texto == "san jose ca" || texto == "san diego ca" || texto == "dallas tx" || texto == "florida usa"
           || texto == "new jersey usa" || texto == "san diego" || texto == "san diego california"
           || texto == "wisconsin" || texto == "dallas tx usa"|| texto == "official ericsson us canada" || texto =="washington dc area" ||
           texto == "sw michigan usa" || texto == "eu" || texto =="newton mass usa" || texto == "san jose ca usa" ||
           texto == "united states of" || texto == "united states of america" || texto == "alameda ca" || text == "united states of" ||
           texto == "silicon valley ca") { 
    
    df_filter[i,90] <- "usa" 
    i = i + 1
  }
  else if (texto == "marseille france" || texto == "paris france" || texto == "france" || texto == "paris"
           || texto == "iledefrance france" || texto == "toulouse france" || texto == "berreletang france"
           || texto == "france" || texto == "massy france") {
    df_filter[i,90] <- "france" 
    
    i = i + 1
  }
  else if(texto == "seminyak bali"){
    df_filter[i,90] <- "indonesia"
    i = i + 1
  }
  else if(texto == "tokyoto japan" || texto == "tokyo japan" || texto == "japon" || texto == "japan"
          || texto == "osaka japan"){
    df_filter[i,90] <- "japan"
    i = i + 1
  }
  else if(texto == "shenzhen china" || texto == "beijin china" || texto == "bejin china"
          || texto == "shanhai china" || texto =="beijing china" || texto == "bejing china"){
    df_filter[i,90] <- "china"
    i = i + 1
  }
  else if(texto == "barcelona" || texto == "barcelona españa" || texto == "madrid"
          || texto == "madrid spain" || texto == "sevilla españa" || texto == "valencia"
          || texto == "barcelona spain" || texto == "valencia españa" || texto == "andalucia"
          || texto == "españa" || texto == "espana" || texto == "madrid españa" || texto == "madrid comunidad de madrid" ||
          texto == "barcelona catalonia"){
    df_filter[i,90] <- "spain"
    i = i + 1
  }
  else if(texto == "toronto on" || texto == "toronto ontario" || texto == "toronto canada"
          || texto == "montreal quebec" || texto == "vancouver bc" || texto == "toronto"
          || texto == "canada bc kelowna" || texto == "canada vancouver" || texto == "canada"
          || texto == "ontario canada"|| texto == "greater vancouver" || texto == "ottawa canada"
  ){
    df_filter[i,90] <- "canada"
    i = i + 1
  }
  else if(texto == "malaysia" || texto == "singapore"){
    df_filter[i,90] <- "malaysia"
    i = i + 1
  }
  else if(texto == "ireland"){
    df_filter[i,90] <- "ireland"
    i = i + 1
  }
  else if(texto == "sao paulo" || texto == "sao paulo brasil" || texto == "sao paulo brazil" || 
          texto == "brazil" || texto == "brasilia brasil" || texto == "betim brasil" 
          || texto == "brasilia brazil" || texto == "brasilia df" || texto == "brasil"
          || texto == "rio de janeiro brasil"){
    df_filter[i,90] <- "brazil"
    i = i + 1
  }
  else if(texto == "istanbul turkiye" || texto == "türkiye" || texto == "turkiye" ){
    df_filter[i,90] <- "turkey"
    i = i + 1
  }
  else if(texto == "genova italy" || texto == "italy" || texto == "treviso italy"
          || texto == "italia" || texto == "rome italy"){
    df_filter[i,90] <- "italy"
    i = i + 1
  }
  else if(texto == "g funkturm" || texto == "italy" || texto == "treviso italy"
          || texto == "hambur germany" || texto == "goettingen germany" || texto == "goettingen germany"
          || texto == "berlin germany" || texto == "berlin deutschland" || texto == "oldenburg" || texto =="hamburg"){
    df_filter[i,90] <- "germany"
    i = i + 1
  }
  else if(texto == "stockholm sverie" || texto == "stockholm sweden" || texto == "berlin" || texto =="stockholm sverige"){
    df_filter[i,90] <- "sweden"
    i = i + 1
  }
  else if(texto == "helsinki finland" || texto == "espoo finland" ){
    df_filter[i,90] <- "finland"
    i = i + 1
  }
  else if(texto == "mostly in tokyo"){
    df_filter[i,90] <- "tokyo"
    i = i + 1
  }
  else if(texto == "lagosnigeria" || texto == "lagos nigeria"){
    df_filter[i,90] <- "nigeria"
    i = i + 1
  }
  else if(texto == "bangkok thailand"){
    df_filter[i,90] <- "thailand"
    i = i + 1
  }
  else if(texto == "the netherlands"){
    df_filter[i,90] <- "netherlands"
    i = i + 1
  }
  else if(texto == "brussels belgium"){
    df_filter[i,90] <- "belgium"
    i = i + 1
  }
  else if(texto == "ushuaia argentina"){
    df_filter[i,90] <- "argentina"
    i = i + 1
  }
  else if(texto == "worldwide" || texto == "global" || texto == "everywhere" || texto == "tere picheufufufuf"
          || texto == "deutschland"|| texto == "earth"|| texto == "o sea"|| texto == "internet"
          || texto == "ai" || texto == "seg"|| texto == "betacaryophyllene" || texto == "iii everywhere"){
    df_filter[i,90] <- "others"
    i = i + 1
  }
  else{
    i = i + 1
  }
  if(i > nrow(df_filter)){
    continue <- FALSE
  }
  
}

#Obtener columna location
probar_freq <- data.frame(df_filter[,90])
colnames(probar_freq)[1]<-"location"
View(probar_freq)


#Filtar datos con location vacias.
probar_freq <- probar_freq %>% filter(location !="")
View(probar_freq)

#Tabla de frecuencia location
tabla_freq <- probar_freq %>% 
  count(location)  %>% 
  arrange(desc(n)) 

View(tabla_freq)

total <- sum(tabla_freq$n)

tabla_freq$porcentage <- tabla_freq$n * 100 / total

View(tabla_freq)
#######
dim(df_filter)

#Depurar location null
df_filter <- df_filter %>% filter(location !="")
df_filter <- df_filter %>% filter(location !=" ")

#Visualizar archivo
View(df_filter)

#Seleccionar columna text
select_text <- df_filter$text
respaldo <- select_text
select_text <- respaldo

#Eliminar caracteres especiales a text
trash <- c("http","<U","#","&amp")
#trash <- c("http","#","<U","&amp",'£','”','•','‘','—')
i <- 1
continue <- TRUE
while(continue) {
  v <- trash[i]
  v <- paste(v,'\\S+\\s*',sep = "")
  select_text <-  str_trim(gsub(v," ", select_text))
  i <- i + 1
  if(i > length(trash)){
    continue <- FALSE
  }
}

View(select_text)

#Función preprocesamiento text
clean_text <- function(corpus){
  corpus <- tm_map(corpus, replace_email)
  corpus <- tm_map(corpus, removeURLhttps)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[,]+', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[@]+', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[#]+', replacement = ' ')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[™]+', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[’]+', replacement = "'")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[—]+', replacement = " ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[“]+', replacement = " ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[”]+', replacement = " ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[–]+', replacement = " ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[…]+', replacement = " ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[·]+', replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[€]+', replacement = " ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[,]+', replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[(]+', replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[)]+', replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[<]+', replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[>]+', replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[_]+', replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[•]+', replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[´]+', replacement = "'")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[°]+', replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[']+", replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[£]+", replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[…]+", replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[”]+", replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[•]+", replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[‘]+", replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[–]+", replacement = "")
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, stemDocument) #recorta palabras
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

#Se convierte de tipo corpus 
select_text <- Corpus(VectorSource(select_text))

#Se aplica la función clean_text
select_text <- clean_text(select_text)

#Se convierte a dataframe
select_text <-  as.data.frame(select_text)

#Se elimina columna doc_id
select_text <- select_text[,-1]

#Se convierte a dataframe
select_text <- as.data.frame(select_text)

#Se cambia el nombre a la columnat ext
colnames(select_text)[1]<-'text'

select_text <- as.data.frame(lapply(select_text, function(y) gsub("[[:punct:]]", "", y)))
select_text <- as.data.frame(lapply(select_text, function(y) gsub(",,", "", y)))

#Se elimina la columna text de 
df_filter <- df_filter[,-5]

View(df_filter)


#Eliminar espacios al inicio y final de tweet
select_text <- as.data.frame(apply(select_text,2,function(x)gsub("^\\s+|\\s+$", "", x)))

#cambiar text a formato latin-ascii
i <- 1
continue <- TRUE
while(continue){
  texto <- select_text[i,1]
  #eliminar tildes
  texto <- chartr('ãáéíóúüé','aaeiouue', texto)
  texto <- stringi::stri_trans_general(texto, "latin-ascii")
  
  write.table(texto, "datos_analisis_sentimiento\\beta_text.csv", sep = ",", append = T,
              row.names=FALSE, 
              col.names=FALSE )
  i = i + 1

  if(i > nrow(select_text)){
    continue <- FALSE
  }
}
select_text <- read.csv("datos_analisis_sentimiento\\beta_text.csv", header = F, sep = ",",check.names = FALSE)


colnames(select_text)[1] <- "text"

View(select_text)

#Se añade nueva columna preprocesada text
df_filter <- cbind(df_filter,select_text)

#respaldo
df_filter2 <- df_filter

#Depurar tweets duplicados.
df_filter2 <- df_filter2[!duplicated(df_filter2$text), ]

#Seleccionar tweets mayores a 80
df_filter2 <- df_filter2 %>% filter(nchar(as.character(text))>80)

#Guardar data en archivo csv
save_as_csv(df_filter2, "datos_analisis_sentimiento\\df_final.csv", prepend_ids = FALSE, na = "",
            fileEncoding = "Windows-1252")

#__________________________________________________
View(df_filter2)
#Freq Column location -> %
#Obtener columna location
probar_freq <- data.frame(df_filter2[,89])
colnames(probar_freq)[1]<-"location"
View(probar_freq)

#Filtar datos con location vacias.
probar_freq <- probar_freq %>% filter(location !="")
probar_freq <- probar_freq %>% filter(location !=" ")
View(probar_freq)

#Tabla de frecuencia location
tabla_freq <- probar_freq %>% 
  count(location)  %>% 
  arrange(desc(n)) 

View(tabla_freq)

total <- sum(tabla_freq$n)

tabla_freq$porcentage <- tabla_freq$n * 100 / total

View(tabla_freq)
#__________________________________________________

#Seleccionar columna text
select_colum_text <- df_filter2$text

#Se convierte a dataframe
select_colum_text <-  as.data.frame(select_colum_text)

#Delete caracter -> ,,
select_colum_text <- as.data.frame(lapply(select_colum_text, function(y) gsub(",,", "", y)))


select_colum_text <- as.data.frame(apply(select_colum_text,2,function(x)gsub("^\\s+|\\s+$", "", x)))

dim(select_colum_text)

#Se cambia el nombre a la columnat text
colnames(select_colum_text)[1]<-'text'

#Guardar data en archivo csv
save_as_csv(select_colum_text, "datos_analisis_sentimiento\\df_text.csv", prepend_ids = FALSE, na = "",
            fileEncoding = "Windows-1252")


#______________________________________
#1 ->positivo
#2->negativo
#0->neutro
data_score <- read.csv("datos_analisis_sentimiento\\data_score.csv", header = FALSE, sep = ",", check.names = TRUE)

colnames(data_score)[1] <- "score"

continuar <- TRUE
i <- 1

while (continuar) {
  v_score <- as.matrix(data_score[i,1])
  if(v_score > 0 ){
    positivo <- 1
    data_score[i,1] <- positivo
    i <- i + 1
  }
  else if(v_score < 0){
    negativo <- 2
    data_score[i,1] <- negativo
    i <- i + 1
  }
  else if(v_score == 0){
    neutro <- 0
    data_score[i,1] <- neutro
    i <- i + 1
  }
  if(i > nrow(data_score)){
    continuar <- FALSE
  }
}

View(data_score)


df_text <- read.csv("datos_analisis_sentimiento\\df_text.csv", header = TRUE, sep = ",",check.names = TRUE)

#se une tweet con score
tweets_score <- cbind(df_text,data_score)

save_as_csv(tweets_score, "datos_analisis_sentimiento\\tweets_score.csv", prepend_ids = FALSE, na = "",
            fileEncoding = "Windows-1252")

#___________________________________
#Columna hashtags freq
hashtags <- df_filter2$hashtags
hashtags <- Corpus(VectorSource(hashtags))
#Generar tabla de freq
dtm <- TermDocumentMatrix(hashtags)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq = v)
#d2 <- head(d,20)
View(d)