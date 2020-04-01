review_data <- read.csv(file = "./b2w-10k.csv")

head(review_data)

filtered_data <- review_data[,c(11,10)]

head(filtered_data)

filtered_data$score<-ifelse(filtered_data$recommend_to_a_friend=="Yes", 1,0)

filtered_data <- filtered_data[1:2000,c(1,3)]

library(stringr)

rm_accent <- function(str,pattern="all") {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "'", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)

  pattern <- unique(pattern)

  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"

  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )

  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )

  accentTypes <- c("´","'","^","~","¨","ç")

  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))

  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)

  return(str)
}

clean_string <- function(string){
    # Lowercase
    temp <- tolower(string)
    # Remove everything that is not a number or letter (may want to keep more 
    # stuff in your actual analyses). 
    temp <- rm_accent(temp)
    temp <- stringr::str_replace_all(temp,"r\\$", " ")
    temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
    # Shrink down to just one white space
    temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
    # Split it
    return(temp)
}

filtered_data$review_text <- clean_string(filtered_data$review_text)

dim(filtered_data)

head(filtered_data)

library(quanteda)

library(dplyr)

df_corpus <- filtered_data %>% summarise(text = paste0(review_text, sep = "", collapse = ". "))

dim(df_corpus)

string_sequence <- clean_string(df_corpus$text)

meu_corpus <- corpus(string_sequence)
summary(meu_corpus)

corpus_dfm <- dfm(meu_corpus, remove_punct = TRUE,
                  remove = quanteda::stopwords("portuguese"))

sorted_dfm <- dfm_sort(corpus_dfm)[, 1:2000]
top_5k_terms <- featnames(sorted_dfm)

term <- dfm_select(corpus_dfm, "contente")

length(featnames(term))

is.na(match(featnames(term), top_5k_terms))

vectorize_sequences <- function(sequences, dimension = 2000) {
  # Creates an all-zero matrix of shape (length(sequences), dimension)
    results <- matrix(0, nrow = length(sequences), ncol = dimension) 
    print(dim(results))
    for (i in 1:length(sequences)){
        print(paste0("Iteration: ",i))
        splitted_sequence <- stringr::str_split(sequences[[i]], " ")[[1]]
        #print(length(splitted_sequence))
        for (j in 1:length(splitted_sequence)){
            term <- dfm_select(corpus_dfm, splitted_sequence[j])
            if(! length(featnames(term))==0){
                term_rank <- match(featnames(term), top_5k_terms)
                if(! is.na(term_rank)){
                    # Sets specific indices of results[i] to 1s
                    results[i, term_rank] <- 1 
                }    
            }
        }
    }
    results
}

feature_matrix <- vectorize_sequences(filtered_data$review_text)

write.table(feature_matrix, file="feature_matrix.txt")



