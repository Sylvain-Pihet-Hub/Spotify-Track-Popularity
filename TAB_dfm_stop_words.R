# The documentation of this function is standard in R... For the package "roxygen2"
# If you put this in a package, the formatting will be automatically converted to a help file
# That way you can see this documentation simply by typing ?TMEF_dfm at any time

#' A simple dfm function for text mining
#' @description Builds a document feature matrix from a corpus of documents
#' @param text character vector This should contain a corpus of documents
#' @param ngrams numeric what length of phrase should we use? Default is 1
#' @param stop.words logical Should stop words be deleted? Default is TRUE
#' @param min.prop numeric threshold for including rare words. Default is .01 (i.e. at least 1% of documents)
#' @details We built this in class. It requires textclean and quanteda to run
#' @return A document feature matrix
#'
#' @import magrittr
#' @import quanteda
#' @import textclean
#' @export
TAB_dfm_stop_words<-function(text,
                   ngrams=1,
                   stop.words=TRUE,
                   min.prop=.01){
  # First, we check our input is correct
  if(!is.character(text)){  
    stop("Must input character vector")
  }
  custom_stopwords <- c("can", "verse", "intro", "chorus", "guitar", "bridge", "outro", "lyrics", "vers", "just", "refrain",
                        "pre-chorus", "post-chorus")
  #uses stop.words arugment to adjust what is dropped
  drop_list <- if(stop.words) c(stopwords("en"), custom_stopwords) else custom_stopwords 
  # quanteda pipeline
  text_data<-text %>%
    replace_contraction() %>%
    tokens(remove_numbers=TRUE,
           remove_punct = TRUE) %>%
    tokens_wordstem() %>%
    tokens_select(pattern = drop_list, 
                  selection = "remove") %>%
    tokens_ngrams(ngrams) %>%
    dfm() %>%
    dfm_trim(min_docfreq = min.prop,docfreq_type="prop")
  return(text_data)
}
