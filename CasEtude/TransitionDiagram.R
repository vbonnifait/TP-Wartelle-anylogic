library(tidyverse)
# library(readxl)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)



generate_transition_diagram <- function(transitions_df) {

  # 1. Début du graphe
  
  dot_code <- "digraph ProcessMap {"
  
  # Configuration générale (options de mise en page)
  # De Gauche à Droite (Left to Right)
  dot_code <- paste0(dot_code, '
    rankdir=LR; 
    node [shape=box, style="filled", fillcolor="lightblue"];
  ')
  
  # 2. Ajout des Arêtes (Transitions)
  for (i in 1:nrow(transitions_df)) {
    #i=1
    from <- transitions_df$from_activity[i] # to sanitise
    from = str_remove(from,"\'")
    to <- transitions_df$to_activity[i] # to sanityse
    to = str_remove(to,"\'")
    count <- transitions_df$n[i]
    selected <- transitions_df$selected[i]
    
    # Utilisation du compte pour l'épaisseur de l'arête (penwidth)
    # Nous normalisons 'n' pour obtenir une épaisseur lisible
    normalized_n <- 1 + (count / max(transitions_df$n) * 3)
    
    edge_attributes <- paste0('label="', count, '", penwidth=', normalized_n)
    
    if (selected) {
      # Ajoute la couleur rouge pour le flux le plus fréquent
      edge_attributes <- paste0(edge_attributes, ', color="red", fontcolor="red"') 
    }
    
    # Assurez-vous d'entourer les noms de nœuds (activities) par des guillemets
    # pour gérer les espaces ou caractères spéciaux (comme le problème 'SORTIE' précédent)
    line <- paste0('  "', from, '" -> "', to, '" [', edge_attributes, '];')
    dot_code <- paste(dot_code, line)
  }
  
  # 3. Fin du graphe
  dot_code <- paste0(dot_code, "\n}")
  
  return(dot_code)
}
