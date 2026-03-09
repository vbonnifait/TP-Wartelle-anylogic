library(tidyverse)
# library(readxl)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Parcours patient circuit d'urgences
# IOA : Infirmier\ère Organisateur/trice d'Acceuil
# Waiting activites are not distinguished
# CC : Circuit Court
# CL : Circuit Long
# UHCD : Unité d'Hospitalisation Courte Durée
# Transfert : Vers un autre hôpital
#activities <- c("Triage IOA","Examen CC","Examen CL","Post Soins",
#                "Sortie UHCD", "Sortie Domicile","Sortie Transfert")
ground_truth_process_model_pathways_df <-
  tibble(pathway = c("Urgent Pathway",
                     "Average Pathway",
                     "Innapropriate Pathway"),
         pathway_probability = c(0.4,0.3,0.3),
         starting_activity = c("Triage IOA",
                               "Triage IOA",
                               "Triage IOA"))
ground_truth_process_model_pathways_df

ground_truth_process_model_transitions_df <- tibble(pathway = "",
      from_activity = "",
      to_activity = "",
      transition_probability = 0) %>%
  rbind(list("Urgent Pathway","Triage IOA","Examen CL",1),
        list("Urgent Pathway","Examen CL","Post Soins",1),
        list("Urgent Pathway","Post Soins","Sortie Domicile",0.2),
        list("Urgent Pathway","Post Soins","Sortie UHCD",0.72),
        list("Urgent Pathway","Post Soins","Sortie Transfert",0.08),
        list("Average Pathway","Triage IOA","Examen CC",1),
        list("Average Pathway","Examen CC","Post Soins",1),
        list("Average Pathway","Post Soins","Sortie Domicile",0.8),
        list("Average Pathway","Post Soins","Sortie UHCD",0.18),
        list("Average Pathway","Post Soins","Sortie Transfert",0.02),
        list("Innapropriate Pathway","Triage IOA","Examen CC",1),
        list("Innapropriate Pathway","Examen CC","Sortie Domicile",1))
ground_truth_process_model_transitions_df

# diagram1 = generate_transition_diagram(ground_truth_process_model_transitions_df %>%
#                               filter(pathway == "Urgent Pathway") %>%
#                               group_by(from_activity) %>%
#                               mutate(transition_probability = transition_probability / sum(transition_probability)) %>%
#                               mutate(n = floor(transition_probability*100),
#                                      weight = transition_probability,
#                                      selected = FALSE)) %>% print()
diagram1 ="digraph ProcessMap {\n    rankdir=LR; \n    node [shape=box, style=\"filled\", fillcolor=\"lightblue\"];\n     \"Triage IOA\" -> \"Examen CL\" [label=\"100\", penwidth=4];   \"Examen CL\" -> \"Post Soins\" [label=\"100\", penwidth=4];   \"Post Soins\" -> \"Sortie Domicile\" [label=\"20\", penwidth=1.6];   \"Post Soins\" -> \"Sortie UHCD\" [label=\"72\", penwidth=3.16];   \"Post Soins\" -> \"Sortie Transfert\" [label=\"8\", penwidth=1.24];\n}"
cat(diagram1)
grViz(diagram1)

# diagram2 = generate_transition_diagram(ground_truth_process_model_transitions_df %>%
#                               filter(pathway == "Average Pathway") %>%
#                               group_by(from_activity) %>%
#                               mutate(transition_probability = transition_probability / sum(transition_probability)) %>%
#                               mutate(n = floor(transition_probability*100),
#                                      weight = transition_probability,
#                                      selected = FALSE)) %>% print()
diagram2 = "digraph ProcessMap {\n    rankdir=LR; \n    node [shape=box, style=\"filled\", fillcolor=\"lightblue\"];\n     \"Triage IOA\" -> \"Examen CC\" [label=\"100\", penwidth=4];   \"Examen CC\" -> \"Post Soins\" [label=\"100\", penwidth=4];   \"Post Soins\" -> \"Sortie Domicile\" [label=\"80\", penwidth=3.4];   \"Post Soins\" -> \"Sortie UHCD\" [label=\"18\", penwidth=1.54];   \"Post Soins\" -> \"Sortie Transfert\" [label=\"2\", penwidth=1.06];\n}"
cat(diagram2)
grViz(diagram2)

# diagram3 = generate_transition_diagram(ground_truth_process_model_transitions_df %>%
#                               filter(pathway == "Innapropriate Pathway") %>%
#                               group_by(from_activity) %>%
#                               mutate(transition_probability = transition_probability / sum(transition_probability)) %>%
#                               mutate(n = floor(transition_probability*100),
#                                      weight = transition_probability,
#                                      selected = FALSE)) %>% print()
diagram3 = "digraph ProcessMap {\n    rankdir=LR; \n    node [shape=box, style=\"filled\", fillcolor=\"lightblue\"];\n     \"Triage IOA\" -> \"Examen CC\" [label=\"100\", penwidth=4];   \"Examen CC\" -> \"Sortie Domicile\" [label=\"100\", penwidth=4];\n}"
cat(diagram3)
grViz(diagram3)

simple_trace <- tibble(trace_id = NULL, activity_seq = NULL, activity = NULL)


# warning : super slow implementation in R
# (use C/C++/Julia for better perfs)
# here it is done only for pedagogical demonstration
for(i in 1:200) {
  selected_pathway_id = sum(runif(1) > cumsum(ground_truth_process_model_pathways_df$pathway_probability)) + 1
  # selected_pathway_id = 3 # for test
  selected_pathway = ground_truth_process_model_pathways_df$pathway[selected_pathway_id]
  
  
  
  activity_0 = ground_truth_process_model_pathways_df$starting_activity[selected_pathway_id]
  
  activity_1 = (ground_truth_process_model_transitions_df %>%
    filter(from_activity == activity_0,
           pathway == selected_pathway) %>%
    mutate(dice = runif(1)) %>%
    filter(dice < cumsum(transition_probability)))$to_activity[1]
  
  activity_2 = (ground_truth_process_model_transitions_df %>%
    filter(from_activity == activity_1,
           pathway == selected_pathway) %>%
    mutate(dice = runif(1)) %>%
    filter(dice < cumsum(transition_probability)))$to_activity[1]
  
  activity_3 = (ground_truth_process_model_transitions_df %>%
    filter(from_activity == activity_2,
           pathway == selected_pathway) %>%
    mutate(dice = runif(1)) %>%
    filter(dice < cumsum(transition_probability)))$to_activity[1]
  
  simple_trace <- simple_trace %>%
    rbind(
      tibble(id = c(i,i,i,i),
             activity_seq = c(1,2,3,4),
             activity = c(activity_0,
                              activity_1,
                              activity_2,
                              activity_3))
    )
  print(i)
}


# minimalist markov model

simple_transition_df <- simple_trace %>%
  group_by(id) %>%
  arrange(id, activity_seq) %>%
  mutate(from_activity = activity,
            to_activity = lead(activity)) %>%
  ungroup() %>%
  select(from_activity,
         to_activity) %>%
  filter(!is.na(to_activity)) %>%
  count(from_activity,
        to_activity)

markov_model_data <- simple_transition_df %>%
    group_by(from_activity) %>%
    mutate(transition_probability = n / sum(n)) %>%
    mutate(weight = transition_probability,
           selected = FALSE)
# A tibble: 8 × 6
# Groups:   from_activity [4]
#   from_activity to_activity          n transition_probability weight selected
#   <chr>         <chr>            <int>                  <dbl>  <dbl> <lgl>   
# 1 Examen CC     Post Soins          59                 0.496  0.496  FALSE   
# 2 Examen CC     Sortie Domicile     60                 0.504  0.504  FALSE   
# 3 Examen CL     Post Soins          81                 1      1      FALSE   
# 4 Post Soins    Sortie Domicile     67                 0.479  0.479  FALSE   
# 5 Post Soins    Sortie Transfert    10                 0.0714 0.0714 FALSE   
# 6 Post Soins    Sortie UHCD         63                 0.45   0.45   FALSE   
# 7 Triage IOA    Examen CC          119                 0.595  0.595  FALSE   
# 8 Triage IOA    Examen CL           81                 0.405  0.405  FALSE 

# diagram4 <- generate_transition_diagram(markov_model_data) %>% print()
diagram4 <-"digraph ProcessMap {\n    rankdir=LR; \n    node [shape=box, style=\"filled\", fillcolor=\"lightblue\"];\n     \"Examen CC\" -> \"Post Soins\" [label=\"59\", penwidth=2.48739495798319];   \"Examen CC\" -> \"Sortie Domicile\" [label=\"60\", penwidth=2.51260504201681];   \"Examen CL\" -> \"Post Soins\" [label=\"81\", penwidth=3.04201680672269];   \"Post Soins\" -> \"Sortie Domicile\" [label=\"67\", penwidth=2.6890756302521];   \"Post Soins\" -> \"Sortie Transfert\" [label=\"10\", penwidth=1.25210084033613];   \"Post Soins\" -> \"Sortie UHCD\" [label=\"63\", penwidth=2.58823529411765];   \"Triage IOA\" -> \"Examen CC\" [label=\"119\", penwidth=4];   \"Triage IOA\" -> \"Examen CL\" [label=\"81\", penwidth=3.04201680672269];\n}"
cat(diagram4)
grViz(diagram4)

# interpolation model

simple_trace %>%
  group_by(id) %>%
  arrange(id, activity_seq) %>%
  filter(!is.na(activity)) %>%
  summarize(family = str_c(activity,collapse = " -> "),
            .groups = 'drop') %>%
  count(family) %>%
  mutate(proba = n/sum(n))

# interpolation model (observed family probas)
# A tibble: 7 × 3
#   family                                                        n proba
#   <chr>                                                     <int> <dbl>
# 1 Triage IOA -> Examen CC -> Post Soins -> Sortie Domicile     46 0.23 
# 2 Triage IOA -> Examen CC -> Post Soins -> Sortie Transfert     3 0.015
# 3 Triage IOA -> Examen CC -> Post Soins -> Sortie UHCD         10 0.05 
# 4 Triage IOA -> Examen CC -> Sortie Domicile                   60 0.3  
# 5 Triage IOA -> Examen CL -> Post Soins -> Sortie Domicile     21 0.105
# 6 Triage IOA -> Examen CL -> Post Soins -> Sortie Transfert     7 0.035
# 7 Triage IOA -> Examen CL -> Post Soins -> Sortie UHCD         53 0.265

# "true" family probas
# 1 Triage IOA -> Examen CC -> Post Soins -> Sortie Domicile      0.24 # 0.3*0.8 
# 2 Triage IOA -> Examen CC -> Post Soins -> Sortie Transfert     0.006 # 0.3*0.02 
# 3 Triage IOA -> Examen CC -> Post Soins -> Sortie UHCD          0.054 # 0.3*0.18 
# 4 Triage IOA -> Examen CC -> Sortie Domicile                    0.3 # 0.3*1 
# 5 Triage IOA -> Examen CL -> Post Soins -> Sortie Domicile      0.08 # 0.4*0.2
# 6 Triage IOA -> Examen CL -> Post Soins -> Sortie Transfert     0.032 # 0.4 * 0.08
# 7 Triage IOA -> Examen CL -> Post Soins -> Sortie UHCD          0.288 # 0.4 * 0.72

# markov model probas
# 1 Triage IOA -> Examen CC -> Post Soins -> Sortie Domicile      0.141 # 0.595*0.496*0.479
# 2 Triage IOA -> Examen CC -> Post Soins -> Sortie Transfert     0.021 # 0.595*0.496*0.0714
# 3 Triage IOA -> Examen CC -> Post Soins -> Sortie UHCD          0.133 # 0.595*0.496*0.45
# 4 Triage IOA -> Examen CC -> Sortie Domicile                    0.300 # 0.595*0.504 
# 5 Triage IOA -> Examen CL -> Post Soins -> Sortie Domicile      0.193 # 0.405*1*0.479
# 6 Triage IOA -> Examen CL -> Post Soins -> Sortie Transfert     0.029 # 0.405*1*0.0714
# 7 Triage IOA -> Examen CL -> Post Soins -> Sortie UHCD          0.182 # 0.405*1*0.45

# comparing each model side by side

# 1 Triage IOA -> Examen CC -> Post Soins -> Sortie Domicile     46 0.23 
# 1 Triage IOA -> Examen CC -> Post Soins -> Sortie Domicile      0.24 # 0.3*0.8 
# 1 Triage IOA -> Examen CC -> Post Soins -> Sortie Domicile      0.141 # 0.595*0.496*0.479

# 2 Triage IOA -> Examen CC -> Post Soins -> Sortie Transfert     3 0.015
# 2 Triage IOA -> Examen CC -> Post Soins -> Sortie Transfert     0.006 # 0.3*0.02
# 2 Triage IOA -> Examen CC -> Post Soins -> Sortie Transfert     0.021 # 0.595*0.496*0.0714

# 3 Triage IOA -> Examen CC -> Post Soins -> Sortie UHCD         10 0.05 
# 3 Triage IOA -> Examen CC -> Post Soins -> Sortie UHCD          0.054 # 0.3*0.18 
# 3 Triage IOA -> Examen CC -> Post Soins -> Sortie UHCD          0.133 # 0.595*0.496*0.45

# 4 Triage IOA -> Examen CC -> Sortie Domicile                   60 0.3  
# 4 Triage IOA -> Examen CC -> Sortie Domicile                    0.3 # 0.3*1 
# 4 Triage IOA -> Examen CC -> Sortie Domicile                    0.300 # 0.595*0.504 

# 5 Triage IOA -> Examen CL -> Post Soins -> Sortie Domicile     21 0.105
# 5 Triage IOA -> Examen CL -> Post Soins -> Sortie Domicile      0.193 # 0.405*1*0.479
# 5 Triage IOA -> Examen CL -> Post Soins -> Sortie Domicile      0.08 # 0.4*0.2

# 6 Triage IOA -> Examen CL -> Post Soins -> Sortie Transfert     7 0.035
# 6 Triage IOA -> Examen CL -> Post Soins -> Sortie Transfert     0.029 # 0.405*1*0.0714
# 6 Triage IOA -> Examen CL -> Post Soins -> Sortie Transfert     0.032 # 0.4 * 0.08

# 7 Triage IOA -> Examen CL -> Post Soins -> Sortie UHCD         53 0.265
# 7 Triage IOA -> Examen CL -> Post Soins -> Sortie UHCD          0.288 # 0.4 * 0.72
# 7 Triage IOA -> Examen CL -> Post Soins -> Sortie UHCD          0.182 # 0.405*1*0.45

# interpolation and markov model are at two extreme of process modeling
#
# the interpolation model allows for maximum fit with the observed data but 
#  will become very heavy as the number of circuit / family increase exponentially
#  with the number of activities and won't be able to generalize well on 
#  non observed data / test data and will also fail to consider properly very rare
#  circuits / families such as family 2 here :
#  (2 Triage IOA -> Examen CC -> Post Soins -> Sortie Transfert)
#  with only 3 observed cases
#
# the minimalist markov model presented here allows for a very compact model
#  that can consider many families / circuits (even some that would not exists)
#  but tends to make huge approximation error on their probabilities
#
# Constructing a proper process model is trying to find a model that is compact,
#  generalisable, and that can seperate the different type of path. Ideally, here
#  you would want to refind the "ground truth model" using a cluster analysis
#  of process pathways (notably if you don't have access to pathway label data).
#  Here, for example it would be logical to process pathway with Examen CC and 
#  Examen CL differently as they concern patients that are very different 
#
#
# To compare the models statistically, you would use a log-likelihood measure on
# on test data, e.g. different from the training from which you would have 
# learned your model







