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
                     "Inapropriate Pathway"),
         pathway_probability = c(0.4,0.3,0.3),
         starting_activity = c("Triage IOA",
                               "Triage IOA",
                               "Triage IOA"))
ground_truth_process_model_pathways_df

ground_truth_process_model_transitions_df <- tibble(pathway = "",
      from_activity = "",
      to_activity = "",
      transition_probability = 0) %>%
  rbind(list("Urgent Pathway","Triage IOA","Examen Init CL",1),
        list("Urgent Pathway","Examen Init CL","Post Soins",0.04),
        list("Urgent Pathway","Examen Init CL","Examen Radio",0.8),
        list("Urgent Pathway","Examen Init CL","Analyse",0.16),
        list("Urgent Pathway","Examen Radio","Analyse",0.8),
        list("Urgent Pathway","Examen Radio","Post Soins",0.2),
        list("Urgent Pathway","Analyse","Post Soins",1),
        list("Urgent Pathway","Post Soins","Sortie Domicile",0.2),
        list("Urgent Pathway","Post Soins","Sortie UHCD",0.72),
        list("Urgent Pathway","Post Soins","Sortie Transfert",0.08),
        list("Average Pathway","Triage IOA","Examen Init CC",1),
        list("Average Pathway","Examen Init CC","Post Soins",0.25),
        list("Average Pathway","Examen Init CC","Examen Radio",0.5),
        list("Average Pathway","Examen Init CC","Analyse",0.25),
        list("Average Pathway","Examen Radio","Analyse",0.5),
        list("Average Pathway","Examen Radio","Post Soins",0.5),
        list("Average Pathway","Analyse","Post Soins",1),
        list("Average Pathway","Post Soins","Sortie Domicile",0.8),
        list("Average Pathway","Post Soins","Sortie UHCD",0.18),
        list("Average Pathway","Post Soins","Sortie Transfert",0.02),
        list("Inapropriate Pathway","Triage IOA","Examen Init CC",1),
        list("Inapropriate Pathway","Examen Init CC","Sortie Domicile",1))
ground_truth_process_model_transitions_df

# on considère qu'on a pu retrouver le ground truth modèle à partir de données ...
# on s'en sert pour générer les circuits / pathway

simple_trace <- tibble(trace_id = NULL, activity_seq = NULL, activity = NULL)

# warning : super slow implementation in R
# (use C/C++/Julia for better perfs)
# here it is done only for pedagogical demonstration
for(i in 1:500) {
  selected_pathway_id = sum(runif(1) > cumsum(ground_truth_process_model_pathways_df$pathway_probability)) + 1
  # selected_pathway_id = 2 # for test
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
  
  activity_4 = (ground_truth_process_model_transitions_df %>%
    filter(from_activity == activity_3,
           pathway == selected_pathway) %>%
    mutate(dice = runif(1)) %>%
    filter(dice < cumsum(transition_probability)))$to_activity[1]
  
  activity_5 = (ground_truth_process_model_transitions_df %>%
    filter(from_activity == activity_4,
           pathway == selected_pathway) %>%
    mutate(dice = runif(1)) %>%
    filter(dice < cumsum(transition_probability)))$to_activity[1]
  
  simple_trace <- simple_trace %>%
    rbind(
      tibble(id = c(i,i,i,i,i,i),
             activity_seq = c(1,2,3,4,5,6),
             activity = c(activity_0,
                              activity_1,
                              activity_2,
                              activity_3,
                              activity_4,
                              activity_5))
    )
  print(i)
}

write_csv2(simple_trace %>% filter(!is.na(activity)),
           "patient_arrival.csv")

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
#diagram4 <- generate_transition_diagram(markov_model_data) %>% print()
#diagram4 <- "digraph ProcessMap {\n    rankdir=LR; \n    node [shape=box, style=\"filled\", fillcolor=\"lightblue\"];\n     \"Analyse\" -> \"Post Soins\" [label=\"242\", penwidth=3.36482084690554];   \"Examen Init CC\" -> \"Analyse\" [label=\"37\", penwidth=1.36156351791531];   \"Examen Init CC\" -> \"Examen Radio\" [label=\"76\", penwidth=1.74267100977199];   \"Examen Init CC\" -> \"Post Soins\" [label=\"36\", penwidth=1.35179153094463];   \"Examen Init CC\" -> \"Sortie Domicile\" [label=\"158\", penwidth=2.54397394136808];   \"Examen Init CL\" -> \"Analyse\" [label=\"36\", penwidth=1.35179153094463];   \"Examen Init CL\" -> \"Examen Radio\" [label=\"153\", penwidth=2.49511400651466];   \"Examen Init CL\" -> \"Post Soins\" [label=\"4\", penwidth=1.03908794788274];   \"Examen Radio\" -> \"Analyse\" [label=\"169\", penwidth=2.6514657980456];   \"Examen Radio\" -> \"Post Soins\" [label=\"60\", penwidth=1.58631921824104];   \"Post Soins\" -> \"Sortie Domicile\" [label=\"163\", penwidth=2.5928338762215];   \"Post Soins\" -> \"Sortie Transfert\" [label=\"18\", penwidth=1.17589576547231];   \"Post Soins\" -> \"Sortie UHCD\" [label=\"161\", penwidth=2.57328990228013];   \"Triage IOA\" -> \"Examen Init CC\" [label=\"307\", penwidth=4];   \"Triage IOA\" -> \"Examen Init CL\" [label=\"193\", penwidth=2.88599348534202];\n}"
cat(diagram4)
grViz(diagram4)


mu=10
CV=0.5

# alpha or k <-> shape = mu
# beta or 1/theta <-> rate = 1/(mu*CV^2)
mean(rgamma(100,shape = 4,rate=1/2.5))
sd(rgamma(100,shape = 4,rate=1/2.5))


# caractéristique de performances
# Triage IOA
# acteur : IOA
# mu = 6/h 
# CV = 1

# Examen Init CL
# acteur Medecin CL
# mu = 2/h 
# CV = 2.5

# Examen Init CC
# acteur Medecin CC
# mu = 3/h 
# CV = 0.7

# Examen Radio
# acteur : Radiologue
# mu = 5/h 
# CV = 4

# Analyse
# acteur : Infirmière (circuits confondus)
# mu = 4/h 
# CV = 0.3

# Post Soins
# acteur : Médecin CC ou CL
# mu = 4/h
# CV = 1


# flux d'arrivées
# IOA 1*lambda
# Médecin CC pour examen init 0.6*lambda
# Médecin CL pour examen init 0.4*lambda
# Radiologue (0.4*0.8 + 0.3*0.5)*lambda = 0.47*lambda
# Infirmière pour analyse 0.47*lambda
# Médecin pour post soins 0.6*lambda

# en considérant une configuration avec 1 IOA 1 Médecin CC 1 Médecin CL 1 Radiologue 1 Infirmière
# Quels sont le(s) goulet(s) d'étranglement ?
# C-A-D Quel lambda (initial) max peut supporter les acteurs
# lambda_max_IOA = 1/1*(1/mu_IOA)= mu_IOA/1 = 6/h
# lambda_max_MedCC = mu_MedCC/0.6 = 3/0.6 = 5/h
# lambda_max_MedCL = mu_MedCL/0.4 = 2/0.4 = 5/h
# lambda_max_Radiologue = mu_Radiologue/0.47 = 5/0.47 = 10.6383/h
# lambda_max_Analyse = mu_analyse/0.47 = 4/0.47 = 8.5/h
# lambda_max_Post_Soins = mu_post_soins/0.6 = 4/0.6 = 6.67/h
# lambda_max_MedCC_MedCL = 2/(0.6*(1/mu_MedCC) + 0.4*(1/mu_MedCL) + 0.6*(1/mu_PostSoins))
#      2/(0.6/3+0.4/2+0.6/4) = 2/(0.2+0.2+0.15) = 2/0.55 = 3.636/h
#      (there are 2 servers here)

# Le point d'engorgement est clairement sur les médecins

