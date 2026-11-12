leipzig <- function(kayser_fleischer, neurological_symptoms,
                    ceruloplasmin, haemolytic_anemia,
                    liver_copper, rhodaine_granules = FALSE,
                    urinary_copper, penicillamine = FALSE,
                    mutated_chromosomes){
  
  
  # Input validation
  
  # Missing + warning
  
  
  
  # Kayser fleicher score
  kf_score <- rep(NA_real_, length(kayser_fleischer))
  kf_score[which(kayser_fleischer)] <- 2
  kf_score[which(!kayser_fleischer)] <- 0
  
  #Neurological symptoms:
  neuro_score <- rep(NA_real_, length(neurological_symptoms))
  neuro_score[which(neurological_symptoms == "severe")] <- 2
  neuro_score[which(neurological_symptoms == "mild")] <- 1
  neuro_score[which(neurological_symptoms == "absent")] <- 0
  
  # Ceruloplasming
  cerulo_score <- rep(NA_real_, length(ceruloplasmin))
  cerulo_score[which(ceruloplasmin > 0.2)] <- 0
  cerulo_score[which(ceruloplasmin <= 0.2 & ceruloplasmin >= 0.1)] <- 1
  cerulo_score[which(ceruloplasmin < 0.1)] <- 2
  
  # Coombs negative haemolytic anemia
  anemia_score <- rep(NA_real_, length(haemolytic_anemia))
  anemia_score[which(haemolytic_anemia)] <- 1
  anemia_score[which(!haemolytic_anemia)] <- 0
  
  # liver copper
  lcopper_score <- rep(NA_real_, length(liver_copper))
  lcopper_score[which(rhodaine_granules)] <- 1
  lcopper_score[which(!rhodaine_granules & liver_copper > 4)] <- 2
  lcopper_score[which(!rhodaine_granules &liver_copper >= 0.8 & liver_copper <= 4)] <- 1
  lcopper_score[which(!rhodaine_granules &liver_copper < 0.8 )] <- -1
  
  
  # urinary copper
  ucopper_score <- rep(NA_real_, length(urinary_copper))
  ucopper_score[which(penicillamine)] <- 2
  ucopper_score[which(!penicillamine & urinary_copper < 1)] <- 0
  ucopper_score[which(!penicillamine & urinary_copper >= 1 & urinary_copper<= 2)] <- 1
  ucopper_score[which(!penicillamine & urinary_copper > 2)] <- 2
  
  # Mutation 
  mutation_score <- rep(NA_real_, length(mutated_chromosomes))
  mutation_score[mutated_chromosomes == 0] <- 0
  mutation_score[mutated_chromosomes == 1] <- 1
  mutation_score[mutated_chromosomes == 2] <- 4
  
  
  score <- kf_score + neuro_score + cerulo_score +
    anemia_score + lcopper_score +
    ucopper_score + mutation_score
  
  score
}