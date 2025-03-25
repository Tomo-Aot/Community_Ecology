# Redundancy Analysis
library(tidyverse)
library(vegan)
library(ggtext)
library(showtext)
library(ggpubr)


res_mat = dune |> scale(scale = TRUE, center = TRUE)

# the RDA models
model = rda(res_mat ~ 
      dune.env$A1 + 
      dune.env$Moisture + 
      dune.env$Use + 
      dune.env$Management + 
      dune.env$Manure
      )

eig_val = model$CCA$eig / sum(model$CCA$eig)

score = scores(model, display = "site")

anova.cca(model)

env_vector = function(rda_model){
  env_vector = 
    vegan::scores(rda_model, display = "bp", choices = 1:length(rda_model$CCA$eig))
  
  env_vector = env_vector |> 
    as_tibble() |> 
    mutate(factor = rownames(env_vector))
  
  return(env_vector)
}

eig_val = env_vector(model)

ggplot() + 
  geom_point(
    aes(x = RDA1, y = RDA2, colour = dune.env$Use),
    data = score
  ) + 
  geom_segment(
    aes(x = 0, xend = RDA1 * 2, y = 0, yend = RDA2 * 2),
    data = eig_val,
    arrow = arrow(length = unit(2, "mm"))
  )


