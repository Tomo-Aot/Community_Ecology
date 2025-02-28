# 冗長性分析 (Redundancy Analysis; RDA)
# packages
library(tidyverse)
library(vegan)
library(emmeans)

# vector of environmental variables
env_vector = function(PCA_Results){
  env_vector = 
    vegan::scores(PCA_Results, display = "bp")
  
  colnames(env_vector) = c("xend", "yend")  # 環境ベクトルの列名を設定
  
  env_vector = env_vector |> 
    as_tibble() |> 
    mutate(factor = rownames(env_vector))
  
  return(env_vector)
}

