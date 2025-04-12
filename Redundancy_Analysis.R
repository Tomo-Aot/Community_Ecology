# Redundancy Analysis
library(tidyverse)
library(vegan)
library(ggtext)
library(showtext)
library(ggpubr)
library(magick)

font_add_google(name = "Noto Sans", family = "ns")
theme_pubr(base_size = 10, base_family = "ns") |> 
  theme_set()
showtext_auto()

data(dune)
data(dune.env)

res_mat = dune |> scale(scale = TRUE, center = TRUE)
res_mat = dune |> decostand(method = "hellinger")

deco = decorana(res_mat)

deco = scores(deco, by = "site")

ggplot() + 
  geom_point(
    aes(x = deco[, 1], y = deco[, 2])
  ) + 
  scale_x_continuous(
    name = "DCA1"
  ) + 
  scale_y_continuous(
    name = "DCA2"
  )

# height = 80
# width = height
# pdfname = "./image/dca_plot.pdf"
# pngname = str_replace(pdfname, "pdf", "png")
# ggsave(pdfname, height = height, width = width, units = "mm")
# image_read_pdf(pdfname, density = 300) |>
#   image_write(pngname)

dune.env = dune.env |> as_tibble() |> 
  mutate(Manure = as.double(Manure),
         Moisture = as.double(Moisture))

# introduce Dummy variables
use = 
  model.matrix(~ dune.env$Use - 1, data = dune.env) |> as_tibble() |> 
  mutate(
    Usehf = str_remove(`dune.env$UseHayfield`, "dune.env//$"),
    Usehp = str_remove(`dune.env$UseHaypastu`, "dune.env//$"),
    Usepa = str_remove(`dune.env$UsePasture`, "dune.env//$"),
  ) |> 
    select(Usehf, Usehp, Usepa)

mana = 
  model.matrix(~ dune.env$Management - 1, data = dune.env) |> 
  as_tibble() |> 
  mutate(
    BF = str_remove(`dune.env$ManagementBF`, "dune.env//$"),
    HF = str_remove(`dune.env$ManagementHF`, "dune.env//$"),
    NM = str_remove(`dune.env$ManagementNM`, "dune.env//$"),
    SF = str_remove(`dune.env$ManagementSF`, "dune.env//$"),
  ) |> 
    select(BF, HF, NM, SF)

dune.env = dune.env |> 
  bind_cols(use, mana) |> 
  select(-c(Management, Use))

# the RDA models
null_model = res_mat ~ 1
model = res_mat ~ 
  dune.env$A1 + 
  dune.env$Moisture + 
  dune.env$HF + 
  dune.env$BF + 
  dune.env$NM + 
  dune.env$Usehf + 
  dune.env$Usehp + 
  dune.env$Manure

null_result = rda(
  null_model
)

result = rda(
  model
      )

vif.cca(result)

value = result$CCA$eig / sum(result$CCA$eig)

score = scores(result, display = "site")

anova(result, by = "axis", permutations = 999)
anova(result, by = "term", permutations = 999)

anova.cca(result, permutations = 999)

env_vector = function(rda_model){
  env_vector = 
    vegan::scores(rda_model, display = "bp", choices = 1:length(rda_model$CCA$eig))
  
  env_vector = env_vector |> 
    as_tibble() |> 
    mutate(factor = rownames(env_vector)) |>
    mutate(factor = str_remove(factor, "dune.env\\$"))
  
  return(env_vector)
}

eig_val = env_vector(result)

col = dune |> as_tibble() |> 
  colnames()

div = dune |> as_tibble() |> 
  mutate(ID = row_number()) |> 
  pivot_longer(
    cols = col,
    names_to = "Species",
    values_to = "value"
  ) |> 
  group_by(ID) |> 
  summarise(
    div = diversity(value, index = "shannon")
  )


ggplot() + 
  geom_point(
    aes(x = RDA1, y = RDA2, colour = div$div),
    data = score,
    alpha = 0.6,
    size = 2
  ) + 
  geom_segment(
    aes(x = 0, xend = RDA1 * 1.5, y = 0, yend = RDA2 * 1.5),
    data = eig_val,
    arrow = arrow(length = unit(2, "mm"))
  ) + 
  geom_richtext(
    aes(x = RDA1 * 1.5, y = RDA2 * 1.5, label = factor),
    data = eig_val,
    colour = "black",
    label.colour = NA,
    fill = NA,
    size = 2,
    vjust = ifelse(eig_val$RDA2 <= 0, 1, 0)
  ) + 
  scale_x_continuous(
    name = str_c("RDA1 (", round(value[1] * 100, 2), "%)"),
    limits = c(-2, 2),
    breaks = seq(-2, 2, length = 5)
  ) + 
  scale_y_continuous(
    name = str_c("RDA2 (", round(value[2] * 100, 2), "%)"),
    limits = c(-2, 2),
    breaks = seq(-2, 2, length = 5)
  ) + 
  theme(
    legend.title = element_blank(),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    legend.background = element_blank(),
    legend.direction = "vertical"
  ) + 
  scale_colour_viridis_c(end = 0.8)

# height = 80
# width = height
# pdfname = "./image/RDA_plot.pdf"
# pngname = str_replace(pdfname, "pdf", "png")
# ggsave(pdfname, height = height, width = width, units = "mm")
# image_read_pdf(pdfname, density = 300) |>
#   image_write(pngname)
