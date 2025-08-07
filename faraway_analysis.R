# 2025-02-05

library(faraway)
library(tidyverse)
library(vegan)
library(stats)
library(ggtext)
library(magick)


# 保存用の図の設定をしておきます。
height = 80
width = height

# farawayパッケージのgalaの解析を行います。
# galaはガラパゴス諸島における生物種に関するデータフレームです。

# Rで使うときはtibbleにします。
# こうすることで変数の型がわかりやすいです。
# 島の名前の変数名がないので追加します。
df = gala |> 
  as_tibble(rownames = "Island")

# 島の面積と植物の種数について考える。
df |> 
  ggplot() + 
  geom_point(
    aes(x = Area, y = Species)
  )

# save image
pdfname = "./image/area_sp.pdf"
pngname = str_replace(pdfname, "pdf", "png")
ggsave(filename = pdfname, height = height, width = width,
       units = "mm")
image_read_pdf(pdfname, density = 300) |> 
  image_write(pngname)

# この図を見た漢字、島の面積はかなり幅があるので、
# 常用対数に変換したほうが使いやすそうです。
# 種数も常用対数に変換してみました。
# きれいな相関関係が示せそうですね
df |> 
  ggplot() + 
  geom_point(
    aes(x = log10(Area), y = log10(Species))
  )

pdfname = "./image/logrea_logsp.pdf"
pngname = str_replace(pdfname, "pdf", "png")
ggsave(filename = pdfname, height = height, width = width,
       units = "mm")

image_read_pdf(pdfname, density = 300) |> 
  image_write(pngname)

# この図を見ると種数は島の面積の累乗と関係がありそうです。
# しまの面積が大きくなるほど種数のばらつきが大きくなっているように見えますが、
# 一旦、このデータでモデルを作成します。
# さきに常用対数に変換した島の面積と生物種数の列を追加しておきます(便利)
df = df |> 
  mutate(logarea = log10(Area),
         logsp = log10(Species))

# Hypothesis #################################################
# 島に生息する生物の種数は島の面積が大きくなると増加する
# 正規分布を仮定するので、一般線形モデル
m0 = glm(formula = logsp ~ 1, data = df, family = gaussian("identity"))
m1 = glm(formula = logsp ~ logarea, data = df,
         family = gaussian("identity"))

# それから、今回は検定を行わないので、帰無モデルも作成しません。
# 次に、作成したモデルの期待値を計算します。

pdata = expand(data = df,
       logarea = seq(min(logarea), max(logarea), length = 20))

tmp = predict(m1, se.fit = TRUE, newdata = pdata) |> 
  as_tibble()

pdata = bind_cols(pdata, tmp)

figure = df |> 
  ggplot() + 
  geom_point(
    aes(x = logarea, y = logsp)
  ) + 
  geom_line(
    aes(x = logarea, y = fit),
    data = pdata
  ) + 
  geom_ribbon(
    aes(x = logarea, ymin = fit - se.fit, ymax = fit + se.fit),
    data = pdata,
    alpha = 0.3
  )

# save image
pdfname = "./image/logarea_logsp_model.pdf"
pngname = str_replace(pdfname, "pdf", "png")
ggsave(filename = pdfname, height = height, width = width,
       units = "mm")
image_read_pdf(pdfname, density = 300) |> 
  image_write(pngname)

# 一見いい感じに見えるモデルですが、適切なモデルかどうかを
# モデルの統計量と診断図を作成して確かめます。
summary = summary(m1)
summary$coefficients
summary$aic

plot(model, which = 1)

# QQプロット
# 図の中の点線に点が重なっていると正規分布に従っている。
# このモデルの場合、7,10,13は少し点線から外れているように見えるが、
# この程度であれば正規性に問題はない
plot(model, which = 2)

# 
plot(model, which = 3)

# クックの距離
# 一般的に0.5を超えるとモデルとして適切ではない可能性がある。
plot(model, which = 4)
plot(model, which = 5)


# 発表などで紹介する際は、図に統計量も載せておくと親切
# ここでは、モデルの自由度調整済み決定係数を記載する
# はじめに、記載する統計量をtibbleにまとめます
m1_s = summary(m1)

m0_dev = deviance(m0)
m1_dev = deviance(m1)

R_sqr = 1 - (m1_dev / m0_dev)

n = nrow(df)
p = length(coef(m1))

adj_R_sqr = 1 - ((1 - R_sqr) * (n - 1) / (n - p - 1))

p3 = df |> 
  ggplot() + 
  geom_ribbon(
    aes(x = logarea, ymin = fit - se.fit, ymax = fit + se.fit),
    data = pdata,
    alpha = 0.3
  ) + 
  geom_line(
    aes(x = logarea, y = fit),
    data = pdata
  ) + 
  geom_point(
    aes(x = logarea, y = logsp)
  ) + 
  geom_richtext(
    aes(x = 2, y = 0.4, label = str_c("R<sup>2</sup>=", round(adj_R_sqr, 3))),
    data = NULL,
    hjust = 0,
    vjust = 0.5,
    text.colour = "black",
    colour = NA,
    label.colour = NA,
    fill = NA
  ) + 
  scale_x_continuous(
    name = "log10(Area)"
  ) + 
  scale_y_continuous(
    name = "log10(Species)"
  ) + 
  theme(
    axis.ticks = element_line(colour = "black")
  )



