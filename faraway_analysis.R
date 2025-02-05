# 2025-02-05

library(faraway)
library(tidyverse)
library(vegan)
library(stats)


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

# この図を見た漢字、島の面積はかなり幅があるので、
# 常用対数に変換したほうが使いやすそうです。
# 種数も常用対数に変換してみました。
# きれいな相関関係が示せそうですね
df |> 
  ggplot() + 
  geom_point(
    aes(x = log10(Area), y = log10(Species))
  )

# この図を見ると種数は島の面積の累乗と関係がありそうです。
# しまの面積が大きくなるほど種数のばらつきが大きくなっているように見えますが、
# 一旦、このデータでモデルを作成します。
# さきに常用対数に変換した面積の列を追加しておきます(便利)
df = df |> 
  mutate(logarea = log10(Area))

model = glm(formula = log10(Species) ~ logarea, data = df, 
    family = gaussian("identity"))

# それから、今回は検定を行わないので、帰無モデルも作成しません。
# 次に、作成したモデルの期待値を計算します。

pdata = expand(data = df,
       logarea = seq(min(logarea), max(logarea), length = 20))

tmp = predict(model, se.fit = TRUE, newdata = pdata) |> 
  as_tibble()


pdata = bind_cols(pdata, tmp)


df |> 
  ggplot() + 
  geom_point(
    aes(x = logarea, y = log10(Species))
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

# 一見いい感じに見えるモデルですが、適切なモデルかどうかを
# モデルの統計量と診断図を作成して確かめます。
summary = summary(model)

summary$coefficients


