# Community Ecology

群集生態学用の解析手法を共有します。
ここに書いているスクリプトはRで使用することができます。


## Generalized Linear Model
faraway パッケージの付属データセット
gala を用いて、一般化線形モデルを作成します。
このデータセットはガラパゴス諸島における生物種をまとめています。
はじめに、島の大きさと生物種の関係について調べてみます。

![Fig. 1. ガラパゴス諸島における島の大きさと生物の多様性の関係](https://raw.githubusercontent.com/Tomo-Aot/Community_Ecology/refs/heads/main/image/area_sp.png)

しかし、この図だと島の大きさと生物種の関係が分かりにくいです。
なので、島の大きさと生物種を常用対数に変換してみます。

![Fig. 2. 常用対数に変換したガラパゴス諸島における島の大きさと生物の多様性の関係](https://raw.githubusercontent.com/Tomo-Aot/Community_Ecology/refs/heads/main/image/logrea_logsp.png)

2つの変数を常用対数に変換するとわかりやすい。
この図を参考にモデルを作成しようと思います。
今回は、モデルの作成とその妥当性の検討を目的とします。

ここでは、正規分布を仮定した一般化線形モデルを作成します。
正規分布を仮定しているので一般線形モデルでも同じですが、
種数を扱う場合はポアソン分布も使う可能性があるので、
今回は一般化線形モデルを選択しています。

```
glm(log10(Species) ~ log10(Area), data = df, family = gaussian("identity"))
```

次に、作成したモデルの期待値を計算します。

```
pdata = expand(data = df,
       logarea = seq(min(log10(Area)), max(log10(Area)), length = 20))

tmp = predict(model, se.fit = TRUE, newdata = pdata) |> 
  as_tibble()
```




## Communities Dissimilarity


## PERMANOVA

