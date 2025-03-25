# Community Ecology

群集生態学用の解析手法を共有します。
ここに書いているスクリプトは R で使用することができます。


## Generalized Linear Model

faraway パッケージの付属データセット
gala を用いて、一般化線形モデルを作成します。
一般化線形モデルは解析手法としてあまり一般的なイメージはないですが、
ある 1 つの目的変数を説明する際に使われることがあります。
gala はガラパゴス諸島における生物種をまとめたデータセットです。
はじめに、島の大きさと生物種の関係について調べてみます。

<img src="https://raw.githubusercontent.com/Tomo-Aot/Community_Ecology/refs/heads/main/image/area_sp.png" width="400">

しかし、この図だと島の大きさと生物種の関係が分かりにくいです。
なので、島の大きさと生物種を常用対数に変換してみます。

<img src="https://raw.githubusercontent.com/Tomo-Aot/Community_Ecology/refs/heads/main/image/logrea_logsp.png" width = "400">

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

複数間の群集間の多様性は β 多様度 (Beta-Diversity)
といいます。
β 多様度は様々な指標で表され、
群集同士がどれくらい異なるか（もしくは似ているか）を計算します。
そのため、群集の非類似度 (Dissimilarity)
や類似度 (Similarity)
と呼ばれることもあります。

β 多様度は生息地間の多様性の指標でもあるため、
群集生態学における重要な要素です。

## Redundancy Analysis (RDA)



## Permutational Analysis of Variance (PERMANOVA)


