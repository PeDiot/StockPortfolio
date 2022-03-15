---
marp: true
author: Pierre-Emmanuel Diot
theme: gaia
_class: lead
paginate: true
backgroundColor: #fff
---

![bg left:30% 80%](home_img.png)

# Your Stock Portfolio 
### Gestion d'actifs financiers avec `shiny`

--- 

# Motivations du projet 

- Suivre l'évolution d'un portefeuille d'actions / cryptos **au jour le jour**
<br>
- Proposer un outil **centralisé**, *user-friendly* et visuel
<br>
- Analyser la **performance** de chaque action via des indicateurs financiers 
<br>
- **Prédire** l'évolution du portefeuille sous 5 jours à l'aide d'un réseau de neurones

--- 

# Toolbox

<br>


**R** : `shiny`, `plotly`, `tidyquant`, `TTR`, `reticulate`

<br>
<br>

**Python** : `tensorflow`, `pandas`, `pyreadr`

--- 

# Fonctionnalités de l'app

<style>
img[alt~="center"] {
  display: block;
  margin: 0 auto;
}
</style>

![fg width:900px center](app.png)

---

# Indicateurs financiers

<br>

**Moyenne mobile sur les $k$ derniers jours d'une série à $T$ valeurs**

$$MA_k = \frac{1}{k} \sum_{t = n - k + 1}^{T} p_t$$


**Rendements cumulés** 

$$R = \Pi_{t=1}^T \ r_t = \Pi_{t=1}^{T-1} \text{ avec } r_t = \frac{p_{t+1} - p_t}{p_t}$$


---

# Indicateurs financiers

<br>

**Candletstick** : graphique affichant les prix d'ouverture (*open*), de fermeture (***close***), le plus haut (*high*) et le plus bas (*low*)

<br>

**Bandes de Bollinger** : mesure de la **volatilité** d'une action via la moyenne mobile à 20 jours et un intervalle de confiance à $\pm$ 2 écarts-types à la moyenne

---

# Indicateurs financiers

<br>

**MACD** (Moving Average Convergence Divergence)
- Mesure de la **tendance** d'un cours 
<br>
- Différence entre 2 moyennes mobiles de court terme et long terme
<br>
- Comparaison à une ligne de signal $\rightarrow$ identifier les **signaux d'achat/vente**

---

# Indicateurs financiers

<br>

**RSI** (Relative Strength Index) 
- Mesure de **l'ampleur** de la variation récente d'un cours 
<br>
- Indicateur compris entre 0 et 100
<br>
- Identifier si la valeur est *trop achetée* (RSI > 70) ou *trop vendue* (RSI < 30)

---

# Prédire l'évolution du cours 

#### Réseaux de neurones

#### Combiner Python & R via `reticulate`

--- 

# Conclusion

#### Apports du projet

- Outil d'**aide à la décision** simple à utiliser et épuré 
<br>
- Données mises à jour **quotidiennement** et sans stockage nécessaire (API)
<br>
- Diversité d'indicateurs
<br>
- Aller plus loin via la **prédiction** sous 5 jours

--- 

# Conclusion

#### Axes d'amélioration

- Laisser l'utilisateur choisir ses propres actions/cryptos 
<br>
- Déployer l'app sur shinyapps avec les prédictions 
<br>
- Améliorer la performance de l'app 

---

# Github 

![fg width:400px center](qrCode.png)

