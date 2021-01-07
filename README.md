# Land doesn't vote, people do.
This is a version of the famous visualization [«Land doesn't vote, people do»](https://themorningnews.org/p/the-history-of-the-map-behind-land-doesnt-vote-people-do) by [Karim Douïeb](https://twitter.com/karim_douieb) for Switzerland written in [R](https://www.r-project.org/). It shows the results of the popular initiative [«For responsible businesses – protecting human rights and the environment»](https://www.admin.ch/gov/en/start/documentation/votes/20201129/iniziativa-popolare-per-imprese-responsabili-a-tutela-dell-essere-umano-e-dell-ambiente.html) at municipal level. All [data used in this process](https://opendata.swiss/de/dataset/echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen) was retrieved via the Swiss Open Government Data platform [opendata.swiss](https://opendata.swiss). The code used can be found [here](https://github.com/zumbov2/votemapswitzerland/blob/main/votemapswitzerland.R).  

<img src="https://github.com/zumbov2/votemapswitzerland/blob/main/animation1.gif" width="600">  

## Start vs. End (with legend)
<img src="https://github.com/zumbov2/votemapswitzerland/blob/main/steps1.png" width="600">  

# Alternative visualisation
An alternative animation (here with the results of the [Amendment of the Hunting Act](https://www.admin.ch/gov/en/start/documentation/votes/20200927/amendment-of-the-hunting-act.html)) can be replicated with the help of script [votemapswitzerland2.R](https://github.com/zumbov2/votemapswitzerland/blob/main/votemapswitzerland2.R)

<img src="https://github.com/zumbov2/votemapswitzerland/blob/main/animation2.gif" width="600">  

## Steps
<img src="https://github.com/zumbov2/votemapswitzerland/blob/main/steps2.png" width="600">  

# Links
## Packages involved
* Data: [`swissdd`](https://github.com/politanch/swissdd)
* Spatial data operations: [`sf`](https://github.com/r-spatial/sf)
* Visualization: [`ggplot2`](https://github.com/tidyverse/ggplot2) and [`hrbrthemes`](https://github.com/hrbrmstr/hrbrthemes)
* Animation: [`transformr`](https://github.com/thomasp85/transformr), [`tweenr`](https://github.com/thomasp85/tweenr), and [`particles`](https://github.com/thomasp85/particles)

## "Media"
* [R Weekly 2020-W51](https://rweekly.org/2020-51.html)
* [R Weekly 2021-W01](https://rweekly.org/2021-W01.html)
* [R Weekly Podcast, Issue 2021-W01 Highlights](https://rweekly.fireside.fm/21)
* [reddit](https://www.reddit.com/r/dataisbeautiful/comments/knfbvp/oc_dont_trust_a_choropleth_mapevidence_from/)
* [twitter](https://twitter.com/DavidZumbach/status/1344547411985911808)

