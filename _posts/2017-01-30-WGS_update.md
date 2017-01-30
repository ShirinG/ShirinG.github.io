---
layout: post
title: "New features in World Gender Statistics app"
author: Shirin Glander
date: 2017-01-30
categories: shiny
tags: shiny ggplot2
image: shiny/2017/01/29/wgs_app.png
---

[In my last post](https://shiring.github.io/shiny/2017/01/29/WGS), I built a [shiny app to explore World Gender Statistics](https://shiring.shinyapps.io/wgs_app/).

To make it a bit nicer and more convenient, I added a few more features:

-   The drop-down menu for Years is now reactive, i.e. it only shows options with data (all NA years are removed)
-   You can click on any country on the map to get information about which country it is, its population size, income group and region
-   Below the world map, you can look at timelines for male vs female values for each country
-   The map is now in Mercator projection
