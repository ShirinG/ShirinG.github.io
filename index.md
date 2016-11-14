---
layout: page
title: Shirin's playgRound
tagline: exploring and playing with data in R
---
{% include JB/setup %}

![]({{ site.url }}/assets/images/doublehelix.png)

<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>

-----

Also check out [R-bloggers](http://www.R-bloggers.com) for lots of cool R stuff!
