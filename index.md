---
layout: page
title: Shirin's R playgRound
tagline: exploring and playing with data in R
---
{% include JB/setup %}

<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>

## About me

See my [Xing](http://www.xing.com/profile/Shirin_Glander) or [Linkedin](http://de.linkedin.com/in/shirin-glander-01120881) profiles for more information.

<img src="{{ site.url }}/assets/images/Bewerbungsfoto.jpg" alt="My photo" width="200">