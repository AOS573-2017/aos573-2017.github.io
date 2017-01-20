---
layout: page
title: Lectures
permalink: /lectures/
---

<ol>
{% for lecture in site.lectures %}
  {% if lecture.published %}
    <li><a href="{{ lecture.url }}">{{ lecture.title }}</a></li>
  {% endif %}
{% endfor %}
</ol>
