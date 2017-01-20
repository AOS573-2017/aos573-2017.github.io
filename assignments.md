---
layout: page
title: Assignments
permalink: /assignments/
---
<ol>
{% for assignment in site.assignments %}
  {% if assignment.published %}
    <li><a href="{{assignment.url}}">{{ assignment.title }}</a></li>
  {% endif %}
{% endfor %}
</ol>
