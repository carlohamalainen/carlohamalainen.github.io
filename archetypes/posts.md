---
draft: true
date: {{ .Date }}
title: {{ replace .File.ContentBaseName `-` ` ` | title }}
author: "Carlo Hamalainen"
url: /{{ time.Now.Format "2006-01-02" }}/{{ .File.ContentBaseName }}
---