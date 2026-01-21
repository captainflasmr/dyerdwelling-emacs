---
title: "OpenWebUI setup"
author: ["James Dyer"]
lastmod: 2025-02-01T10:16:00+00:00
tags: [2025]
categories: ["linux", "open-source"]
draft: true
thumbnail: "/linux/20250201101602-emacs--OpenWebUI setup.jpg"
---

docker pull ghcr.io/open-webui/open-webui:main

Now, spin up the Open WebUI container:

docker run -d \\
  -p 3000:8080 \\
  --add-host=host.docker.internal:host-gateway \\
  -v open-webui:/app/backend/data \\
  --name open-webui \\
  --restart always \\
  ghcr.io/open-webui/open-webui:main

Now, open your web browser and navigate to: <http://localhost:3000>

Ollama runs at:

<http://localhost:11434>

docker run -d -p 3000:8080 --add-host=host.docker.internal:host-gateway -v open-webui:/app/backend/data --name open-webui --restart always ghcr.io/open-webui/open-webui:main
