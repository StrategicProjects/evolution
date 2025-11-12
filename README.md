
<!-- README.md is generated from README.Rmd. Please edit that file -->

# evolution <a href="https://github.com/StrategicProjects/evolution/"><img src="man/figures/logo.png" align="right" height="107" alt="evolution website" /></a>

<!-- badges: start -->

![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/evolution) 
![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/evolution) 
![License](https://img.shields.io/badge/license-MIT-darkviolet.svg) 
![](https://img.shields.io/badge/devel%20version-0.0.1-orangered.svg)

<!-- badges: end -->

> R wrapper for [Evolution API v2](https://evoapicloud.com) — a
> lightweight WhatsApp API.

## Overview

**evolution** is a tidy-style R client for the **Evolution API v2**, a
RESTful platform that enables automation across WhatsApp (via
Web/Baileys or Cloud API modes) and other channels. It wraps HTTP calls
using **{httr2}**, exposes **snake_case** helper functions, and
integrates structured logging with **{cli}** for use in modern R
pipelines and production ETL tasks.

With `evolution` you can:

- Send plain text, media (image/video/document), WhatsApp audio,
  stickers, or status (stories)
- Send locations, contacts, buttons, or polls
- Check if a number is on WhatsApp
- Use `options(evolution.timeout)` to control timeouts and
  `verbose = TRUE` for detailed logs

> **Note:** This package is an independent wrapper for the Evolution API
> and is not affiliated with WhatsApp or Meta.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("StrategicProjects/evolution")
```

## Quick Start

``` r
library(evolution)

client <- evo_client(
  base_url = "https://YOUR-HOST",
  api_key  = Sys.getenv("EVO_APIKEY"),
  instance = "yourInstance"
)

# Optional: set a global timeout
options(evolution.timeout = 60)

# Example: Send a simple message
send_text(client, "+5581999990000", "Hello from R!", verbose = TRUE)
```

## Functions Overview

| Function | Description | Key Arguments |
|----|----|----|
| `evo_client()` | Creates preconfigured client | `base_url`, `api_key`, `instance` |
| `send_text()` | Sends plain text message | `number`, `text`, `delay`, `verbose` |
| `send_status()` | Sends status (text or media) | `type`, `content`, `caption`, `verbose` |
| `send_media()` | Sends image/video/document (URL, base64, or file) | `number`, `mediatype`, `mimetype`, `media`, `file_name` |
| `send_whatsapp_audio()` | Sends voice note (PTT) | `number`, `audio`, `verbose` |
| `send_sticker()` | Sends sticker (URL or base64) | `number`, `sticker`, `verbose` |
| `send_location()` | Sends location pin | `number`, `latitude`, `longitude`, `name` |
| `send_contact()` | Sends one or more contacts (auto wuid) | `number`, `contact`, `verbose` |
| `send_reaction()` | Sends emoji reaction to message | `key`, `reaction`, `verbose` |
| `send_buttons()` | Sends message with interactive buttons | `number`, `buttons`, `verbose` |
| `send_poll()` | Sends a poll | `number`, `name`, `values`, `verbose` |
| `check_is_whatsapp()` | Verifies numbers | `numbers` |
| `jid()` | Builds WhatsApp JID | `number` |

## Examples

### Send Text

``` r
send_text(client, "+5581999990000", "Hello world!", delay = 120, link_preview = FALSE, verbose = TRUE)
```

### Send Media

``` r
# URL
send_media(client, "+5581999990000", "image", "image/png",
            "https://www.r-project.org/logo/Rlogo.png", "Rlogo.png",
            caption = "R Logo", verbose = TRUE)

# Local File
send_media(client, "+5581999990000", "document", "application/pdf",
            media = "report.pdf", file_name = "report.pdf",
            caption = "Monthly Report", verbose = TRUE)
```

### Send Contact

``` r
send_contact(client, "+5581999990000",
             contact = list(
               fullName = "Jane Doe",
               phoneNumber = "+5581999990000",
               organization = "Company Ltd.",
               email = "jane@example.com",
               url = "https://company.com"
             ),
             verbose = TRUE)
```

### Send Location

``` r
send_location(
  client,
  number = "+5581999990000",
  latitude = -8.05,
  longitude = -34.88,
  name = "Recife Antigo",
  address = "Marco Zero - Recife/PE",
  verbose = TRUE
)
```

## Configuration

- Timeout: controlled by `options(evolution.timeout)` (default 60
  seconds)
- Verbose mode: `verbose = TRUE` enables detailed CLI + httr2 logs
- Error handling: HTTP status \>= 400 triggers error message via
  `req_error()`

## Advanced Tips

- **Base64**: remove `data:*;base64,` prefix and all line breaks before
  sending
- **Contact wuid**: automatically generated as `<digits>@s.whatsapp.net`
- **Security**: store your API key securely using `.Renviron`
- **Debugging**: compare verbose output with a working curl request

## Contributing

Contributions are welcome! Open issues with reproducible examples and
sanitized logs.

## License

MIT © 2025 Andre Leite, Hugo Vasconcelos & Diogo Bezerra  
See [LICENSE](LICENSE) for details.
