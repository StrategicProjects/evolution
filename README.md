
<!-- README.md is generated from README.Rmd. Please edit that file -->

# evolution <a href="https://github.com/StrategicProjects/evolution/"><img src="man/figures/logo.png" align="right" height="107" alt="evolution website" /></a>

<!-- badges: start -->

![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/evolution) 
![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/evolution) 
![License](https://img.shields.io/badge/license-MIT-darkviolet.svg) 
![](https://img.shields.io/badge/devel%20version-0.1.0-orangered.svg)

<!-- badges: end -->

> R wrapper for [Evolution API v2](https://doc.evolution-api.com) — a
> lightweight WhatsApp integration API.

## Overview

**evolution** is a tidy-style R client for the **Evolution API v2**, a
RESTful platform that enables automation across WhatsApp (via
Web/Baileys or Cloud API modes). It wraps HTTP calls using **{httr2}**,
exposes **snake_case** helper functions, and integrates structured
logging with **{cli}** for use in modern R pipelines and production
tasks.

With `evolution` you can:

- Send plain text, media (image/video/document), WhatsApp audio,
  stickers, or status stories
- Send locations, contacts, interactive buttons, polls, or list messages
- Check if numbers are registered on WhatsApp
- Use `options(evolution.timeout)` to control timeouts and
  `verbose = TRUE` for detailed CLI logs with timing

> **Note:** This package is an independent wrapper for the Evolution API
> and is not affiliated with WhatsApp or Meta.

## Installation

``` r
# From CRAN
install.packages("evolution")

# Or the development version from GitHub
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

# Optional: set a global timeout (default: 60s)
options(evolution.timeout = 30)

# Send a simple message
send_text(client, "5581999990000", "Hello from R!", verbose = TRUE)
```

## Functions Overview

| Function | Description | Key Arguments |
|----|----|----|
| `evo_client()` | Creates preconfigured API client | `base_url`, `api_key`, `instance` |
| `send_text()` | Sends plain text message | `number`, `text`, `delay`, `verbose` |
| `send_status()` | Posts a status/story (text or media) | `type`, `content`, `caption`, `verbose` |
| `send_media()` | Sends image/video/document (URL, base64, or file) | `number`, `mediatype`, `mimetype`, `media`, `file_name` |
| `send_whatsapp_audio()` | Sends voice note (PTT) | `number`, `audio`, `verbose` |
| `send_sticker()` | Sends sticker (URL or base64) | `number`, `sticker`, `verbose` |
| `send_location()` | Sends location pin | `number`, `latitude`, `longitude`, `name` |
| `send_contact()` | Sends one or more contacts (auto wuid) | `number`, `contact`, `verbose` |
| `send_reaction()` | Sends emoji reaction to a message | `key`, `reaction`, `verbose` |
| `send_buttons()` | Sends message with interactive buttons ⚠️ | `number`, `buttons`, `verbose` |
| `send_poll()` | Sends a poll | `number`, `name`, `values`, `verbose` |
| `send_list()` | Sends an interactive list message ⚠️ | `number`, `sections`, `button_text` |
| `check_is_whatsapp()` | Checks if numbers are on WhatsApp | `numbers` |
| `jid()` | Builds WhatsApp JID from phone number | `number` |

> ⚠️ **`send_buttons()` and `send_list()` — Baileys users:** Interactive
> buttons and list messages are **not supported** on the Baileys
> (WhatsApp Web) connector and are likely to be discontinued by
> Evolution API. Use `send_poll()` instead. These endpoints work only on
> the **Cloud API** connector.

> 💡 **Local file support:** `send_media()`, `send_sticker()`, and
> `send_whatsapp_audio()` accept local file paths (including `~/...`) —
> files are auto-encoded to base64 via `{base64enc}`.

## Examples

### Send Text

``` r
send_text(client, "5581999990000", "Hello world!",
          delay = 1200, link_preview = FALSE, verbose = TRUE)
```

### Send Media

``` r
# From URL
send_media(client, "5581999990000", "image", "image/png",
           media = "https://www.r-project.org/logo/Rlogo.png",
           file_name = "Rlogo.png", caption = "R Logo")

# From local file
send_media(client, "5581999990000", "document", "application/pdf",
           media = "report.pdf", file_name = "report.pdf",
           caption = "Monthly Report")
```

### Send Contact

``` r
send_contact(client, "5581999990000",
  contact = list(
    fullName     = "Jane Doe",
    phoneNumber  = "+5581999990000",
    organization = "Company Ltd.",
    email        = "jane@example.com",
    url          = "https://company.com"
  ))
```

### Send Location

``` r
send_location(client, "5581999990000",
  latitude = -8.05, longitude = -34.88,
  name = "Recife Antigo", address = "Marco Zero - Recife/PE")
```

### Send List

``` r
send_list(client, "5581999990000",
  title = "Our Menu",
  description = "Select from the options below:",
  button_text = "View options",
  sections = list(
    list(title = "Drinks", rows = list(
      list(title = "Coffee", description = "Hot coffee", rowId = "1"),
      list(title = "Tea",    description = "Green tea",  rowId = "2")
    ))
  ))
```

### Check WhatsApp Numbers

``` r
check_is_whatsapp(client, c("5581999990000", "5511988887777"))
```

## Configuration

| Option | Default | Description |
|----|----|----|
| `evolution.timeout` | `60` | HTTP request timeout in seconds |
| `verbose = TRUE` | per-call | Enables CLI logging with timing, request body, and response preview |

## Verbose Output

When `verbose = TRUE`, every function call logs structured diagnostics:

    ── evoapi POST message/sendText/myInstance ──
    ℹ Timeout: 60s
    ℹ Body:
      List of 2
       $ number: chr "5581999990000"
       $ text  : chr "Hello from R!"
    ✔ HTTP 201 (0.34s)
    ℹ Content-Type: application/json
    ℹ Response: {"key":{"remoteJid":"5581999990000@s.whatsapp.net", ...}

If an API error occurs, you get an actionable message:

    Error in `.evo_post()`:
    ✖ Evolution API returned HTTP 400.
    ℹ Endpoint: POST message/sendText/myInstance
    ! API message: instance requires property "number"

## Security Tips

- **Never hardcode API keys.** Use `Sys.getenv("EVO_APIKEY")` or
  `.Renviron`.
- **Base64 media**: the package auto-strips `data:*;base64,` prefixes.
- **Contact wuid**: automatically generated as
  `<digits>@s.whatsapp.net`.
- **Debugging**: use `verbose = TRUE` and compare with a working `curl`
  request.

## Contributing

Contributions are welcome! Open issues with reproducible examples and
sanitised logs (remove API keys and phone numbers).

## License

MIT © 2025 Andre Leite, Hugo Vasconcelos & Diogo Bezerra See
[LICENSE](LICENSE) for details.
