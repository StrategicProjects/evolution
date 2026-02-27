# Send a sticker

Sends a sticker image via Evolution API v2.

## Usage

``` r
send_sticker(client, number, sticker, delay = NULL, verbose = FALSE)
```

## Arguments

- client:

  An
  [`evo_client()`](https://strategicprojects.github.io/evolution/reference/evo_client.md)
  object.

- number:

  Character. Recipient number with country code (e.g., `"5581999990000"`
  or `"+5581999990000"`).

- sticker:

  URL, base64-encoded sticker image, or local file path (auto-encoded to
  base64). Supports `~` expansion.

- delay:

  Integer (ms). Optional presence delay before sending. Simulates typing
  before the message is sent.

- verbose:

  Logical. If `TRUE`, logs request/response details with **cli**.

## Value

A named list with the API response. The HTTP status code is stored in
`attr(result, "http_status")`.

## Examples

``` r
if (FALSE) { # \dontrun{
send_sticker(client, "5581999990000",
             sticker = "https://example.com/sticker.webp")
} # }
```
