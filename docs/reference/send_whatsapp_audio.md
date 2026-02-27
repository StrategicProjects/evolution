# Send WhatsApp audio (voice note)

Sends an audio message (push-to-talk / voice note) via Evolution API v2.

## Usage

``` r
send_whatsapp_audio(
  client,
  number,
  audio,
  delay = NULL,
  quoted = NULL,
  verbose = FALSE
)
```

## Arguments

- client:

  An
  [`evo_client()`](https://monitoramento.sepe.pe.gov.br/evolution/reference/evo_client.md)
  object.

- number:

  Character. Recipient number with country code (e.g., `"5581999990000"`
  or `"+5581999990000"`).

- audio:

  URL, base64-encoded audio, or local file path (auto-encoded to
  base64). Supports `~` expansion.

- delay:

  Integer (ms). Optional presence delay before sending. Simulates typing
  before the message is sent.

- quoted:

  Optional list with Baileys message `key` and `message` to reply to a
  specific message.

- verbose:

  Logical. If `TRUE`, logs request/response details with **cli**.

## Value

A named list with the API response. The HTTP status code is stored in
`attr(result, "http_status")`.

## Examples

``` r
if (FALSE) { # \dontrun{
send_whatsapp_audio(client, "5581999990000",
                    audio = "https://example.com/note.ogg")
} # }
```
