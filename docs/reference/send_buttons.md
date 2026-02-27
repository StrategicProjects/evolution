# Send interactive buttons

Sends a message with interactive buttons via Evolution API v2.

## Usage

``` r
send_buttons(
  client,
  number,
  title,
  description,
  footer,
  buttons,
  delay = NULL,
  link_preview = NULL,
  mentions_everyone = NULL,
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

- title:

  Character. Button message title.

- description:

  Character. Button message description/body.

- footer:

  Character. Footer text.

- buttons:

  List of buttons. Each button should be a named list following the API
  specification (see Evolution API docs).

- delay:

  Integer (ms). Optional presence delay before sending. Simulates typing
  before the message is sent.

- link_preview:

  Logical. Enable URL link preview in the message.

- mentions_everyone:

  Logical. Mention everyone in a group.

- verbose:

  Logical. If `TRUE`, logs request/response details with **cli**.

## Value

A named list with the API response. The HTTP status code is stored in
`attr(result, "http_status")`.

## Note

**Baileys connector:** Interactive buttons are **not supported** on the
Baileys (WhatsApp Web) connector and are likely to be discontinued. This
endpoint is fully supported only on the **Cloud API** connector. If you
are on Baileys, consider using
[`send_poll()`](https://monitoramento.sepe.pe.gov.br/evolution/reference/send_poll.md)
as an alternative.

## See also

[`send_poll()`](https://monitoramento.sepe.pe.gov.br/evolution/reference/send_poll.md)

## Examples

``` r
if (FALSE) { # \dontrun{
send_buttons(client, "5581999990000",
  title = "Choose",
  description = "Pick an option:",
  footer = "Powered by R",
  buttons = list(
    list(type = "reply", title = "Option A"),
    list(type = "reply", title = "Option B")
  ))
} # }
```
