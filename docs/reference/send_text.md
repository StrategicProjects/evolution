# Send a plain text message

Sends a plain text WhatsApp message using Evolution API v2.

## Usage

``` r
send_text(
  client,
  number,
  text,
  delay = NULL,
  link_preview = NULL,
  mentions_everyone = NULL,
  mentioned = NULL,
  quoted = NULL,
  verbose = FALSE
)
```

## Arguments

- client:

  An
  [`evo_client()`](https://strategicprojects.github.io/evolution/reference/evo_client.md)
  object.

- number:

  Character. Recipient number with country code (e.g., `"5581999990000"`
  or `"+5581999990000"`).

- text:

  Character. Message body.

- delay:

  Integer (ms). Optional presence delay before sending. Simulates typing
  before the message is sent.

- link_preview:

  Logical. Enable URL link preview in the message.

- mentions_everyone:

  Logical. Mention everyone in a group.

- mentioned:

  Character vector of JIDs to mention (e.g., `jid("+5581999990000")`).

- quoted:

  Optional list with Baileys message `key` and `message` to reply to a
  specific message.

- verbose:

  Logical. If `TRUE`, logs request/response details with **cli**.

## Value

A named list parsed from the JSON response returned by Evolution API,
containing the message `key` (with `remoteJid`, `fromMe`, `id`),
`message`, `messageTimestamp`, and `status`. The HTTP status code is
stored in `attr(result, "http_status")`.

## See also

[`send_media()`](https://strategicprojects.github.io/evolution/reference/send_media.md),
[`send_location()`](https://strategicprojects.github.io/evolution/reference/send_location.md),
[`jid()`](https://strategicprojects.github.io/evolution/reference/jid.md)

## Examples

``` r
if (FALSE) { # \dontrun{
client <- evo_client("https://my-host", Sys.getenv("EVO_APIKEY"), "myInst")
send_text(client, "5581999990000", "Hello from R!", verbose = TRUE)
} # }
```
