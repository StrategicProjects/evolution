# Send a WhatsApp Status (story)

Posts a status (story) message visible to your contacts. Supports text
or media (image, video, document, audio) types.

## Usage

``` r
send_status(
  client,
  type = c("text", "image", "video", "document", "audio"),
  content,
  caption = NULL,
  background_color = NULL,
  font = NULL,
  all_contacts = FALSE,
  status_jid_list = NULL,
  verbose = FALSE
)
```

## Arguments

- client:

  An
  [`evo_client()`](https://monitoramento.sepe.pe.gov.br/evolution/reference/evo_client.md)
  object.

- type:

  One of `"text"`, `"image"`, `"video"`, `"document"`, `"audio"`.

- content:

  Text (for `type = "text"`) or URL/base64 for media.

- caption:

  Optional caption for media types.

- background_color:

  Hex colour for text status background (e.g., `"#FF5733"`).

- font:

  Integer font id (0–14).

- all_contacts:

  Logical. If `TRUE`, sends to all contacts.

- status_jid_list:

  Optional character vector of specific JIDs to receive the status.

- verbose:

  Logical. If `TRUE`, logs request/response details with **cli**.

## Value

A named list with the API response. The HTTP status code is stored in
`attr(result, "http_status")`.

## Examples

``` r
if (FALSE) { # \dontrun{
send_status(client, type = "text", content = "Hello from R!",
            background_color = "#317873", font = 2, all_contacts = TRUE)
} # }
```
