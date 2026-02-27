# Send media (image, video, document)

Sends an image, video, or document via Evolution API v2. The `media`
argument is flexible: it accepts an HTTP(S) URL, a local file path
(auto-encoded to base64), raw base64, or a data-URI.

## Usage

``` r
send_media(
  client,
  number,
  mediatype,
  mimetype,
  media,
  file_name,
  caption = NULL,
  delay = NULL,
  link_preview = NULL,
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

- mediatype:

  One of `"image"`, `"video"`, `"document"`.

- mimetype:

  MIME type string, e.g., `"image/png"`, `"video/mp4"`,
  `"application/pdf"`.

- media:

  The media content. Can be: (a) an HTTP/HTTPS URL; (b) a local file
  path; (c) raw base64; or (d) a data-URI (`data:*;base64,...`).

- file_name:

  Suggested filename for the recipient (should match the MIME type,
  e.g., `"report.pdf"`).

- caption:

  Optional caption text displayed with the media.

- delay:

  Integer (ms). Optional presence delay before sending. Simulates typing
  before the message is sent.

- link_preview:

  Logical. Enable URL link preview in the message.

- verbose:

  Logical. If `TRUE`, logs request/response details with **cli**.

## Value

A named list with the API response. The HTTP status code is stored in
`attr(result, "http_status")`.

## Examples

``` r
if (FALSE) { # \dontrun{
# From URL
send_media(client, "5581999990000", "image", "image/png",
           media = "https://www.r-project.org/logo/Rlogo.png",
           file_name = "Rlogo.png", caption = "R Logo")

# From local file
send_media(client, "5581999990000", "document", "application/pdf",
           media = "report.pdf", file_name = "report.pdf")
} # }
```
