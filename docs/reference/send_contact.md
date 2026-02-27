# Send a WhatsApp contact (auto-generate wuid)

Sends one or more contacts following the Evolution API v2 format.
Automatically generates the `wuid` field as `<digits>@s.whatsapp.net`
from each contact's phone number (or from `number` if not provided).

## Usage

``` r
send_contact(client, number, contact, verbose = FALSE)
```

## Arguments

- client:

  An
  [`evo_client()`](https://monitoramento.sepe.pe.gov.br/evolution/reference/evo_client.md)
  object.

- number:

  Recipient number (e.g., `"5581999990000"`).

- contact:

  Either:

  - a named list with fields `fullName`, `phoneNumber`, `organization`,
    `email`, `url`; or

  - a list of such lists (to send multiple contacts). The `wuid` field
    will be auto-generated if missing.

- verbose:

  Logical; if `TRUE`, shows detailed logs.

## Value

Parsed JSON response as list (see
[`.evo_post()`](https://monitoramento.sepe.pe.gov.br/evolution/reference/dot-evo_post.md)
for details).

## Examples

``` r
if (FALSE) { # \dontrun{
send_contact(client, "5581999990000",
  contact = list(
    fullName     = "Jane Doe",
    phoneNumber  = "+5581999990000",
    organization = "Company Ltd.",
    email = "jane@example.com",
    url   = "https://company.com"
  ))
} # }
```
