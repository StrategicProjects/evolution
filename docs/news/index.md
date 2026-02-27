# Changelog

## evolution (development version)

## evolution 0.1.0

### Breaking changes

- The global timeout option was renamed from `evoapi.timeout` to
  `evolution.timeout` to match the package name. The previous option
  name was silently ignored due to a mismatch between `zzz.R` and the
  internal request function
  ([\#1](https://github.com/StrategicProjects/evolution/issues/1)).

### New features

- [`send_list()`](https://strategicprojects.github.io/evolution/reference/send_list.md):
  sends interactive list messages with sections and selectable rows,
  mapping to the Evolution API v2 `sendList` endpoint.

- [`check_is_whatsapp()`](https://strategicprojects.github.io/evolution/reference/check_is_whatsapp.md):
  verifies whether phone numbers are registered on WhatsApp via the
  `chat/whatsappNumbers` endpoint.

- `print.evo_client()`: new S3 print method that displays instance name
  and base URL when inspecting a client object.

### Improved error handling

- [`.evo_post()`](https://strategicprojects.github.io/evolution/reference/dot-evo_post.md)
  now extracts and surfaces the actual API error message from the JSON
  response body (fields `response$message` or `message`), instead of
  showing a generic “Evolution API error (status)” string.

- Non-JSON error responses (e.g., HTML from a 502/503 gateway error) are
  now handled gracefully instead of crashing with a parse error.

- All `send_*()` functions now validate required arguments up front with
  clear [cli](https://cli.r-lib.org) error messages (e.g.,
  `"'number' must be a single non-empty character string"`).

- [`evo_client()`](https://strategicprojects.github.io/evolution/reference/evo_client.md)
  uses descriptive
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  messages instead of
  [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html), with hints
  like `"Use Sys.getenv(\"EVO_APIKEY\")"` for the API key argument.

### Improved logging (verbose mode)

- Verbose output now includes response timing (e.g.,
  `✔ HTTP 201 (0.34s)`).

- Verbose output now shows a preview of the response body (first 500
  characters) to help with debugging.

- Large base64 `media` fields are truncated in verbose logs to keep
  output readable.

### Bug fixes

- [`jid()`](https://strategicprojects.github.io/evolution/reference/jid.md)
  now strips all non-digit characters (including `+`) before appending
  `@s.whatsapp.net`. Previously, `jid("+5581...")` produced an invalid
  JID with the `+` preserved.

- [`send_list()`](https://strategicprojects.github.io/evolution/reference/send_list.md):
  `footer` parameter now defaults to `""` instead of `NULL`, because the
  Evolution API requires `footerText` to be present in the request body
  (HTTP 400 otherwise).

- [`send_sticker()`](https://strategicprojects.github.io/evolution/reference/send_sticker.md)
  and
  [`send_whatsapp_audio()`](https://strategicprojects.github.io/evolution/reference/send_whatsapp_audio.md)
  now pass media through
  [`.normalize_media()`](https://strategicprojects.github.io/evolution/reference/dot-normalize_media.md),
  enabling local file paths (e.g., `"~/Downloads/sticker.webp"`) to be
  auto-encoded to base64. Previously only
  [`send_media()`](https://strategicprojects.github.io/evolution/reference/send_media.md)
  supported this.

- [`.normalize_media()`](https://strategicprojects.github.io/evolution/reference/dot-normalize_media.md)
  now calls [`path.expand()`](https://rdrr.io/r/base/path.expand.html)
  so that `~` in file paths is correctly resolved to the user’s home
  directory.

- [`send_whatsapp_audio()`](https://strategicprojects.github.io/evolution/reference/send_whatsapp_audio.md)
  removed unused parameters (`link_preview`, `mentions_everyone`,
  `mentioned`) that are not part of the `sendWhatsAppAudio` API
  endpoint.

- [`send_buttons()`](https://strategicprojects.github.io/evolution/reference/send_buttons.md)
  now emits a
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html) at
  runtime alerting that interactive buttons are **not supported** on the
  Baileys (WhatsApp Web) connector and may be discontinued. The warning
  suggests
  [`send_poll()`](https://strategicprojects.github.io/evolution/reference/send_poll.md)
  as an alternative.

- [`send_list()`](https://strategicprojects.github.io/evolution/reference/send_list.md)
  now emits a
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html) at
  runtime alerting that interactive list messages are **not supported**
  on the Baileys (WhatsApp Web) connector and may be discontinued. The
  warning suggests
  [`send_poll()`](https://strategicprojects.github.io/evolution/reference/send_poll.md)
  as an alternative.

### Documentation

- Fixed author name typo in DESCRIPTION: “Vaconcelos” → “Vasconcelos”.

- README now accurately lists all exported functions including
  [`send_list()`](https://strategicprojects.github.io/evolution/reference/send_list.md)
  and
  [`check_is_whatsapp()`](https://strategicprojects.github.io/evolution/reference/check_is_whatsapp.md).

- README includes new sections: verbose output example, configuration
  table, and
  [`send_list()`](https://strategicprojects.github.io/evolution/reference/send_list.md)
  /
  [`check_is_whatsapp()`](https://strategicprojects.github.io/evolution/reference/check_is_whatsapp.md)
  usage examples.

- All roxygen documentation improved with `@description`, `@examples`,
  `@seealso` cross-references, and more specific `@return` descriptions.

- Added `jsonlite` to `Imports` (used for response preview in verbose
  mode).

## evolution 0.0.1

CRAN release: 2025-11-20

- Initial CRAN release.
- Core messaging functions:
  [`send_text()`](https://strategicprojects.github.io/evolution/reference/send_text.md),
  [`send_status()`](https://strategicprojects.github.io/evolution/reference/send_status.md),
  [`send_media()`](https://strategicprojects.github.io/evolution/reference/send_media.md),
  [`send_whatsapp_audio()`](https://strategicprojects.github.io/evolution/reference/send_whatsapp_audio.md),
  [`send_sticker()`](https://strategicprojects.github.io/evolution/reference/send_sticker.md),
  [`send_location()`](https://strategicprojects.github.io/evolution/reference/send_location.md),
  [`send_contact()`](https://strategicprojects.github.io/evolution/reference/send_contact.md),
  [`send_reaction()`](https://strategicprojects.github.io/evolution/reference/send_reaction.md),
  [`send_buttons()`](https://strategicprojects.github.io/evolution/reference/send_buttons.md),
  [`send_poll()`](https://strategicprojects.github.io/evolution/reference/send_poll.md).
- [`evo_client()`](https://strategicprojects.github.io/evolution/reference/evo_client.md)
  factory with httr2 retry and apikey header.
- [`jid()`](https://strategicprojects.github.io/evolution/reference/jid.md)
  helper for building WhatsApp JIDs.
