# evolution (development version)

# evolution 0.1.0

## Breaking changes

* The global timeout option was renamed from `evoapi.timeout` to
 `evolution.timeout` to match the package name. The previous option name
 was silently ignored due to a mismatch between `zzz.R` and the internal
 request function (#1).

## New features

* `send_list()`: sends interactive list messages with sections and
  selectable rows, mapping to the Evolution API v2 `sendList` endpoint.

* `check_is_whatsapp()`: verifies whether phone numbers are registered
  on WhatsApp via the `chat/whatsappNumbers` endpoint.

* `print.evo_client()`: new S3 print method that displays instance name
  and base URL when inspecting a client object.

## Improved error handling

* `.evo_post()` now extracts and surfaces the actual API error message
  from the JSON response body (fields `response$message` or `message`),
  instead of showing a generic "Evolution API error (status)" string.

* Non-JSON error responses (e.g., HTML from a 502/503 gateway error) are
  now handled gracefully instead of crashing with a parse error.

* All `send_*()` functions now validate required arguments up front with
  clear `{cli}` error messages (e.g.,
 `"'number' must be a single non-empty character string"`).

* `evo_client()` uses descriptive `cli::cli_abort()` messages instead of
 `stopifnot()`, with hints like
 `"Use Sys.getenv(\"EVO_APIKEY\")"` for the API key argument.

## Improved logging (verbose mode)

* Verbose output now includes response timing
  (e.g., `✔ HTTP 201 (0.34s)`).

* Verbose output now shows a preview of the response body (first 500
  characters) to help with debugging.

* Large base64 `media` fields are truncated in verbose logs to keep
  output readable.

## Bug fixes
 
* `jid()` now strips all non-digit characters (including `+`) before
  appending `@s.whatsapp.net`. Previously, `jid("+5581...")` produced
  an invalid JID with the `+` preserved.

* `send_list()`: `footer` parameter now defaults to `""` instead of
  `NULL`, because the Evolution API requires `footerText` to be present
  in the request body (HTTP 400 otherwise).

* `send_sticker()` and `send_whatsapp_audio()` now pass media through
  `.normalize_media()`, enabling local file paths (e.g.,
  `"~/Downloads/sticker.webp"`) to be auto-encoded to base64. Previously
  only `send_media()` supported this.

* `.normalize_media()` now calls `path.expand()` so that `~` in file
  paths is correctly resolved to the user's home directory.

* `send_whatsapp_audio()` removed unused parameters (`link_preview`,
 `mentions_everyone`, `mentioned`) that are not part of the
 `sendWhatsAppAudio` API endpoint.

* `send_buttons()` now emits a `cli::cli_warn()` at runtime alerting
  that interactive buttons are **not supported** on the Baileys
  (WhatsApp Web) connector and may be discontinued. The warning
  suggests `send_poll()` as an alternative.

* `send_list()` now emits a `cli::cli_warn()` at runtime alerting
  that interactive list messages are **not supported** on the Baileys
  (WhatsApp Web) connector and may be discontinued. The warning
  suggests `send_poll()` as an alternative.

## Documentation

* Fixed author name typo in DESCRIPTION: "Vaconcelos" → "Vasconcelos".

* README now accurately lists all exported functions including
 `send_list()` and `check_is_whatsapp()`.

* README includes new sections: verbose output example, configuration
  table, and `send_list()` / `check_is_whatsapp()` usage examples.

* All roxygen documentation improved with `@description`, `@examples`,
 `@seealso` cross-references, and more specific `@return` descriptions.

* Added `jsonlite` to `Imports` (used for response preview in verbose
  mode).

# evolution 0.0.1

* Initial CRAN release.
* Core messaging functions: `send_text()`, `send_status()`,
 `send_media()`, `send_whatsapp_audio()`, `send_sticker()`,
 `send_location()`, `send_contact()`, `send_reaction()`,
 `send_buttons()`, `send_poll()`.
* `evo_client()` factory with httr2 retry and apikey header.
* `jid()` helper for building WhatsApp JIDs.
