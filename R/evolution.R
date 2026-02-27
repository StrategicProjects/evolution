# ---- Client factory ---------------------------------------------------------

#' Create an Evolution API client
#'
#' @description Creates a preconfigured **httr2** client to call Evolution API v2.
#' It sets the `apikey` header, a custom User-Agent and basic automatic retries.
#'
#' The returned object is used by every `send_*()` function and stores the base
#' request (`req`) and the instance name so you don't have to repeat them.
#'
#' @param base_url Character. Server base URL (no trailing slash), e.g.
#'   `"https://your-host"`.
#' @param api_key  Character. API key (sent as `apikey` header).
#'   Prefer `Sys.getenv("EVO_APIKEY")` to avoid hardcoding secrets.
#' @param instance Character. Instance name/ID used in endpoint paths.
#' @return An object of class `evo_client` with fields `req` (httr2 request)
#'   and `instance`.
#' @examples
#' \dontrun{
#' client <- evo_client(
#'   base_url = "https://your-evolution-host.com",
#'   api_key  = Sys.getenv("EVO_APIKEY"),
#'   instance = "myInstance"
#' )
#' }
#' @seealso [send_text()], [send_media()], [send_location()]
#' @export
evo_client <- function(base_url, api_key, instance) {
  if (!is.character(base_url) || !nzchar(base_url)) {
    cli::cli_abort("{.arg base_url} must be a non-empty string.")
  }
  if (!is.character(api_key) || !nzchar(api_key)) {
    cli::cli_abort(
      "{.arg api_key} must be a non-empty string. Use {.code Sys.getenv(\"EVO_APIKEY\")}."
    )
  }
  if (!is.character(instance) || !nzchar(instance)) {
    cli::cli_abort("{.arg instance} must be a non-empty string.")
  }

  req <- httr2::request(sub("/+$", "", base_url)) |>
    httr2::req_headers(apikey = api_key, `Content-Type` = "application/json") |>
    httr2::req_user_agent("evolution-r/0.1.0 (httr2)") |>
    httr2::req_retry(max_tries = 3)

  structure(list(req = req, instance = instance), class = "evo_client")
}

#' @export
print.evo_client <- function(x, ...) {
  cli::cli_h3("Evolution API Client")
  cli::cli_li("Instance: {.val {x$instance}}")
  cli::cli_li("Base URL: {.url {x$req$url}}")
  invisible(x)
}

# ---- Internals --------------------------------------------------------------

#' Build internal API path
#'
#' @keywords internal
#' @param ... Character path segments.
#' @return A single character scalar with segments concatenated by "/".
.evo_path <- function(...) {
  paste0(c(...), collapse = "/")
}

#' Compact a list removing NULL elements
#'
#' @keywords internal
#' @param x A list possibly containing `NULL` elements.
#' @return The same list with all `NULL` elements removed.
.compact <- function(x) x[!vapply(x, is.null, logical(1))]

#' Perform a JSON POST request (internal)
#'
#' Sends a JSON POST to the Evolution API and returns the parsed response.
#' Includes structured CLI logging when `verbose = TRUE` and robust error
#' handling that surfaces the actual API error message.
#'
#' @keywords internal
#' @param client An `evo_client` object.
#' @param path   Character. Path to append to the base URL.
#' @param body   List to be JSON-encoded as the request body.
#' @param verbose Logical. If `TRUE`, prints request/response diagnostics via
#'   **cli**.
#' @return Parsed JSON as list (with raw HTTP status stored in attribute
#'   `http_status`).
.evo_post <- function(client, path, body, verbose = FALSE) {
  if (!inherits(client, "evo_client")) {
    cli::cli_abort(
      "{.arg client} must be an {.cls evo_client} object created by {.fn evo_client}."
    )
  }

  timeout <- getOption("evolution.timeout", 60)
  body <- .compact(body)

  req <- client$req |>
    httr2::req_url_path_append(path) |>
    httr2::req_body_json(body, auto_unbox = TRUE, null = "null") |>
    httr2::req_timeout(timeout)

  # --- Verbose: log request ---------------------------------------------------
  if (isTRUE(verbose)) {
    cli::cli_rule(left = "{.strong evoapi} POST {path}")
    cli::cli_alert_info("Timeout: {timeout}s")
    show <- body
    if (!is.null(show$apikey)) show$apikey <- "<REDACTED>"
    if (!is.null(show$media) && nchar(show$media) > 80) {
      show$media <- paste0(substr(show$media, 1, 40), "...<truncated>")
    }
    cli::cli_alert_info("Body:")
    capture <- utils::capture.output(utils::str(show, give.attr = FALSE))
    for (line in capture) cli::cli_text("  {line}")
  }

  # --- Perform request --------------------------------------------------------
  t0 <- proc.time()["elapsed"]
  resp <- tryCatch(
    {
      req |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform()
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Request to Evolution API failed (connection/network error).",
        "i" = "Endpoint: POST {.val {path}}",
        "!" = conditionMessage(e)
      ), call = NULL)
    }
  )
  elapsed <- round(proc.time()["elapsed"] - t0, 2)
  status <- httr2::resp_status(resp)

  # --- Parse response body safely ---------------------------------------------
  ct <- httr2::resp_header(resp, "content-type") %||% ""
  is_json <- grepl("application/json", ct, fixed = TRUE)

  out <- if (is_json) {
    tryCatch(
      httr2::resp_body_json(resp, simplifyVector = TRUE),
      error = function(e) list(.raw_body = httr2::resp_body_string(resp))
    )
  } else {
    list(.raw_body = httr2::resp_body_string(resp))
  }

  # --- Verbose: log response --------------------------------------------------
  if (isTRUE(verbose)) {
    if (status < 400) {
      cli::cli_alert_success("HTTP {status} ({elapsed}s)")
    } else {
      cli::cli_alert_danger("HTTP {status} ({elapsed}s)")
    }
    if (nzchar(ct)) cli::cli_alert_info("Content-Type: {ct}")
    preview <- if (!is.null(out$.raw_body)) {
      substr(out$.raw_body, 1, 500)
    } else {
      tryCatch(
        substr(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = FALSE), 1, 500),
        error = function(e) "<unable to serialize>"
      )
    }
    cli::cli_alert_info("Response: {preview}")
  }

  # --- Handle HTTP errors -----------------------------------------------------
  if (status >= 400) {
    api_msg <- out$response$message %||%
      out$message %||%
      out$.raw_body %||%
      "No details returned by the API."
    if (is.list(api_msg)) api_msg <- paste(unlist(api_msg), collapse = "; ")

    cli::cli_abort(c(
      "x" = "Evolution API returned HTTP {status}.",
      "i" = "Endpoint: POST {path}",
      "!" = "API message: {api_msg}"
    ), call = NULL)
  }

  attr(out, "http_status") <- status
  out
}

# ---- Helpers ----------------------------------------------------------------

#' Build a WhatsApp JID from a raw phone number
#'
#' @description Normalises a raw phone number by removing spaces, dashes,
#'   parentheses, and the leading `+` sign, then appends
#'   `@s.whatsapp.net`.
#'
#' @param number Character scalar or vector. Raw phone number(s)
#'   (e.g., `"+5581999990000"`).
#' @return Character JID(s) (e.g., `"5581999990000@s.whatsapp.net"`).
#' @examples
#' jid("+55 81 99999-0000")
#' #> "5581999990000@s.whatsapp.net"
#'
#' jid("5581999990000")
#' #> "5581999990000@s.whatsapp.net"
#' @export
jid <- function(number) {
  if (!is.character(number) || length(number) < 1L) {
    cli::cli_abort("{.arg number} must be a character string or vector.")
  }
  cleaned <- gsub("[^0-9]", "", number)
  paste0(cleaned, "@s.whatsapp.net")
}

# ---- Endpoints --------------------------------------------------------------

#' Send a plain text message
#'
#' @description Sends a plain text WhatsApp message using Evolution API v2.
#'
#' @param client An [evo_client()] object.
#' @param number Character. Recipient number with country code
#'   (e.g., `"5581999990000"` or `"+5581999990000"`).
#' @param text   Character. Message body.
#' @param delay  Integer (ms). Optional presence delay before sending.
#'   Simulates typing before the message is sent.
#' @param link_preview Logical. Enable URL link preview in the message.
#' @param mentions_everyone Logical. Mention everyone in a group.
#' @param mentioned Character vector of JIDs to mention
#'   (e.g., `jid("+5581999990000")`).
#' @param quoted Optional list with Baileys message `key` and `message`
#'   to reply to a specific message.
#' @param verbose Logical. If `TRUE`, logs request/response details with
#'   **cli**.
#' @return A named list parsed from the JSON response returned by Evolution
#'   API, containing the message `key` (with `remoteJid`, `fromMe`, `id`),
#'   `message`, `messageTimestamp`, and `status`.
#'   The HTTP status code is stored in `attr(result, "http_status")`.
#' @examples
#' \dontrun{
#' client <- evo_client("https://my-host", Sys.getenv("EVO_APIKEY"), "myInst")
#' send_text(client, "5581999990000", "Hello from R!", verbose = TRUE)
#' }
#' @seealso [send_media()], [send_location()], [jid()]
#' @export
send_text <- function(client, number, text, delay = NULL,
                      link_preview = NULL, mentions_everyone = NULL,
                      mentioned = NULL, quoted = NULL, verbose = FALSE) {
  .assert_scalar_string(number, "number")
  .assert_scalar_string(text, "text")

  body <- list(
    number            = number,
    text              = text,
    delay             = delay,
    linkPreview       = link_preview,
    mentionsEveryOne  = mentions_everyone,
    mentioned         = mentioned,
    quoted            = quoted
  )
  .evo_post(client, .evo_path("message", "sendText", client$instance),
            body, verbose = verbose)
}

#' Send a WhatsApp Status (story)
#'
#' @description Posts a status (story) message visible to your contacts.
#'   Supports text or media (image, video, document, audio) types.
#'
#' @inheritParams send_text
#' @param type One of `"text"`, `"image"`, `"video"`, `"document"`, `"audio"`.
#' @param content Text (for `type = "text"`) or URL/base64 for media.
#' @param caption Optional caption for media types.
#' @param background_color Hex colour for text status background
#'   (e.g., `"#FF5733"`).
#' @param font Integer font id (0--14).
#' @param all_contacts Logical. If `TRUE`, sends to all contacts.
#' @param status_jid_list Optional character vector of specific JIDs to
#'   receive the status.
#' @return A named list with the API response. The HTTP status code is
#'   stored in `attr(result, "http_status")`.
#' @examples
#' \dontrun{
#' send_status(client, type = "text", content = "Hello from R!",
#'             background_color = "#317873", font = 2, all_contacts = TRUE)
#' }
#' @export
send_status <- function(client, type = c("text", "image", "video", "document", "audio"),
                        content, caption = NULL, background_color = NULL, font = NULL,
                        all_contacts = FALSE, status_jid_list = NULL, verbose = FALSE) {
  type <- match.arg(type)
  .assert_scalar_string(content, "content")

  body <- list(
    type            = type,
    content         = content,
    caption         = caption,
    backgroundColor = background_color,
    font            = font,
    allContacts     = isTRUE(all_contacts),
    statusJidList   = status_jid_list
  )
  .evo_post(client, .evo_path("message", "sendStatus", client$instance),
            body, verbose = verbose)
}


#' Send media (image, video, document)
#'
#' @description Sends an image, video, or document via Evolution API v2.
#'   The `media` argument is flexible: it accepts an HTTP(S) URL, a local
#'   file path (auto-encoded to base64), raw base64, or a data-URI.
#'
#' @inheritParams send_text
#' @param mediatype One of `"image"`, `"video"`, `"document"`.
#' @param mimetype MIME type string, e.g., `"image/png"`, `"video/mp4"`,
#'   `"application/pdf"`.
#' @param media The media content. Can be: (a) an HTTP/HTTPS URL;
#'   (b) a local file path; (c) raw base64; or
#'   (d) a data-URI (`data:*;base64,...`).
#' @param file_name Suggested filename for the recipient
#'   (should match the MIME type, e.g., `"report.pdf"`).
#' @param caption Optional caption text displayed with the media.
#' @return A named list with the API response. The HTTP status code is
#'   stored in `attr(result, "http_status")`.
#' @examples
#' \dontrun{
#' # From URL
#' send_media(client, "5581999990000", "image", "image/png",
#'            media = "https://www.r-project.org/logo/Rlogo.png",
#'            file_name = "Rlogo.png", caption = "R Logo")
#'
#' # From local file
#' send_media(client, "5581999990000", "document", "application/pdf",
#'            media = "report.pdf", file_name = "report.pdf")
#' }
#' @export
send_media <- function(client, number, mediatype, mimetype,
                       media, file_name, caption = NULL,
                       delay = NULL, link_preview = NULL, verbose = FALSE) {
  .assert_scalar_string(number, "number")
  .assert_scalar_string(mimetype, "mimetype")
  .assert_scalar_string(file_name, "file_name")
  if (!mediatype %in% c("image", "video", "document")) {
    cli::cli_abort(
      '{.arg mediatype} must be one of {.val image}, {.val video}, or {.val document}. Got {.val {mediatype}}.'
    )
  }

  media_norm <- .normalize_media(media)

  body <- .compact(list(
    number      = number,
    mediatype   = mediatype,
    mimetype    = mimetype,
    caption     = caption,
    media       = media_norm,
    fileName    = file_name,
    delay       = delay,
    linkPreview = link_preview
  ))

  .evo_post(client, .evo_path("message", "sendMedia", client$instance),
            body, verbose = verbose)
}

#' Send WhatsApp audio (voice note)
#'
#' @description Sends an audio message (push-to-talk / voice note) via
#'   Evolution API v2.
#'
#' @inheritParams send_text
#' @param audio URL, base64-encoded audio, or local file path
#'   (auto-encoded to base64). Supports `~` expansion.
#' @return A named list with the API response. The HTTP status code is
#'   stored in `attr(result, "http_status")`.
#' @examples
#' \dontrun{
#' send_whatsapp_audio(client, "5581999990000",
#'                     audio = "https://example.com/note.ogg")
#' }
#' @export
send_whatsapp_audio <- function(client, number, audio, delay = NULL,
                                quoted = NULL, verbose = FALSE) {
  .assert_scalar_string(number, "number")
  .assert_scalar_string(audio, "audio")

  audio_norm <- .normalize_media(audio)
  body <- list(
    number = number,
    audio  = audio_norm,
    delay  = delay,
    quoted = quoted
  )
  .evo_post(client, .evo_path("message", "sendWhatsAppAudio", client$instance),
            body, verbose = verbose)
}

#' Send a sticker
#'
#' @description Sends a sticker image via Evolution API v2.
#'
#' @inheritParams send_text
#' @param sticker URL, base64-encoded sticker image, or local file path
#'   (auto-encoded to base64). Supports `~` expansion.
#' @return A named list with the API response. The HTTP status code is
#'   stored in `attr(result, "http_status")`.
#' @examples
#' \dontrun{
#' send_sticker(client, "5581999990000",
#'              sticker = "https://example.com/sticker.webp")
#' }
#' @export
send_sticker <- function(client, number, sticker, delay = NULL, verbose = FALSE) {
  .assert_scalar_string(number, "number")
  .assert_scalar_string(sticker, "sticker")

  sticker_norm <- .normalize_media(sticker)
  body <- list(number = number, sticker = sticker_norm, delay = delay)
  .evo_post(client, .evo_path("message", "sendSticker", client$instance),
            body, verbose = verbose)
}

#' Send a location
#'
#' @description Sends a geographic location pin via Evolution API v2.
#'
#' @inheritParams send_text
#' @param latitude  Numeric. Latitude coordinate.
#' @param longitude Numeric. Longitude coordinate.
#' @param name    Optional character. Location label name.
#' @param address Optional character. Address description.
#' @return A named list with the API response. The HTTP status code is
#'   stored in `attr(result, "http_status")`.
#' @examples
#' \dontrun{
#' send_location(client, "5581999990000",
#'               latitude = -8.05, longitude = -34.88,
#'               name = "Recife Antigo", address = "Marco Zero")
#' }
#' @export
send_location <- function(client, number, latitude, longitude,
                          name = NULL, address = NULL, verbose = FALSE) {
  .assert_scalar_string(number, "number")
  if (!is.numeric(latitude) || !is.numeric(longitude)) {
    cli::cli_abort("{.arg latitude} and {.arg longitude} must be numeric values.")
  }

  body <- list(
    number    = number,
    latitude  = latitude,
    longitude = longitude,
    name      = name,
    address   = address
  )
  .evo_post(client, .evo_path("message", "sendLocation", client$instance),
            body, verbose = verbose)
}

#' Send a WhatsApp contact (auto-generate wuid)
#'
#' @description Sends one or more contacts following the Evolution API v2
#'   format. Automatically generates the `wuid` field as
#'   `<digits>@@s.whatsapp.net` from each contact's phone number
#'   (or from `number` if not provided).
#'
#' @param client An [evo_client()] object.
#' @param number Recipient number (e.g., `"5581999990000"`).
#' @param contact Either:
#'   - a named list with fields `fullName`, `phoneNumber`, `organization`,
#'     `email`, `url`; or
#'   - a list of such lists (to send multiple contacts).
#'   The `wuid` field will be auto-generated if missing.
#' @param verbose Logical; if `TRUE`, shows detailed logs.
#' @return Parsed JSON response as list (see [.evo_post()] for details).
#' @examples
#' \dontrun{
#' send_contact(client, "5581999990000",
#'   contact = list(
#'     fullName     = "Jane Doe",
#'     phoneNumber  = "+5581999990000",
#'     organization = "Company Ltd.",
#'     email = "jane@@example.com",
#'     url   = "https://company.com"
#'   ))
#' }
#' @export
send_contact <- function(client, number, contact, verbose = FALSE) {
  .assert_scalar_string(number, "number")

  to_wuid <- function(num) {
    clean <- gsub("[^0-9]", "", num)
    if (nzchar(clean)) paste0(clean, "@s.whatsapp.net") else NULL
  }

  # If a single contact (has $fullName), wrap it in a list
  if (is.list(contact) && !is.null(contact$fullName)) {
    contact <- list(contact)
  }

  contact <- lapply(contact, function(ct) {
    if (is.null(ct$wuid)) {
      phone <- ct$phoneNumber %||% number
      ct$wuid <- to_wuid(phone)
    }
    .compact(ct)
  })

  body <- .compact(list(number = number, contact = contact))
  .evo_post(client, .evo_path("message", "sendContact", client$instance),
            body, verbose = verbose)
}


#' React to a message
#'
#' @description Sends an emoji reaction to an existing message.
#'
#' @inheritParams send_text
#' @param key List with `remoteJid`, `fromMe`, and `id` identifying the
#'   target message.
#' @param reaction Emoji string (e.g., `"\U0001f44d"` for thumbs up).
#'   Use an empty string `""` to remove a reaction.
#' @return A named list with the API response. The HTTP status code is
#'   stored in `attr(result, "http_status")`.
#' @examples
#' \dontrun{
#' send_reaction(client, key = list(
#'   remoteJid = "5581999990000@@s.whatsapp.net",
#'   fromMe = TRUE,
#'   id = "BAE594145F4C59B4"
#' ), reaction = "\U0001f44d")
#' }
#' @export
send_reaction <- function(client, key, reaction, verbose = FALSE) {
  if (!is.list(key) || is.null(key$id)) {
    cli::cli_abort("{.arg key} must be a list with at least an {.val id} field.")
  }
  if (!is.character(reaction) || length(reaction) != 1L) {
    cli::cli_abort("{.arg reaction} must be a single character string (emoji or empty string).")
  }

  body <- list(key = key, reaction = reaction)
  .evo_post(client, .evo_path("message", "sendReaction", client$instance),
            body, verbose = verbose)
}

#' Send interactive buttons
#'
#' @description Sends a message with interactive buttons via Evolution API v2.
#'
#' @note **Baileys connector:** Interactive buttons are **not supported** on
#'   the Baileys (WhatsApp Web) connector and are likely to be discontinued.
#'   This endpoint is fully supported only on the **Cloud API** connector.
#'   If you are on Baileys, consider using [send_poll()] as an alternative.
#'
#' @inheritParams send_text
#' @param title       Character. Button message title.
#' @param description Character. Button message description/body.
#' @param footer      Character. Footer text.
#' @param buttons     List of buttons. Each button should be a named list
#'   following the API specification (see Evolution API docs).
#' @return A named list with the API response. The HTTP status code is
#'   stored in `attr(result, "http_status")`.
#' @seealso [send_poll()]
#' @examples
#' \dontrun{
#' send_buttons(client, "5581999990000",
#'   title = "Choose",
#'   description = "Pick an option:",
#'   footer = "Powered by R",
#'   buttons = list(
#'     list(type = "reply", title = "Option A"),
#'     list(type = "reply", title = "Option B")
#'   ))
#' }
#' @export
send_buttons <- function(client, number, title, description, footer, buttons,
                         delay = NULL, link_preview = NULL,
                         mentions_everyone = NULL, verbose = FALSE) {
  cli::cli_warn(c(
    "!" = "Interactive buttons are {.strong not supported} on the Baileys connector and may be discontinued.",
    "i" = "This endpoint works on the {.strong Cloud API} connector only.",
    "i" = "Consider {.fun send_poll} as an alternative."
  ))
  .assert_scalar_string(number, "number")
  .assert_scalar_string(title, "title")
  .assert_scalar_string(description, "description")
  .assert_scalar_string(footer, "footer")
  if (!is.list(buttons) || length(buttons) == 0L) {
    cli::cli_abort("{.arg buttons} must be a non-empty list of button definitions.")
  }

  body <- list(
    number           = number,
    title            = title,
    description      = description,
    footer           = footer,
    buttons          = buttons,
    delay            = delay,
    linkPreview      = link_preview,
    mentionsEveryOne = mentions_everyone
  )
  .evo_post(client, .evo_path("message", "sendButtons", client$instance),
            body, verbose = verbose)
}

#' Send a poll
#'
#' @description Sends a poll (question with selectable options) via
#'   Evolution API v2.
#'
#' @inheritParams send_text
#' @param name Question text displayed in the poll.
#' @param values Character vector of poll options (minimum 2).
#' @param selectable_count Integer. Number of options a user can select
#'   (default `1L` for single-choice).
#' @return A named list with the API response. The HTTP status code is
#'   stored in `attr(result, "http_status")`.
#' @examples
#' \dontrun{
#' send_poll(client, "5581999990000",
#'   name = "Favourite language?",
#'   values = c("R", "Python", "Julia"),
#'   selectable_count = 1)
#' }
#' @export
send_poll <- function(client, number, name, values,
                      selectable_count = 1L, verbose = FALSE) {
  .assert_scalar_string(number, "number")
  .assert_scalar_string(name, "name")
  if (!is.character(values) || length(values) < 2L) {
    cli::cli_abort("{.arg values} must be a character vector with at least 2 options.")
  }

  body <- list(
    number          = number,
    name            = name,
    values          = as.list(values),
    selectableCount = as.integer(selectable_count)
  )
  .evo_post(client, .evo_path("message", "sendPoll", client$instance),
            body, verbose = verbose)
}

#' Send a list message
#'
#' @description Sends an interactive list message via Evolution API v2.
#'   List messages display a menu of selectable options organised into
#'   sections.
#'
#' @note **Baileys connector:** Interactive list messages are **not supported**
#'   on the Baileys (WhatsApp Web) connector and are likely to be
#'   discontinued. This endpoint is fully supported only on the **Cloud API**
#'   connector. If you are on Baileys, consider using [send_poll()] as an
#'   alternative.
#'
#' @inheritParams send_text
#' @param title       Character. List message title.
#' @param description Character. List message body text.
#' @param button_text Character. Text displayed on the list button
#'   (e.g., `"View options"`).
#' @param footer      Character. Footer text (required by the API, defaults
#'   to `""`).
#' @param sections    A list of section objects. Each section is a named
#'   list with `title` and `rows`, where `rows` is a list of named lists
#'   each containing `title`, optional `description`, and optional `rowId`.
#' @return A named list with the API response. The HTTP status code is
#'   stored in `attr(result, "http_status")`.
#' @seealso [send_poll()]
#' @examples
#' \dontrun{
#' send_list(client, "5581999990000",
#'   title = "Our Menu",
#'   description = "Select from the options below:",
#'   button_text = "View options",
#'   footer = "Powered by R",
#'   sections = list(
#'     list(title = "Drinks", rows = list(
#'       list(title = "Coffee", description = "Hot coffee", rowId = "1"),
#'       list(title = "Tea",    description = "Green tea",  rowId = "2")
#'     )),
#'     list(title = "Food", rows = list(
#'       list(title = "Cake", description = "Chocolate cake", rowId = "3")
#'     ))
#'   ))
#' }
#' @export
send_list <- function(client, number, title, description,
                      button_text, sections, footer = "",
                      delay = NULL, verbose = FALSE) {
  cli::cli_warn(c(
    "!" = "Interactive list messages are {.strong not supported} on the Baileys connector and may be discontinued.",
    "i" = "This endpoint works on the {.strong Cloud API} connector only.",
    "i" = "Consider {.fun send_poll} as an alternative."
  ))
  .assert_scalar_string(number, "number")
  .assert_scalar_string(title, "title")
  .assert_scalar_string(description, "description")
  .assert_scalar_string(button_text, "button_text")
  if (!is.list(sections) || length(sections) == 0L) {
    cli::cli_abort("{.arg sections} must be a non-empty list of section definitions.")
  }

  body <- .compact(list(
    number      = number,
    title       = title,
    description = description,
    buttonText  = button_text,
    footerText  = footer,
    sections    = sections,
    delay       = delay
  ))
  .evo_post(client, .evo_path("message", "sendList", client$instance),
            body, verbose = verbose)
}

#' Check if numbers are on WhatsApp
#'
#' @description Verifies whether one or more phone numbers are registered
#'   on WhatsApp using the Evolution API v2 chat controller endpoint.
#'
#' @param client An [evo_client()] object.
#' @param numbers Character vector of phone numbers to check (with country
#'   code, e.g., `"5581999990000"`).
#' @param verbose Logical. If `TRUE`, logs request/response details.
#' @return A named list (or data frame) from the API indicating which
#'   numbers are registered. The HTTP status code is stored in
#'   `attr(result, "http_status")`.
#' @examples
#' \dontrun{
#' check_is_whatsapp(client, c("5581999990000", "5511988887777"))
#' }
#' @export
check_is_whatsapp <- function(client, numbers, verbose = FALSE) {
  if (!is.character(numbers) || length(numbers) == 0L) {
    cli::cli_abort("{.arg numbers} must be a non-empty character vector of phone numbers.")
  }

  body <- list(numbers = as.list(numbers))
  .evo_post(client, .evo_path("chat", "whatsappNumbers", client$instance),
            body, verbose = verbose)
}

# ---- Internal utilities -----------------------------------------------------

#' Assert that a value is a single non-empty string
#' @keywords internal
#' @param x Value to check.
#' @param name Argument name for the error message.
.assert_scalar_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
    cli::cli_abort("{.arg {name}} must be a single non-empty character string.")
  }
}

#' Normalise media input (URL, file path, base64, data-URI)
#' @keywords internal
#' @param x The media input to normalise.
#' @return Character string suitable for the API body (URL or base64).
.normalize_media <- function(x) {
  if (!is.character(x) || length(x) != 1L) {
    cli::cli_abort("{.arg media} must be a single string (URL, base64, or file path).")
  }

  # Case 1: HTTP(S) URL
  if (grepl("^https?://", x, ignore.case = TRUE)) return(x)

  # Case 2: Local file (expand ~ to home dir)
  expanded <- path.expand(x)
  if (file.exists(expanded)) {
    if (!requireNamespace("base64enc", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg base64enc} is required to encode local files.")
    }
    cli::cli_alert_info("Encoding local file {.file {expanded}} to base64.")
    return(base64enc::base64encode(expanded))
  }

  # Case 3: Data-URI prefix
  if (grepl("^data:.*;base64,", x)) {
    x <- sub("^data:.*;base64,", "", x)
  }

  # Clean whitespace and validate as base64
  x <- gsub("\\s+", "", x)
  if (!grepl("^[A-Za-z0-9+/=]+$", x)) {
    cli::cli_abort(c(
      "x" = "{.arg media} does not appear to be a valid URL, file path, or base64 string.",
      "i" = "Expected one of: HTTP(S) URL, existing file path, base64 string, or data-URI."
    ))
  }
  x
}
