# Package index

## Client

Create and configure the API client.

- [`evo_client()`](https://strategicprojects.github.io/evolution/reference/evo_client.md)
  : Create an Evolution API client

## Send Messages

Functions to send different types of WhatsApp messages.

- [`send_text()`](https://strategicprojects.github.io/evolution/reference/send_text.md)
  : Send a plain text message
- [`send_media()`](https://strategicprojects.github.io/evolution/reference/send_media.md)
  : Send media (image, video, document)
- [`send_whatsapp_audio()`](https://strategicprojects.github.io/evolution/reference/send_whatsapp_audio.md)
  : Send WhatsApp audio (voice note)
- [`send_sticker()`](https://strategicprojects.github.io/evolution/reference/send_sticker.md)
  : Send a sticker
- [`send_location()`](https://strategicprojects.github.io/evolution/reference/send_location.md)
  : Send a location
- [`send_contact()`](https://strategicprojects.github.io/evolution/reference/send_contact.md)
  : Send a WhatsApp contact (auto-generate wuid)
- [`send_status()`](https://strategicprojects.github.io/evolution/reference/send_status.md)
  : Send a WhatsApp Status (story)
- [`send_reaction()`](https://strategicprojects.github.io/evolution/reference/send_reaction.md)
  : React to a message
- [`send_buttons()`](https://strategicprojects.github.io/evolution/reference/send_buttons.md)
  : Send interactive buttons
- [`send_poll()`](https://strategicprojects.github.io/evolution/reference/send_poll.md)
  : Send a poll
- [`send_list()`](https://strategicprojects.github.io/evolution/reference/send_list.md)
  : Send a list message

## Chat Utilities

Query and verify WhatsApp numbers.

- [`check_is_whatsapp()`](https://strategicprojects.github.io/evolution/reference/check_is_whatsapp.md)
  : Check if numbers are on WhatsApp
- [`jid()`](https://strategicprojects.github.io/evolution/reference/jid.md)
  : Build a WhatsApp JID from a raw phone number

## Internals

Internal helper functions (not exported).

- [`.assert_scalar_string()`](https://strategicprojects.github.io/evolution/reference/dot-assert_scalar_string.md)
  : Assert that a value is a single non-empty string
- [`.compact()`](https://strategicprojects.github.io/evolution/reference/dot-compact.md)
  : Compact a list removing NULL elements
- [`.evo_path()`](https://strategicprojects.github.io/evolution/reference/dot-evo_path.md)
  : Build internal API path
- [`.evo_post()`](https://strategicprojects.github.io/evolution/reference/dot-evo_post.md)
  : Perform a JSON POST request (internal)
- [`.normalize_media()`](https://strategicprojects.github.io/evolution/reference/dot-normalize_media.md)
  : Normalise media input (URL, file path, base64, data-URI)
