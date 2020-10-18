# General stuff

We should in the end probably use a framwork such as [UIkit](https://getuikit.com/docs/introduction). That way we can just write plain HTML and adorn it as seen in, for example, https://getuikit.com/docs/list and https://getuikit.com/docs/iconnav. This way we can avoid CSS altogether. We don't even need to import an icon pack (https://getuikit.com/docs/icon). :-)

To start with we should probably use plain unadorned HTML though.

I found some great advice here:

* https://www.w3.org/TR/mobile-bp/
* https://developer.android.com/guide/webapps/best-practices

# Overal layout

I suggest that we use a vertical bottom icon navigation bar on all **pages**. Commonality and ease of use.

This top-level navigation bar could have these entries (but with icons):

| *My Key* | *Import Key* | *List players* | *Import key bundle* | *Settings* |

# Pages

## Page: Splash screen

Shown initially in 2 seconds to impress noone. :-P

Navigation bar: **none**

## Page: Initialization screen

A newly initialized box only provides this single page to the user (for at most one hour). On this page the player's secret key is shown as a 2d-barcode together with instructions on how to important it is to make a backup of this key. There is one button on this page: "I understand!".

If the user clicks the button he/she is taken to a new page where a nym and appropriate mail password isi given 

No navigation bar

## Page: List players

Show a list of players, i.e. their nyms and public keys.

Functions:

* Players are sorted alphabetically on nym
* If the user clicks on a player a 2d-barcode + OCR string is shown
* A player is deleted if the user clicks on its right aligned trash icon
* A user can select several indvidually with checkboxes
* Selected players can be deleted in a single swoop
* All players can be selected using a dedicated "select-all" checkbox
* A live filter search input field can be seen in the upper right corner. It makes it possible to substring-filter players in the list.
* A user can select players in the list and export them as a single bundle using an activated "export" icon in the navigation bar. The user is prompted for a password and after it has been provided an encrypted player bundle is prepared on the box. On success the user is presented with a save button which points to a temporary link on the box.

## Page: Import player

A live camera feed is started. On Android a 2d-barcode engine can be activated using the JavascriptInterface support, e.g.
https://stackoverflow.com/a/14606975.

As soon as a valid 2d-barcode has been read the user is taken back to the Web UI and the player is auto-imported to the box, and shown as a new 2d-barcode (and OCR string). If something fails in the process the user is informed.

## Page: Import a bundle of players

The user is asked to provide a player bundle using a file-upload input field and an appropriate password. An "import" icon is activated in the navigation bar as soon as a URL and password has been given. If the user clicks on "import" the whole bundle is uploaded to the box, and decrypted. The user is on success taken to the "List players" page.

## Page: Show the users own 2d-barcode

Well, the user's nym + public key is shown as a 2d-barcode and other users can go to their "Import player" page to import it.

## Page: Mail settings (lower prio)

The user can change the POP3/SMTP ports (and possiblly other mail related parameters) and manage a list of mail accounts (and their passwords) which should be granted mail service on the box. I know. :-) Maybe not.

## Page: System settings

We will come up with loads of them. I'm sure. :-)
