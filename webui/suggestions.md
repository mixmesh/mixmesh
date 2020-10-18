# Suggestions: The making of a Web UI

We should in the end probably use a framwork such as [UIkit](https://getuikit.com/docs/introduction). That way we can just write plain HTML and adorn it as seen in, for example, https://getuikit.com/docs/list and https://getuikit.com/docs/iconnav. This way we can avoid CSS altogether. We do not even need to import an icon pack if we use https://getuikit.com/docs/icon. :-)

To start with we should probably use plain unadorned HTML though.

I found some great advice here:

* https://www.w3.org/TR/mobile-bp/
* https://developer.android.com/guide/webapps/best-practices

# Overall layout

I suggest that we use a vertical bottom icon navigation bar on all **pages** (if applicable). Commonality and ease of use.

The navigation bar could have these entries (with icons):

| *My Key* | *Import Key* | *List Keys* | *Import Key Kundle* | *Settings* |

# Pages

## Page: Splash screen

Shown initially for 2 seconds to impress noone. :-P

Navigation bar: **none**

## Page: Initialization screen

A newly initialized box only provides this single initialization page to the user (for at most one hour). On this page the secret key is shown as a 2d-barcode together with instructions on how important it is to make a backup copy of it. There is one button on this page: "I understand!".

If the user clicks on the button he/she is taken to a new page where a nym and password must be given. All details on how to the create a mail account is also listed here, i.e. IP-address, port numbers and SSL settings etc. Ideally we should ask the user if we are allowed to automatically create a mail account. Not for the faint of heart though. Hard. There is one button on this page: "Finish".

Navigation bar: **none**

## Page: *My Key*

Well, here the user's nym and public key is shown as a 2d-barcode and other users can go to their "Import key" page to import it.

Navigation bar: | **My Key** | *Import Key* | *List keys* | *Import key bundle* | *Settings* |

## Page: *Import Key*

A live camera feed is started. On Android a 2d-barcode engine can be activated using the `JavascriptInterface` support, e.g.
https://stackoverflow.com/a/14606975.

As soon as a valid 2d-barcode has been read by the 2d-barcode engine the user is taken back to the browser and the key is auto-imported to the box. On success a the new key's 2d-barcode is shown. If something fails the user is informed.

Navigation bar: | *My Key* | **Import Key** | *List Keys* | *Import Key Bundle* | *Settings* |

## Page: *List Keys*

Shows a list of keys, i.e. nyms and public keys.

Functions:

* Keys are sorted alphabetically on nym
* If the user clicks on a key a 2d-barcode is shown as an overlay on the page (it has a close box)
* A key is deleted if the user clicks on its right aligned trash icon
* A user can select several indvidually keys with checkboxes
* Selected keys can be deleted in a single swoop with a "delete-selected" button
* All keys can be selected using a "select-all" checkbox
* A live search input field can be seen in the upper right corner. It makes it possible to do substring-filtering on nyms.
* A user can select keys in the list and export them as a single key bundle using an "export-selected" button. The user is then prompted for a password and an "export" button appears. If clicked the box encrypts a bundle with the selected key and presents the user with a new page with some info what to do and a "save" button (a link to a .txt file with a base64 encoded key bundle). The user puts the bundle in a place of his/her choosing.

Navigation bar: | *My Key* | *Import Key* | **List Keys** | *Import Key Bundle* | *Settings* |

## Page: *Import Key Bundle*

The user is asked to provide a key bundle using a file-upload input field and an appropriate password. An "import" button is activated as soon as an URL and password has been provided. When clicked the whole bundle is uploaded to the box and decrypted. The user is on success taken to the "List keys" page or provided with a failure reason.

Navigation bar: | *My Key* | *Import Key* | *List Keys* | **Import Key Bundle** | *Settings* |

## Page: Settings

* Mail server ip address
* Mail server ports
* SSL settings

We will come up with loads of parameters. I'm sure. :-)

Navigation bar: | *My Key* | *Import Key* | *List keys* | *Import key bundle* | **Settings** |
