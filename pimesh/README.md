# PiMesh INFO

## Keyboard

The Pincode is entered via a keypad (sometimes) the suggested
pincodes are typically 4 or 6 digits.

There are 3 prototype keybaords

- one keyboard 3x3 [1-9] on the TCA8418E-EVM (evaluation board)
- one BIG old style (telefon) keypad 4x3  [0-9#\*]
- one MEDIUM style (telefon) keypad 4x3  [0-9#\*]

Pinlayout on EVM baord

    C9 C8 C7 C6 C5 C4 C3 C2 C1 C0
    G  V  R7 R6 R5 R4 R3 R2 R1 R0

The BIG keyboard 8-pin layout

    G  C2 C0 R3 C2 R2 R1 R0

For the EVM do not connect the ground (G) pin
(may be used for PULLUP for eached Ri and  Cj)

The SMALL keyboard 7-pin layout

    R2 R1 C0 R0 C2 R3 C1

## LED & PIN-ENTRY

The pin entry process is guided only with LEDS,

Example

- Pin code is needed RED-BLINK, GREEN-OFF
- Key is pressed FLASH GREEN for each PRESS/RELEASE
- Pin code is accepted GREEN-ON, RED-OFF, device enabled

Pin code could be accepted either when the correct digits
have been pressed regardless how many key have been pressed.
Or the # key is used to enter the pincode. If keyboard is
missing a key for enter (#) then the first option may be
needed.
Enter a bad pin code will set RED-ON and run a exponetial back-off
delay until a new pin code may be tried.

- one MEDIUM style (telefon) keyboard 4x3 [0-9#\*]