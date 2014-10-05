# Cooper

<img src="http://p.hagelb.org/cooper.jpg" />

A primitive [Racket](http://racket-lang.org)
[HyperCard](http://www.loper-os.org/?p=568) clone, sort of.

Create a stack consisting of cards with drawings and buttons on
them. Write code to run when buttons get clicked. Explore through the
stacks you've created.

## Modes

Right-click to change modes.

* explore (hand)
* buttons (crosshair)
* drawing (bullseye)

Buttons are invisible unless you're in "buttons" mode. Double-clicking
on a button gives you edit mode, where you can either give the button
a target card or write a function that takes the state and can return
a modification of it. Buttons only run the code if their action is set
to `<code>`, otherwise they only jump to the specified card.

Drawing is only straight lines for now.

Card Zero is where you start; it's a card that gets prepopulated with
a self-hosted card listing/editing interface.

## Keys

If you get lost at any point, control-0 will return you to card zero.

Control-s saves, and control-l loads.

Control-c clears background, control-b clears buttons, control-n creates cards.

## Implementation

See `cooper/cooper.rkt` for an overview of the structs used and the
contracts on their fields. State is tracked with the `state` struct
inside a `box`. Each mode defines a number of callback functions for
various event handling. Handlers take the current state (along with
event and canvas objects) and return a modified state.

Cards can have event handler functions attached to them. The card
listing is implemented on the `zero` card, which comes pre-populated
with an `"enter"` event handler that creates buttons pointing to all
the cards in the stack.

## License

Copyright © 2014 Phil Hagelberg and contributors

Image above © 2014 Boom Entertainment, Inc.

Distributed under the GNU General Public License version 3; see file LICENSE.
