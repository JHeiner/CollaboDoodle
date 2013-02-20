This is an SBT project. I used version 0.12.2 when developing it.
The SBT command "container:start" should be all you need to get it going,
then point your browser at port 8080.

The top part is a chat. There's a checkbox hiding in the corner that
toggles the auto-scroll-to-bottom-on-new-message behavior. The IRC
"/nick" command is supported. And you can change your color by appending
to your nickname "%color" - e.g. "/nick jeremy%green" or "/nick jeremy%#aa2".

The bottom part has a place for doodling. Each participant's doodles are
displayed in the same area, but nobody can interact with anybody else's.
