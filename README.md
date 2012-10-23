OpenAlchemistAI
===============

An AI for the game OpenAlchemist (http://www.openalchemist.com/)

What does it do
---------------

Hopefully, play OpenAlchemist on its own, at least the "thinking part", even if it still requires you to move the objects yourself with your keyboard.
The goal is to have an AI that can calculate the best moves and do better than my poor brain.

How to use it
-------------

Run the executable while your new game of OpenAlchemist is open and visible on your desktop. It will give you the best move it thinks you should play, in the form of a tuple of Tile,Column, where Column starts at 0.

How does it work
----------------

For the moment, it doesn't use any API from OpenAlchemist since it looked like everything was C++. So it capture the window where the game is running, tries to "see" what the game is, what the next
objects to play are, and then calculate the best move.
The best move is calculated taking into account the current state of the game, the two objects that have to be played, and the two objects that will come afterwards.

Code organization
-----------------

There are three source folders:

* exe: the executable entry point
* src: the main source code
	+ Game is the game handling (resolve object collapse and such) and AI code
	+ Types contains the type definitions
	+ Vision performs the capturing and detection of shapes 
* test: contains the unit tests