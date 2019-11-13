# Magic Grapher ![build status](https://img.shields.io/circleci/build/github/mcmire/magic-grapher)

Draw and generate hierarchical diagrams with ease.

## Background

One of the first things that I tend to do when understanding a system —
whether it's a network of services or the architecture of a codebase —
is to map that system out visually by drawing diagrams.
These diagrams usually consist of a series of rounded rectangles or ovals with text inside,
connected to each other via arrows,
and the arrows usually follow one direction (down).

Here's the problem:
creating these kinds of diagrams is fairly time-consuming.
First, you have to find a program that you don't hate.
Google Drawings is usually my weapon of choice;
tools like Lucidchart, Draw.io, and Gliffy are nice in theory,
but they make you sign up for their service,
and some of the other ones aren't free.

Second, you have to actually draw the diagram itself.
It seems that none of these programs are designed for speed.
I usually start by creating the first "node":
drawing a shape,
changing its background color,
adding a border,
typing some text inside the shape,
and resizing the shape to fit the content.
Then I copy that node,
change its text,
and resize it to match.
Then I draw an arrow in between the first and second node.
Then I copy another node and repeat the process all over again.
Of course, as I add more nodes,
I have to constantly reposition them
so that they don't collide with each other
and so I can clearly see all of the connections between the nodes
without lines crossing this way and that.
It ends up being a huge waste of time.

In the past I used Graphviz to solve this problem.
Graphviz is nice because it allows you to describe your graph as a text file.
That way you don't have to worry about the layout:
the program will draw all of the nodes for you
and place them on the page in an intelligent way.
The problem is that as soon as you reach any level of complexity
(which arrives fairly fast),
the text file gets pretty unmanageable.
Plus, the Graphviz documentation is pretty antiquated
and the program itself doesn't support some features you'd think would be there.
So not only does it prove that text is a bad way to specify a graph,
but its limitations only result in frustration.

## Goals

Here are some of the goals of this app:

* You should be able to create a diagram (or graph) of reasonable size (30 nodes)
  in about 10 minutes.
  This is accomplishable by making it possible to do a lot of things from the keyboard.
* You shouldn't have to resize nodes;
  the app should do that for you automatically
  based on the content within.
* Any stylistic modifications you make to a node
  should carry forward to the next node you create,
  thereby saving time.
* You should be able to reposition nodes as you go,
  but then when you're ready,
  you should be able to press a keystroke
  and have the program reposition all of your nodes intelligently.
* For simplicity, the app will only have one layout algorithm implemented,
  the one that looks the best
  (a top-down hierarchical sort of approach).
  Cyclical graphs will not be supported, at least at first,
  since that would require a more complicated algorithm.

## Development

Run:

    bin/setup

Now run:

    npm run start

And finally visit:

    http://localhost:1234/

To run tests, say:

    npm run test
