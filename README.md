# scxml-extension
An emacs lisp program which tries to draw scxml diagrams.  It doesn't work correctly right now.

main entry points
1) M-x scxml-do-new - make a new diagram with no xml backing it.
2) M-x scxml-do-diagram - derive a diagram from a valid scxml xml document in the current buffer.
3) M-x scxml-do-link - derive a diagram from a valid scxml xml document in the current buffer and keep the xml document up to date when changes are made to the document.  Eventually it should also update the diagram when changes are made to the xml document.
