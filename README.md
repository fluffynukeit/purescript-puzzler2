# purescript-puzzler2
A reimplementation of purescript-puzzler (https://github.com/fluffynukeit/purescript-puzzler) using actions based architecture instead of MVI

There are a number of things that bothered me about my MVI implementation.  I decided to try out new experimental architecture in which GUI state was carried along in event handling closures, but that had some problems of its own.

I'll probably write a post on [my blog](http://www.fluffynukeit.com), so search there if you want to read it.
