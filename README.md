# joejoe
joejoe is a minor mode for Emacs that seeks to emulate [Joe's Own Editor's](https://github.com/jhallen/joe-editor) Wordstar-like keybindings and features. Because Joe does not have some of the same features as Emacs, joejoe also features some new keybindings to make things easier.

The current version of joejoe is incomplete as not all of Joe's features and keybindings have been implemented yet.

# joejoe and other modes 

Joejoe exists in-part to be an alternative to evil-mode, which is rather invasive to Emacs and makes it difficult to use specific modes.  Currently, joejoe has issues with some modes because of its redefinition of the backspace character, in particular with vterm-mode.  These modes unfortunately require that joejoe be turned off at this point, but this bug will be fixed in the future. 
