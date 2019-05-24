# joestar
joestar is a minor mode for Emacs that seeks to emulate [Joe's Own Editor's](https://github.com/jhallen/joe-editor) Wordstar-like keybindings and features.

The current version of joestar is incomplete as not all of Joe's features and keybindings have been implemented yet.

# joestar and other modes 

joestar exists in-part to be an alternative to evil-mode, which is rather invasive to Emacs and makes it difficult to use specific modes that don't support it.  Currently, joestar has issues with some modes because of its redefinition of the backspace character, in particular with vterm-mode.  These modes unfortunately require that joestar be turned off at this point, but this bug will be fixed in the future. 
