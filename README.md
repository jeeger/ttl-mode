This is an Emacs mode for editing Turtle (RDF) files.

It is based on an excellent start made by Hugo Haas.
I've extended it to support indentation, some electric punctuation,
and hungry delete.

To use, download the file `ttl-mode.el` from [Bitbucket](https://bitbucket.org/nxg/ttl-mode)
(or clone the project), put the file in the emacs load path (look at the variable
load-path to find where emacs currently searches), and add something
like the following to your `.emacs` file.

    (autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files" t)
    (add-hook 'ttl-mode-hook    ; Turn on font lock when in ttl mode
              'turn-on-font-lock)
    (setq auto-mode-alist
          (append
           (list
            '("\\.n3" . ttl-mode)
            '("\\.ttl" . ttl-mode))
           auto-mode-alist))

Comments and contributions most welcome.

  * Copyright 2003-2007, [Hugo Haas](http://www.hugoh.net)
  * Copyright 2011-2012, [Norman Gray](https://nxg.me.uk)
  * Copyright 2013, [Daniel Gerber](https://danielgerber.net)
  * Copyright 2016, [Peter Vasil](http://petervasil.net)

`ttl-mode.el` is released under the terms of the
[two-clause BSD licence](https://opensource.org/licenses/bsd-license.php)
(see the `ttl-model.el` header for the licence text).

Norman Gray  
https://nxg.me.uk
