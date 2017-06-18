# Named Clips

Give persistent names to things you copy and paste so you don't have to go through the whole kill ring til you find what you need.


# Installing

Copy the `named-clips.el` file into your .emacs.d directory and add this line to init.el:

```lisp
(load "~/.emacs.d/named-clips.el")
```

Or you could put it at some other path and load that path instead.  If you prefer a different prefix for the keybindings, set the `named-clips-keybinding-prefix` before loading the file, or call `nclip-set-keybindings` with a new prefix as the argument:

```lisp
;; This works:
(setq named-clips-keybinding-prefix "C-c q")
(load "~/.emacs.d/named-clips.el")

;; Or this:
(load "~/.emacs.d/named-clips.el")
(nclip-set-keybindings "C-c q")
```


# Commands and Keybindings

All keybindings have a common prefix followed by a single key.  `C-c n` is the default prefix and here are the default keybindings for the commands:

| Keybinding    | Command       |
| ------------- | ------------- |
| `C-c n n`     | If there is an active region, give it a name, otherwise name the last item on the kill ring |
| `C-c n w`     | Name active region as a clip |
| `C-c n k`     | Name last item on the kill ring |
| `C-c n i`     | Insert named clip at point in current buffer |
| `C-c n l`     | Show list of all currently defined named clips |
| `C-c n r`     | Remove clip with name |
| `C-c n X`     | Clear all named clips |

The first four commands will prompt you for the name of the clip you want.  `C-c n n` is probably the most convenient way of naming clips since you can use it for both naming a region or the last item on the kill ring.  This is is the equivalent of "copy".  `C-c n i` is the equivalent of "paste" and has tab completion when it prompts you for the name.  You will be prompted for confirmation if you attempt to create a clip with the same name as a clip that already exists.

