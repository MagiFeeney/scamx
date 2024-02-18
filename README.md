# Scamx: A Minimalist Modal Editing Mechanism for Emacs

Scamx is an experimental modal editing mechanism designed for Emacs. It introduces a modularized, multi-layered approach that integrates seamlessly with default Emacs keybindings, avoiding modifier keys and focusing solely on essential commands. Scamx is crafted to deliver a streamlined and functional modal editing experience within the Emacs environment. As mathematical principles dictate, only mapping matters and those powerful functions are just out there.

## How to Use
* Install `meow` with `package-install`
* Copy the file to your `.emacs.d` directory:
```
git clone https://github.com/MagiFeeney/scamx.git && mv scamx ~/.emacs.d
```
* Load the package with `use-package`:
``` elisp
(use-package scamx
  :load-path "~/.emacs.d/scamx/"
  :config
  (electric-pair-mode))
```

## Modes
There are four major modes: normal, X, convert, and visit, where some minor modes, such as isearch, are wrapped into them. The normal mode serves as a base for basic navigation and modes invoking. And X mode is sequence commands with a prefix "x". Convert mode "c" is bolder compared to normal mode, and visit mode "v" navigates, views and manages buffer or window.


### X mode

* `x f` open a file.
* `x w` write file
* `x s` save buffer
* `x c` save and close emacs
* `x z` minimize window
* `x h` select whole buffer
* `x 0` delete current window
* `x 1` delete other window
* `x 2` split window vertically
* `x 3` split window horizontally
* `x o` move forward to other window
* `x O` move backward to other window
* `x <tab>` indent region
* `x (` start macro
* `x )` end macro
* `x e` call macro
* `x [` backward page
* `x ]` forward page
* `x *` calculator
* `x b` switch to buffer
* `x l` list all buffers
* `x k` kill buffer
* `x i` insert file
* `x j` dired jump
* `x x` exchange point and mark
* `x ;` comment line
* `x ` comment or uncomment region
* `x SPC` pop to mark command
* `x n` duplicate line
* `x ESC` repeat complex command
* `x .` eval last sexp
* `x :` eval expression
* `x =` adjust text scale
* `x <mouse-1>` previous buffer
* `x <mouse-3>` next buffer

### Convert mode
* `g` back to normal mode
* `n` forward paragraph
* `p` backward paragraph
* `f` forward word
* `b` backward word
* `d` kill word or delete region if selected
* `backspace` backward kill word or delete region if selected
* `e` kill sexp
* `a` backward kill sexp
* `k` kill paragraph or kill region if selected
* `h` mark sexp
* `l` kill whole line
* `w` copy
* `y` yank
* `u` undo
* `/` redo
* `a` backward kill sexp
* `e` kill sexp
* `(` backward list
* `)` forward list
* `[` backward sexp
* `]` forward sexp
* `{` backward up list
* `}` up list
* `=` mark sexp

### Visit mode
* `g` back to normal mode
* `l` last buffer
* `n` next buffer
* `p` previous buffer
* `f` other window
* `b` previous window any frame
* `c` clone buffer
* `s` scratch buffer
* `d` scroll up command
* `u` scroll down command
* `e` scroll down line
* `a` scroll up line
* `r` revert buffer
* `0` balance windows
* `m` minimize window
* `M` maximize window
* `+` bold enlarge window horizontally
* `-` bold shrink window horizontally
* `w` swap window
* If you have installed package `ace-window`, then you can further have:
  * `t` select window
  
### Normal mode
* `x` X mode
* `c` convert mode
* `v` visit mode
* `g` cancel selection or exit minibuffer
* `a` move beginning of line
* `e` move end of line
* `d` delete forward one char or delete region if selected
* `backspace` delete backward one char or delete region if selected
* `i` insert
* `o` open below
* `O` open above
* `n` next line
* `p` previous line
* `f` forward char
* `b` backward char
* `j` newline (a.k.a \<return\>)
* `m` back to indentation
* `s` isearch forward minor mode
* `k` kill line at point or kill region if selected
* `h` mark line
* `w` copy
* `y` yank
* `u` undo
* `/` redo
* `t` select to char
* `z` zap to char
* `Z` zap up to char
* `q` quit current buffer
* `;` goto line
* `r` repeat
* `l` recenter top bottom
* `\` delete horizontal space
* `=` mark word
* `SPC` set mark command
* `!` shell command
* `$` ispell word
* `%` query replace
* `,` meow inner of thing
* `.` meow bounds of thing
* `[` meow beginning of thing
* `]` meow end of thing

### Insert mode
* `gg` back to normal mode

### Help mode (Distilled)
* `? k` describe key
* `? c` describe key briefly
* `? f` describe function
* `? m` describe mode
* `? \\` describe input method
* `? b` describe bindings
* `? p` describe package
* `? t` tutorial
* `? d` debugging tutorial
* `? e` view *messages* buffer
* `? r` find manual
* `? i` info overview
* `? s` search for command
* `? v` search by keyword
* `? C-q` quick toggle
* `? ?` further options
* `? q` quit help

To view commands from X mode, you can type `x` followed by `?`.

### Minibuffer
When enter into the minibuffer, it is by default at the insert mode. Once you have finished typing, you can call `gg` to leave insert mode, then use the regular navigation commands such as `n` or `p` to select the candidates. If you have finished selection, you can use `<return>` to abort the minibuffer, or just use `g` to leave if it needs to be stopped.

Visit mode also applies to the minibuffer, which means you can move back-and-forth between buffers and the minibuffer.

### Isearch minor mode
Isearch forward is wrapped into an additional layer. Once it is invoked with the `s` key, it prompts at the minibuffer to enter the string to be searched. Hitting `return` then marks the finish of the input process. After that, you can use the familiar navigation commands `n` or `p` to loop through all candidate in the buffer. When `s` is pressed again, you can enter a new string to search. Otherwise, you can go back to the normal state with `g`. You can also simply leave the input string blank followed by the `return`. With `g`, you can directly leave.

Please check the full set up in `scamx-anchor.el`.

### Mark
* You can hit `SPC` twice to set mark, and then use `x SPC` to return back to the position.
* The convert mode can be invoked on top of the current mark, use whatever commands needed, then revert back to normal mode while keeping the mark.

### Negative argument
* `-` can be combined with directional commands, i.e. `- t` select backward to, `- h` mark backward line.
* You can then use `r` to repeat those commands if needed. 

## Issues and Plans
- In visit mode, it may occasionally not handle the buffer `*Messages*` properly when navigating previous or next buffer at first attempt.
- Command conflicts would be observed in one-key oriented mode i.e. dired-mode, image-mode and magit-mode etc.
  - Now you can switch between those modes and scamx, by entering into the normal mode with `g` and reverting back with `` ` ``. When `g` conflicts with `revert buffer`, you can alternatively use `r` in visit mode.

### TODO list
- [X] Add more functions to convert mode (i.e. up-list, kill-sexp).
- Integrate with multiple-cursors.
- Merge meow of things command series into generalized zap command in a dwim style.
- Allow one command in the normal mode be excuted in the convert mode (like `ctrl-o` in vim).
