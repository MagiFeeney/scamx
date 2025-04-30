# Scamx: A Minimalist Modal Editing Mechanism for Emacs

Scamx is an experimental modal editing mechanism designed for Emacs. It introduces a modularized, multi-layered approach that integrates seamlessly with default Emacs keybindings, avoiding modifier keys and focusing solely on essential commands. Scamx is crafted to deliver a streamlined and functional modal editing experience within the Emacs environment. As mathematical principles dictate, only mapping matters and those powerful functions are just out there.

## How to Use
* Install dependencies `meow`, `multiple-cursors` with `package-install`
* Copy the files to your `.emacs.d` directory:
```
git clone https://github.com/MagiFeeney/scamx.git && mv scamx ~/.emacs.d
```
* Load the package with `use-package` in either `.emacs` or `init.el`:
``` elisp
(use-package scamx
  :load-path "~/.emacs.d/scamx/"
  :config
  (electric-pair-mode)     ; produce balanced expressions upon selected region
  (delete-selection-mode)  ; act without delete or kill
  (multiple-cursors-mode)  ; multiple-cursors integration
  )
```

## Modes
There are four major modes: normal, X, convert, and visit, where some minor modes, such as isearch, are wrapped into them. The normal mode serves as a base for basic navigation and modes invoking. And X mode is sequence commands with a prefix "x". Convert mode "c" is bolder compared to normal mode, and visit mode "v" navigates, views and manages buffer or window.

### X mode
* `x f` open a file.
* `x s` save buffer
* `x c` save and close emacs
* `x z` minimize window
* `x h` select whole buffer
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
* `x j` dired jump
* `x x` exchange point and mark
* `x SPC` pop to mark command
* `x n` duplicate line
* `x ESC` repeat complex command
* `x .` eval last sexp
* `x :` eval expression
* `x =` adjust text scale
* `x <mouse-1>` previous buffer
* `x <mouse-3>` next buffer
* `x \` copy current file path to kill ring
* `x o` org mode map (e.g. org mode or org-roam commands)
* `x m` set mode map (e.g. eshell, python ...)

### Normal mode
* `x` X mode
* `c` convert mode
* `v` visit mode
* `` ` `` motion mode
* `?` help mode
* `g` cancel selection or exit minibuffer
* `a` move beginning of line
* `e` move end of line
* `d` delete forward one char or delete region if selected
* `h` delete backward one char or delete region if selected
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
* `w` copy
* `y` yank
* `u` undo
* `r` redo
* `t` select to char
* `z` zap up to char
* `Z` zap to char
* `q` quit current buffer
* `Q` goto line
* `l` recenter top bottom
* `\` delete horizontal space
* `=` mark word
* `SPC` set mark command
* `!` shell command
* `$` ispell word
* `%` query replace
* `;` comment line
* `'` comment or uncomment region
* `<backspace>` kill whole line

### Convert mode
* `g` back to normal mode
* `n` forward paragraph
* `p` backward paragraph
* `f` forward word
* `b` backward word
* `d` kill word or delete region if selected
* `h` backward kill word or delete region if selected
* `k` kill paragraph or kill region if selected
* `w` copy
* `y` yank
* `u` undo
* `r` redo
* `a` backward kill sexp
* `j` raise sexp
* `e` kill sexp
* `(` backward list
* `)` forward list
* `[` backward sexp
* `]` forward sexp
* `{` backward up list
* `}` forward up list
* `<` move beginning of defun
* `>` move end of defun
* `=` mark sexp
* `,` mark defun
* `s` allow one key in Normal mode be executed

### Visit mode
* `g` back to normal mode
* `l` last buffer
* `n` next buffer
* `p` previous buffer
* `f` other window
* `b` previous window any frame
* `c` clone buffer
* `s` scratch buffer
* `d` scroll page down
* `D` scroll other page down
* `u` scroll page up
* `U` scroll other page up
* `e` scroll line down
* `a` scroll line up
* `r` revert buffer
* `0` delete window
* `1` delete other windows
* `=` balance windows
* `m` minimize window
* `M` maximize window
* `|` split window horizontally
* `_` split window vertically
* `+` bold enlarge window horizontally
* `-` bold shrink window horizontally
* `w` swap window
* `j` move cursor on top, center or bottom of the screen
* `(` tear off window
* `)` delete frame
* If you have installed package `ace-window`, then you can further have:
  * `t` select window
  
#### Multiple cursors in Normal mode
* `[` mc/mark-previous-like-this
* `]` mc/mark-next-like-this
* `<` mc/skip-to-previous-like-this
* `>` mc/skip-to-next-like-this
* `:` mc/mark-all-like-this
* `"` mc/edit-lines
* `@` mc/mark-all-words-like-this
* `#` mc/mark-all-in-region

### Insert mode
* `gg` back to normal mode

### Help mode (Distilled)
* `? k` describe key
* `? c` describe key briefly
* `? f` describe function
* `? m` describe mode
* `? \` describe input method
* `? b` describe bindings
* `? p` describe package
* `? t` tutorial
* `? d` debugging tutorial
* `? e` view \*Messages\* buffer
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

If you want to loop through the history, you can use `n` or `p` in convert mode instead.

Visit mode also applies to the minibuffer, which means you can move back-and-forth between buffers and the minibuffer.

### Isearch minor mode
Isearch forward is wrapped into an additional layer. Once it is invoked with the `s` key, it prompts at the minibuffer to enter the string to be searched. Hitting `return` then marks the finish of the input process. After that, you can use the familiar navigation commands `n` or `p` to loop through all candidate in the buffer. When `s` is pressed again, you can enter a new string to search. Otherwise, you can go back to the normal state with `g`. You can also simply leave the input string blank followed by the `return`. With `g`, you can directly leave.

Please check the full set up in `scamx-anchor.el`.

### Mark
* You can hit `SPC` twice to set mark, and then use `x SPC` to return back to the position.
* The convert mode can be invoked on top of the current mark, use whatever commands needed, then revert back to normal mode while keeping the mark.

### Negative argument
* `-` can be combined with directional commands, i.e. `- t` select backward to a char, `- k` kill backward a line.

## Issues and Plans
- In visit mode, it may occasionally not handle the buffer `*Messages*` properly when navigating previous or next buffer at first attempt.
- Command conflicts would be observed in one-key oriented mode i.e. dired-mode, image-mode and magit-mode etc.

------

Now you can switch between those modes and scamx, by entering into the normal mode with `g` and reverting back with `` ` ``. When `g` conflicts with `revert buffer`, you can alternatively use `r` in visit mode.

### TODO list
#### First Stage
- [X] Add more functions to convert mode (i.e. up-list, kill-sexp).
- [X] Integrate with multiple-cursors.
- [X] Merge meow of things command series into generalized zap command in a dwim style.
- [X] Allow one command in the normal mode be excuted in the convert mode (like `ctrl-o` in vim).
#### Second Stage
- [ ] Motion mode as buffer-wise.
- [ ] Improve selection of inside and outside of balanced expressions.
