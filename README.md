# An experimental modal editing

This is my first attempt that designs a modularized, multi-layer functionalized and minimal-effort modal editing mechanism. It combines with default keybindings of emacs, but without modifier keys and only some necessary commands are binded. This is neither a package nor a under-development project.


## How to Use
* Install `meow` with `package-install`
* Copy the file to your `.emacs.d` directory:
```
git clone https://github.com/MagiFeeney/scamx.git
mv scamx ~/.emacs.d
```
* Load all files by order in your `.emacs` or `init.el`:
``` elisp
(load-file "~/.emacs.d/scamx/function.el")
(load-file "~/.emacs.d/scamx/minibuffer.el")
(load-file "~/.emacs.d/scamx/X.el")
(load-file "~/.emacs.d/scamx/convert.el")
(load-file "~/.emacs.d/scamx/visit.el")
(load-file "~/.emacs.d/scamx/scamx.el")
(load-file "~/.emacs.d/scamx/insert.el")
(load-file "~/.emacs.d/scamx/exit.el")
```

## Modes
There are four major modes i.e. normal, X, convert and visit mode. The normal mode serves as a base for basic navigation and modes invoking. And X mode is sequence commands with a prefix "x". Convert mode "c" is bolder compared to normal mode, and visit mode "v" navigates, views and manages buffer or window.


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
* `x b` switch to buffer (recommend using consult-buffer to utilize the minibuffer feature)
* `x k` kill buffer
* `x i` insert file
* `x j` dired jump
* `x x` exchange point and mark
* `x ;` comment line
* `x '` comment or uncomment region
* `x SPC` pop to mark command
* `x n` duplicate line

### Convert mode

* `g` back to normal mode
* `n` forward paragraph
* `p` backward paragraph
* `f` forward word
* `b` backward word
* `d` kill word
* `D` backward kill word
* `e` forward sentence
* `a` backward sentence
* `k` kill sentence
* `K` backward kill sentence
* `h` mark paragraph
* `u` undo
* `y` yank
   
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
* If you have installed package `ace-window`, then you can further have:
  * `w` swap window
  * `t` select window
### Normal mode
* `x` X mode
* `c` convert mode
* `v` visit mode
* `,` meow inner of thing
* `.` meow bounds of thing
* `[` meow beginning of thing
* `]` meow end of thing
* `a` move beginning of line
* `e` move end of line
* `d` delete forward one char
* `D` delete backward one char
* `g` cancel selection or exit minibuffer
* `b` backward char
* `i` insert
* `o` open below
* `O` open above
* `n` next line
* `p` previous line
* `f` forward char
* `j` newline (a.k.a \<return\>)
* `m` back to indentation
* `s` isearch forward (recommend using swiper to utilize the minibuffer feature)
* `y` yank
* `q` quit current buffer
* `Q` goto line (recommend using consult-goto-line to utilize the minibuffer feature)
* `k` kill line at point or kill region if selected
* `t` select to char
* `u` undo
* `/` redo
* `h` mark line
* `w` copy
* `z` zap to char
* `r` repeat
* `l` recenter top bottom
* `\` delete horizontal space
* `=` mark word
* `SPC` set mark command

### Insert mode
* `gg` back to normal mode

### Minibuffer
When enter into the minibuffer, it is by default at the insert mode. Once you have finished typing, you can call `gg` to leave insert mode, then use the regular navigation commands such as `n` or `p` to select the candidates. If you have finished selection, you can use `<return>` to abort the minibuffer, or just use `g` to leave if it needs to be stopped.

Visit mode also applies to the minibuffer, which means you can move back-and-forth between buffers and the minibuffer.

### Mark
* You can hit twice to set mark, and then use `x SPC` to return back to the position.
* The convert mode can be invoked on top of the current mark, use whatever commands needed, then revert back to normal mode while keeping the mark.

### Negative argument
* `-` can be combined with directional commands, i.e. `- t` select backward to, `- h` mark backward line.
* You can then use `r` to repeat those commands if needed. 

## Issues and Plans
In visit mode, it may occasionally not handle the buffer `*Messages*` properly when navigating previous or next buffer.

### TODO list
- Add more functions to convert mode (i.e. up-list, kill-sexp).
- Integrate with multiple-cursors.
- Merge meow of things command series into generalized zap command in a dwim style.
- Allow one command in the normal mode be excuted in the convert mode (like `ctrl-o` in vim).
