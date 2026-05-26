# Scamx: Emacs, Rewired

Scamx is a modal editing framework designed for Emacs. By adopting a modular, multi-layered architecture, it integrates seamlessly with existing Emacs keybindings while minimizing reliance on modifier keys. Rather than redefining core functionality, Scamx focuses on providing ergonomic and consistent access to the powerful commands already available within Emacs.

## Installation

```elisp
(use-package scamx
  :ensure t
  :vc (:url "https://github.com/MagiFeeney/scamx/")
  :config
  (electric-pair-mode))
```

> **Note:** For the full setup, please reference `scamx-anchor.el`.

## Core Concepts & Modes

Scamx maximizes ergonomics through a multi-layered modal architecture, reusing a compact set of keys across four global core modes while allowing seamless, mode-specific customization.

#### The Layers

* **Normal (`g` to return):** The base mode for basic navigation and invoking other modes.
* **Convert Mode (`c` prefix):** Bolder, structural editing (words, paragraphs, sexps).
* **Visit Mode (`v` prefix):** Navigates, views, and manages buffers and windows.
* **X Mode (`x` prefix):** Sequence commands for system, buffer, and macro management.

#### Local Control
Scamx also provides an additional layer, **Motion Mode**, which can be customized for specific major modes (e.g. `Org mode`). By default, Motion Mode starts as a blank state unless the target mode already defines a default keymap, such as in `Dired` or `Magit`. Users can freely bind any function to any key according to their workflow.

> **Tip:** You can seamlessly switch between these native modes and scamx. Enter Normal mode with `g` and revert back to Emacs' default state with `` ` ``.

---

## Keybindings

### Normal Mode

Base navigation and editing. Use `` ` `` for motion mode, `?` for help, and `g` to cancel/exit.

| Key | Action | Key | Action |
| --- | --- | --- | --- |
| `/` | `M-x` | `w` / `y` | Copy / Yank |
| `x`/`c`/`v` | Enter X / Convert / Visit mode | `u` / `r` | Undo / Redo |
| `i` | Insert (`gg` to exit) | `k` | Kill line at point/region |
| `a` / `e` | Move to beginning / end of line | `t` | Select to char |
| `d` / `h` | Delete forward / backward (or region) | `z` / `Z` | Zap up to char / Zap to char |
| `o` / `O` | Open line below / above | `q` / `Q` | Quit buffer / Goto line |
| `n` / `p` | Next / Previous line | `l` | Recenter top/bottom |
| `f` / `b` | Forward / Backward char | `\` | Delete horizontal space |
| `j` / `m` | Newline (`RET`) / Back to indentation | `!` / `$` | Shell command / Ispell word |
| `s` | Isearch minor mode | `%` | Query replace |
| `=` | Mark word | `;` / `'` | Comment line / region |
| `SPC` | Set mark command | `,` / `.` | Mark inside / outside pairs dwim |
| `<backspace>` | Kill whole line | `-` | Negative argument prefix |

#### Multiple Cursors (Normal Mode)

| Key | Action | Key | Action |
| --- | --- | --- | --- |
| `[` / `]` | Mark previous / next like this | `<` / `>` | Skip to previous / next like this |
| `:` | Mark all like this | `"` | Edit lines |
| `@` | Mark all words like this | `#` | Mark all in region |

### Convert Mode

Triggered via `c` from Normal mode. Built for structural editing. Exit with `g`.

| Key | Action | Key | Action |
| --- | --- | --- | --- |
| `/` | `M-x` | `a` / `j` / `e` | Backward kill sexp / Raise sexp / Kill sexp |
| `n` / `p` | Forward / Backward paragraph | `(` / `)` | Backward / Forward list |
| `f` / `b` | Forward / Backward word | `[` / `]` | Backward / Forward sexp |
| `d` / `h` | Kill / Backward kill word (or region) | `{` / `}` | Backward / Forward up list |
| `k` | Kill paragraph (or region) | `<` / `>` | Move beginning / end of defun |
| `w` / `y` | Copy / Yank | `=` / `,` | Mark sexp / Mark defun |
| `u` / `r` | Undo / Redo | `s` | Allow one Normal mode key to execute |

### Visit Mode

Triggered via `v` from Normal mode. Built for window and workspace management. Exit with `g`.

| Key | Action | Key | Action |
| --- | --- | --- | --- |
| `l` / `c` / `s` | Last / Clone / Scratch buffer | `0` / `1` | Delete window / Delete other windows |
| `n` / `p` | Next / Previous buffer | `=` / `w` | Balance windows / Swap window |
| `f` / `b` | Other window / Prev window (any frame) | `m` / `M` | Minimize / Maximize window |
| `d` / `D` | Scroll page down / Other page down | `|` / `_` | Split horizontally / vertically |
| `u` / `U` | Scroll page up / Other page up | `+` / `-` | Enlarge / Shrink window horizontally |
| `e` / `a` | Scroll line down / line up | `j` | Move cursor to top/center/bottom |
| `r` | Revert buffer | `(` / `)` | Tear off window / Delete frame |

### X Mode

Triggered via `x` from Normal mode. (To view X mode commands dynamically, type `x ?`).

| Key | Action | Key | Action |
| --- | --- | --- | --- |
| `f` / `s` / `c` | Open / Save / Save & Close Emacs | `b` / `l` / `k` | Switch / List / Kill buffer |
| `z` | Minimize window | `x` | Exchange point and mark |
| `h` | Select whole buffer | `SPC` | Pop to mark command |
| `<tab>` | Indent region | `n` / `=` | Duplicate line / Adjust text scale |
| `(` / `)` / `e` | Start / End / Call macro | `:` / `.` | Eval expression / last sexp |
| `[` / `]` | Backward / Forward page | `\` / `~` | Copy file path to kill ring / Shutdown |
| `j` | Dired jump | `o` / `m` | Org mode map / Set mode map |
| `*` | Calculator | `ESC` | Repeat complex command |

### Distilled Help Mode

Triggered via `?` from Normal mode. Exit with `q`.

| Key | Action | Key | Action |
| --- | --- | --- | --- |
| `k` / `c` | Describe key / Describe key briefly | `p` / `m` | Describe package / Describe mode |
| `f` / `b` | Describe function / bindings | `t` / `d` | Tutorial / Debugging tutorial |
| `s` / `v` | Search command / keyword | `i` / `r` | Info overview / Find manual |
| `\` / `e` | Describe input method / View *Messages* | `C-q` / `?` | Quick toggle / Further options |

---

## Special Contexts & Features

* **Minibuffer:** Defaults to Insert mode (`i`). Type `gg` to leave insert mode, use `n` / `p` to select candidates, and `<RET>` to confirm. Use `g` to abort.

> **Note:** Visit mode (`v`) works here, allowing back-and-forth movement between the minibuffer and other buffers. You can also use `c` (Convert mode) `n`/`p` to loop through history.

* **Isearch (`s`):** Prompts in the minibuffer. Hit `<RET>` to finish input, then use `n` / `p` to loop through buffer candidates. Press `s` again to search a new string (or `e` to modify the last string), or `g` to exit.
* **Mark Management:** Hit `SPC` twice to set a mark, and `x SPC` to return to it. You can invoke Convert mode over a mark, execute commands, and return to Normal mode while retaining the mark.
* **Negative Arguments:** Combine `-` with directional commands (e.g., `- t` to select backward to a char, `- k` to kill backward a line).

## Motion Mode in Practice
Motion Mode complements the core layers by providing a private, mode-specific set of keybindings. It is particularly useful for modes that define specialized functions or keybinding overrides.

For example, in `Org mode`, navigating between headings normally requires `C-c C-n` and `C-c C-p`. With Scamx, these commands can be rebound to simpler keys such as `n` and `p`, enabling a more streamlined workflow. The same approach can be extended to other frequently used commands. Users are free to customize both the bindings and the underlying functions according to their preferences.

```elisp
(scamx-motion-define-key
  '("g" . meow-motion-exit)		; applied globally
  (org-mode
   '("=" . org-mark-element)
   '("n" . org-next-visible-heading)
   '("p" . org-previous-visible-heading)
   '("RET" . org-meta-return)
   '("t" . org-insert-todo-heading)
   '("h" . org-insert-heading-respect-content)
   '("j" . org-insert-todo-heading-respect-content)
   '("f" . org-shiftright)
   '("b" . org-shiftleft)
   '("u" . org-shiftup)
   '("d" . org-shiftdown)
   '("l" . org-insert-link)
   '("i" . org-toggle-inline-images)
   '(">" . org-goto-calendar)
   '("e" . org-set-effort)
   '("C-i" . org-clock-in)
   '("C-o" . org-clock-out))
  ;; add more here
  )
```

---

## Issues & Roadmap

### Issues
* **Visit Mode:** May occasionally mishandle the `*Messages*` buffer on the first navigation attempt.
* **Command Conflicts:** Occur in natively one-key oriented modes (e.g., `dired-mode`, `image-mode`, `magit-mode`). If `g` conflicts with a native `revert buffer`, use `r` in Visit mode instead.

### Development

* [x] Add more functions to convert mode (e.g., up-list, kill-sexp).
* [x] Integrate with `multiple-cursors`.
* [x] Merge `meow` *of things* command series into a generalized, DWIM-style zap command.
* [x] Allow single Normal mode commands to execute inside Convert mode (similar to `C-o` in Vim).
* [X] Implement Motion mode as buffer-wise.
* [X] Improve selection mechanics for the inside/outside of balanced expressions.
* [ ] Implement Scamx multiple cursors
