# imenu-list
Emacs plugin to show the current buffer's imenu entries in a seperate buffer

To activate imenu-list manually, use `M-x imenu-list-minor-mode`.  
To activate it automatically on startup, add this to your init file:
`(imenu-list-minor-mode)`

You can also use `M-x imenu-list-minor-mode` to toggle imenu-list (and its window) on and off.
You may wish to bind it to a key, for example `C-'`:
```elisp
(global-set-key (kbd "C-'") #'imenu-list-minor-mode)
```

The imenu of the current buffer will be displayed in the `*Ilist*` buffer. From the `*Ilist*` buffer, you can use these shortcuts:  
- `<enter>`: goto entry under cursor, or toggle case-folding.  
- `<space>`: display entry under cursor, but `*Ilist*` buffer remains current  
- `<mouse click>`: same as \<enter\>  
- `<tab>`: next line  
- `<backtab>`: previous line  
- `n`: next line  
- `p`: previous line  
- `f`: toggle case-folding (`hs-toggle-hiding`)  

Some users might prefer the `imenu-list-minor-mode` command to also set the focus to the `*Ilist*` window.
To do so, use the variable `imenu-list-focus-after-activation`:
```elisp
(setq imenu-list-focus-after-activation t)
```

## Display
imenu-list has several faces for showing different levels of nesting in the `*Ilist*` buffer. To customize them, see `M-x customize-group RET imenu-list RET`.

The mode-line of `*Ilist*` buffer can be changed by customizing `imenu-list-mode-line-format`, also available via `M-x customize-group RET imenu-list RET`.

Here are some pictures. Note that you can hide/show parts of the imenu list.

![](https://github.com/bmag/imenu-list/blob/master/images/imenu-list-light.png)

![](https://github.com/bmag/imenu-list/blob/master/images/imenu-list-dark.png)

## Window Position and Size
The size and position of `*Ilist*` window can be changed by customizing these variables:
- `imenu-list-position`: should be `left`, `right`, `above` or `below`, to display the window
at the left, right, top or bottom of the frame.
- `imenu-list-size`: should be a positive integer or a percentage. If integer, decides the total
number of rows/columns the window has. If percentage (0 < `imenu-list-size` < 1), decides the
number of rows/columns relative to the total number of rows/columns in the frame.

imenu-list controls its display by adding an entry to `display-buffer-alist`. If you want
fuller control over how the window is displayed, you should replace that entry.

If imenu-list can't open a new window (could happen when the frame is small or already split into many windows),
the window will be displayed using the regular rules of `display-buffer`.

### window-purpose
For users of `window-purpose`, imenu-list adds an entry to `purpose-special-action-sequences`.
If you want fuller control over how the window is displayed, you should replace that entry.
