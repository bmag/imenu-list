# imenu-list
Emacs plugin to show the current buffer's imenu entries in a seperate buffer

To activate imenu-list manually, use `M-x imenu-list-minor-mode`.  
To activate it automatically on startup, add this to your init file:
`(imenu-list-minor-mode)`

The imenu of the current buffer will be displayed in the `*Ilist*` buffer. From the `*Ilist*` buffer, you can use these shortcuts:  
- `<enter>`: goto entry under cursor, or toggle case-folding.  
- `<space>`: display entry under cursor, but `*Ilist*` buffer remains current  
- `<mouse click>`: same as \<enter\>  
- `<tab>`: next line  
- `<backtab>`: previous line  
- `n`: next line  
- `p`: previous line  
- `f`: toggle case-folding (`hs-toggle-hiding`)  

## Display
imenu-list has several faces for showing different levels of nesting in the `*Ilist*` buffer. To customize them, see `M-x customize-group RET imenu-list RET`.

The mode-line of `*Ilist*` buffer can be changed by customizing `imenu-list-mode-line-format`, also available via `M-x customize-group RET imenu-list RET`.

Here are some pictures. Note that you can hide/show parts of the imenu list.

![](https://github.com/bmag/imenu-list/blob/master/images/imenu-list-light.png)

![](https://github.com/bmag/imenu-list/blob/master/images/imenu-list-dark.png)
