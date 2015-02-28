# imenu-list
Emacs plugin to show the current buffer's imenu entries in a seperate buffer

To activate imenu-list manually, use `M-x imenu-list-minor-mode`.  
To activate it automatically on startup, add this to your init file:
`(imenu-list-minor-mode)`

The imenu of the current buffer will be displayed in the `*Ilist*` buffer. From the `*Ilist*` buffer, you can use these shortcuts:  
\<enter\>: goto entry under cursor  
\<space\>: display entry under cursor, but `*Ilist*` buffer remains current

![](https://github.com/bmag/imenu-list/blob/master/images/imenu-list.png)
