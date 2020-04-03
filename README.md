# emacs_python_ide
Settings to make emacs a python-ide

## Prerequisites
The command python3 should be installed.
If not eitehr create a symlink named python3 to python2
or change the variable py-interpreter in the IDE.el to python:
(setq py-interpreter "python3") to (setq py-interpreter "python3")  

## Install
Simply add 
(setq IDE-path "~/.emacs.d/")
(load (mapconcat 'identity (list IDE-path "IDE.el") ""))

to your .emacs

The packages:
- jedi
- flake8
- rope
- importmagic
- yapf
- black
- autopep8
should be automatically installed
with pip.


### Sphinx-Compatible defs
Do a backup of your .emacs.d/elpa/elpy-20190130.2109/snippets/python-mode/defs
Then do a symbolic link:
```
ln -s /home/<wherever_you_saved_this_repo>/defs /home/$USER/.emacs.d/elpa/elpy-20190130.2109/snippets/python-mode/defs
```

## Notes

If not working try to start package-refresh-contents first

## Things I find useful and cool
If you are constantly getting the error: "error while saving the desktop"
Try: M-x desktop-save

In case you get an error the file is not existing, use:
mkdir -p  /home/$USER/.emacs.d/desktop
touch  /home/$USER/.emacs.d/desktop/emacs.desktop

Thank you:
https://stackoverflow.com/questions/12069341/issue-with-desktop-save-mode-not-saving

A very cool feature I love is to permanetnly highlight lines.
For this just use the command:
bm-toggle

There's a shortcut that used to work, but doesn't. Edits are welcome
