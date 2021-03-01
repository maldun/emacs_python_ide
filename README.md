# emacs_python_ide
Settings to make emacs a python-ide

## Prerequisites
The command python3 should be installed.
If not either create a symlink named python3 to python2
or change the variable py-interpreter in the IDE.el to python:
```
(setq py-interpreter "python3") to (setq py-interpreter "python3")  
```

## Install

```
$ sudo dnf install emacs #Basic Install
$ emacs #Needs to be started once so you get the .emacs.d folder. Close again after the successful start
$ git clone https://github.com/maldun/emacs_python_ide .emacs.d/
#Adding: 
$ echo '(setq IDE-path "~/.emacs.d/")' >> .emacs
$ echo "(load (mapconcat 'identity (list IDE-path \"IDE.el\") \"\"))" >> .emacs
$ emacs #Have fun
```

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
ln -s /home/<wherever_you_saved_this_repo>/defs /home/$USER/.emacs.d/elpa/elpy-DATE.VERSION/snippets/python-mode/defs
```

I personally find the snippet in pdbpm pretty useful to be able to debug on failure. It doesn't exist, so no backup required.
```
ln -s /home/<wherever_you_saved_this_repo>/pdbpm /home/$USER/.emacs.d/elpa/elpy-DATE.VERSION/snippets/python-mode/pdbpm
```



## Notes

If not working try to start package-refresh-contents first

## Things I find useful and cool
If you are constantly getting the error: "error while saving the desktop"
Try: ```M-x desktop-save```

In case you get an error the file is not existing, use:
```
mkdir -p  /home/$USER/.emacs.d/desktop
touch  /home/$USER/.emacs.d/desktop/emacs.desktop
```
Thank you:
https://stackoverflow.com/questions/12069341/issue-with-desktop-save-mode-not-saving

A very cool feature I love is to permanetnly highlight lines.
For this just use the command:
```bm-toggle```

There's a shortcut that used to work, but doesn't. Edits are welcome
