# emacs_python_ide
Settings to make emacs a python-ide


## Install
Simply add 
(load-file "/path/to/IDE.el")

to your .emacs

optinally install:
- jedi
- flake8
- rope
- importmagic

###Sphinx-Compatible defs
Do a backup of your .emacs.d/elpa/elpy-20190130.2109/snippets/python-mode/defs
Then do a symbolic link:
```
ln -s /home/<wherever_you_saved_this_repo>/defs /home/$USER/.emacs.d/elpa/elpy-20190130.2109/snippets/python-mode/defs
```

## Notes

If not working try to start package-refresh-contents first
