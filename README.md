# emacs_python_ide
Settings to make emacs a python-ide

## Prerequisites
The command python3 should be installed.
If not eitehr create a symlink named python3 to python2
or change the variable py-interpreter in the IDE.el to python:
(setq py-interpreter "python3") to (setq py-interpreter "python3")  

## Install
Simply add 
(load-file "/path/to/IDE.el")

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
