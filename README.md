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
$ emacs #Have fun, but this might take just a little
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

If not, use:
```pip3 install --user black flake8 jedi rope importmagic```


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

## Potential Errors (Possibly Ubuntu)

```
Elpy is creating the RPC virtualenv (’/home/marioschwaiger/.emacs.d/elpy/rpc-venv’)
elpy-rpc--create-virtualenv: Elpy needs the ’virtualenv’ or ’venv’ python packages to create its virtualenv. Please install one of them or disable the dedicated virtualenv with ‘(setq elpy-rpc-virtualenv-path ’current)‘
```

virtualenv is missing, just install:
```sudo apt install virtualenv```

### Neither easy_install nor pip found

```Try installing a more recent version of python-pycompile, and please open a bug report if the issue persists in the latest release.  Thanks!
elpy-insert--pip-button-value-create: Neither easy_install nor pip found
```

Just install:
```sudo apt install python3-pip```

###NameError: name 'quiet' is not defined

```
During handling of the above exception, another exception occurred:

Traceback (most recent call last):
  File "/usr/lib/python3.8/runpy.py", line 194, in _run_module_as_main
    return _run_code(code, main_globals, None,
  File "/usr/lib/python3.8/runpy.py", line 87, in _run_code
    exec(code, run_globals)
  File "/usr/lib/python3.8/py_compile.py", line 218, in <module>
    sys.exit(main())
  File "/usr/lib/python3.8/py_compile.py", line 213, in main
    if quiet < 2:
NameError: name 'quiet' is not defined

Try installing a more recent version of python-pycompile, and please open a bug report if the issue persists in the latest release.  Thanks!
mwheel-scroll: End of buffer
```
###Error: (error "autopep8 command not found.")

```
sudo apt-get install python3-autopep8

#OR

pip3 install autopep8

```

### Neotree doesn's show symbols
```M-x all-the-icons-install-fonts```

## Dealing with other Errors
```
M-x toggle-debug-on-error
```
