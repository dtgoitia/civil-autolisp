# Civil AutoLISP

This project contains a bunch of ~~functions~~ scripts I use every day -literally- at work. Everything works around *AutoCAD*, and it is focused on civil engineering.

## Getting Started

These instructions will help you to be ready to start in few minutes.

### Prerequisities

Any *AutoCAD* full version.

As all the project is written for *AutoCAD*, it should work fine in any of the *AutoCAD* derivatives too (*AutoCAD Civil 3D*, etc.), as long as they support *AutoLISP* and *VLISP* languages. This project will not run in any LT version.

### Installing

There are many different ways to load a `.lsp` file:

To load a `.lsp` file temporarily, drag it file into *AutoCAD*'s *Model Space* and select *Load Once* as shown below:
![alt text](https://github.com/dtgoitia/civil-autolisp/blob/master/README/load_lsp_file.gif "Load LISP file. Clic and drag")

If you want *AutoCAD* to remember your choice, you can select *Always Load* and you won't be asked each time you load it.

Another option is to add a directory to *Trusted Locations*. This way, any file within this directory will be *trusted*. Thus, the message above will not pop up when you load any file from this *trusted* directory.

## Executing

Open the `.lsp` file with a text editor and look for `(defun c:`. The characters immediately after this are the triggers of the command.

Let's suppose that you load an hypothetical `hello.lsp` file which looks like this:
```lisp
(defun c:greeting()
  (alert "Hi!")
);END defun
```
Once loaded, go to the command line and type `greeting`. If everything is correct, *AutoCAD* should suggest you the `GREETING` command even before you finish typing it. Hit ENTER to execute it.

## To-do

- [ ] Tidy up dump folder.
- [ ] Map all dependencies.
- [ ] Reorganize the whole repository.
  - [ ] Keep core scripts in `master` branch.
  - [ ] Move extra features to `testing` branches.

## Author

**David Torralba Goitia** ([@dtgoitia](https://github.com/dtgoitia))

See also the list of [contributors](https://github.com/dtgoitia/civil-autolisp/graphs/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](https://github.com/dtgoitia/civil-autolisp/blob/master/LICENSE.md) file for details

## Acknowledgments

[Javier Domingo Cansino](https://github.com/txomon/) for introducing me to *Git* and *GitHub*, and contributing with elegant and effective solutions in error handling functions.
