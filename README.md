# dot-files

My . files setup for my linux boxes.

# Usage

Clone this repository and then link all this folder in your home.

Something along those lines should do the trick:

```sh
REPO=/path/to/your/clones/dot-files
for i in $(ls $REPO); do ln -s $REPO/$i ~/; done
```
