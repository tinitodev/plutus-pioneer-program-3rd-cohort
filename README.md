# Plutus Pioneer Program 3rd Cohort

[Week-01](lectures/week01/) \
[Week-02](lectures/week02/) \
[Week-03](lectures/week03/) \
[Week-04](lectures/week04/) \
[Week-05](lectures/week05/) \
[Week-06](lectures/week06/) \
[Week-07](lectures/week07/) \
[Week-08](lectures/week08/) 


## Initial Setup





## Weekly Steps

First look for this week's tag:

```
$ cd ~/workspace/plutus-pioneers-program

$ git pull

$ cd ~/workspace/plutus-pioneers-program/code/week0Y

(0Y == number of the week/lecture)

$ cat cabal.project 
(search for 'source-repository-package' where 'location' is: https://github.com/input-output-hk/plutus-apps.git and copy the 'tag')
```

Now checkout this week's tag on plutus-app repo:

```
$ cd ~/workspace/plutus-apps

$ git checkout main

$ git pull

$ git checkout 'this-week-tag'
```

Build and run Playground Server (if needed):

```
$ nix-build -A plutus-playground.server (optional)

$ nix-shell

$ cd plutus-playground-server/

$ plutus-playground-server (run Plutus Playground Server)
```

Build and run Playground Client (if needed):

```
open another terminal (from ~/workspace/plutus-apps)

$ nix-shell

$ cd plutus-playground-client

$ npm run start (to start Plutus Playground Client)
```


Start Repl:

```
open another terminal (from ~/workspace/plutus-apps)

$ nix-shell

$ cd ../plutus-pioneers-program

$ cd code/week0Y/

(cabal update - optional)

$ cabal build

$ cabal repl
```