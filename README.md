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

The Haskell libraries in the Plutus Platform are built with Cabal and Nix. The other artifacts (docs etc.) are also most easily built with Nix.

1. Install [Nix](https://nixos.org/nix/)

    ``$ sh <(curl -L https://nixos.org/nix/install) --daemon``

2. Make sure you have setup IOHK Cache files for Nix (DO NOT IGNORE THIS)

    ### How to set up the IOHK binary caches

    Adding the IOHK binary cache to your Nix configuration will speed up
    builds a lot, since many things will have been built already by their CI.
    If you find you are building packages that are not defined in the 
    plutus-apps repository or if the build seems to take a very long time then you may not have this set up properly.

    To setup the cache on non-NixOS, edit `/etc/nix/nix.conf` and add the following lines:

    ```
    substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
    trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
    ```

    If you don't have an `/etc/nix/nix.conf` or don't want to edit it, you may add the `nix.conf` lines to `~/.config/nix/nix.conf` instead.
    You must be a [trusted user](https://nixos.org/nix/manual/#ssec-multi-user) to do this.


## Weekly Steps

### First look for this week's tag:

```
$ cd ~/workspace/plutus-pioneers-program

$ git pull

$ cd ~/workspace/plutus-pioneers-program/code/week0Y

(0Y == number of the week/lecture)

$ cat cabal.project 
(search for 'source-repository-package' where 'location' is: https://github.com/input-output-hk/plutus-apps.git and copy the 'tag')
```

### Now checkout this week's tag on plutus-app repo:

```
$ cd ~/workspace/plutus-apps

$ git checkout main

$ git pull

$ git checkout 'this-week-tag'
```

### Build and run Playground Server (if needed):

```
$ nix-build -A plutus-playground.server (optional)

$ nix-shell

$ cd plutus-playground-server/

$ plutus-playground-server (run Plutus Playground Server)
```

### Build and run Playground Client (if needed):

```
open another terminal (from ~/workspace/plutus-apps)

$ nix-shell

$ cd plutus-playground-client

$ npm run start (to start Plutus Playground Client)
```


### Start Repl:

```
open another terminal (from ~/workspace/plutus-apps)

$ nix-shell

$ cd ../plutus-pioneers-program

$ cd code/week0Y/

(cabal update - optional)

$ cabal build

$ cabal repl
```