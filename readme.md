# Webport of Naproche-SAD

## How to build

 1. Install `try-reflex` to get `nix` with all the dependencies:
    https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst
    
    (If mmorph can't be build on your system, you may need to use v0.6.2.0 
    , see [this issue](https://github.com/reflex-frp/reflex-platform/issues/717))

 2. Copy `Naproche-SAD` into `frontend/Naproche-SAD`
    (or tell me how to get submodules to run without larger problems).
 3. Delete `frontend/Naproche-SAD/Naproche-SAD.cabal`.
    (This needs to be done only if you encounter an error ".. .cabal was built with a newer hpack, please upgrade".)
 4. Delete in `frontend/Naproche-SAD/package.yaml` everything after the `library` definition.
    This is important, because otherwise the auto-generated `default.nix` (which we can't override)
    will demand the `Isabelle` library. The `Isabelle` library depends on `network` and can't be included here.
 5. `cd` into this projects root directory and run `nix-build -o frontend-result -A ghcjs.frontend`.
 6. Delete the last line of `frontend-result/bin/frontend.jsexe/all.js` and add this code

```javascript
var closure = "start";

onmessage = function(msg) {
    if(closure === "start") {
        closure = null;
        h$main(h$mainZCZCMainzimain);
    } else {
        if(closure !== null) {
            var cl = closure
            closure = null;
            cl(msg);
        } else {
            console.log("Unhandled by Naproche Worker: ");
            console.log(msg);
        }
    }
};

var requestMessage = function(msg, c) {
    postMessage(msg);
    closure = c;
}
```

  7. Now you can include `all.js` as a webworker and communicate with it :)
  8. Change, rebuilt by going to step 5 and open a PR :)