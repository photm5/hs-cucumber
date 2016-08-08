# Trying out the bootstrap

I wrote this implementation using the [cucumber-bootstrap][1] method.  This
document shows you how to validate yourself that the tests actually pass.  I
assume you use the Nix package manager.

Assuming you are already in a temporary directory you want to try this out in,
first clone the two repositories:

```sh
git clone https://github.com/cucumber/cucumber-bootstrap
git clone https://github.com/shak-mar/hs-cucumber
```

Since there is a [problem][2] with json 1.6.1 and latest ruby versions, you may
want to apply a [patch][3] to use a newer version of json that fixes the
problem:

```sh
pushd cucumber-bootstrap
git am ../hs-cucumber/doc/supporting-files/bootstrap-Use-json-1.8.2.patch
popd
```

With a bit of prayer (uninformed me sadly doesn’t know of any real ruby
packaging for Nix that actually works), the following bundler run will succeed,
leaving you with the required ruby things:

```sh
pushd cucumber-bootstrap
nix-shell -p bundler --run 'bundler --binstubs bin --path gems
popd
```

Next, compile the `cucino` binary I implemented in Haskell:

```sh
pushd hs-cucumber
nix-shell --run 'cabal build'
popd
```

You’re ready to go!  Try this:

```sh
pushd cucumber-bootstrap
nix-shell -p bundler ruby --run 'PATH=$PATH:../hs-cucumber/dist/build/cucino/ ./bin/cucumber'
popd
```

[1]: https://github.com/cucumber/cucumber-bootstrap
[2]: https://github.com/flori/json/issues/229
[3]: https://github.com/shak-mar/hs-cucumber/blob/master/doc/supporting-files/bootstrap-Use-json-1.8.2.patch
