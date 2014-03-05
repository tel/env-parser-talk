
# The `env-parser` talk/repository

*This repository is a tutorial.* It was built to guide a talk on applicative
functors and is intended to be "read" commit-by-commit. Additionally, it forms
a good example of how to build, test, and release an open source Haskell
package. To begin, run

```bash
> git checkout the-beginning
```

and then continue by stepping through the repository.

## Description

This package provides an "environment parser" capable of collecting
application configuration information from the system environment nicely
while also providing static documentation. By default, Haskell includes only
the `lookupEnv` function

```haskell
lookupEnv :: String -> IO (Maybe String)
```

and it's not uncommon to use ad-hoc `IO` computations structured to build
program configuring using this primitive. However, since the environment
forms a part of the public interface of a program, it'd be nice if our
"environment parsing" were self-documenting. The end goal is to enclose most
of the practice of environment parsing in a single type, highly inspired by
`Parser` types like `parsec` or `attoparsec`. This type should be useful not
only for producing the program's runtime environment configuration but also
for documenting the environment configuration interface the program exposes.

## Changelog

For future readers following along, the `Changelog` file may be of use as it
summarizes the high-level changes at each commit.

## Notes on stepping through the repository

`git` does not include a method of single-stepping through a repository
history. This is unfortunate because that's exactly the best way to explore
the material here. To fix this I've included a `git` command in a local
Makefile which steps to the next commit

```bash
> make step
```
