# {{sectionHeader}} - First steps

{{gitCommitOffset}}

Right so we'll be storing the values as a JSON file. No we won't be using Hadoop, a database or some crazy quantum encryption device - let's be pragmatic.

First we need to add the time library to cabal: 

```
{{ gitDiff app.cabal }}
```

Lets define the types we know so far, and some imports.

```haskell
{{ file src/Main.hs }}
```
