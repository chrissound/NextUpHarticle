# {{sectionHeader}} - Input output 

{{gitCommitOffset}}

Right lets add some functions relating to the functionality! We'll just mock this for now.

```haskell
{{ gitDiff src/Main.hs }}
```

Some potentially interesting things here:
`undefined` is essentially a function that returns an error, it's often used as a 'placeholder' to later on add an actual implementation. In Haskell you can't just have an 'empty' function - it has to return a value.
The `_` as the function parameter for `saveTodos` means we are ignoring this value (as we only plan to use this value later on).

