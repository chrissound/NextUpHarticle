# {{sectionHeader}} - Jsonify

{{ gitCommitOffset }}

Right, time to actually make this work!

Right lets add some functions relating to the functionality! We'll just mock this for now.

First we need some additional libraries, so we add them to cabal:

```haskell
{{ gitDiff app.cabal }}
```

We add JSON encoding / decoding instance for our `Todo` data type, functionality to actually save and retrieve the files, and lastly we need add the ability to process a 'save' command.

```haskell
{{ gitDiff src/Main.hs }}
```

Add now you should be able to:
```
stack exec app -- save "Testing todo" 01/01/18
```

{{{{ shellOutput rm todoData.json }}}}
And see output of:
```
{{{{ shellOutput stack exec app -- save "Testing todo" 01/01/18 }}}}
```

And to list all the todos:

```
stack exec app
```

And see output of:
```
{{{{ shellOutput stack exec app }}}}
```

Great! We've got the core functionality implemented!
