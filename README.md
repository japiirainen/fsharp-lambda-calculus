# Lambda Calculus in f#

This is not very interesting. Just testing f# by building a simple lambda-calculus
interpreter.

## run REPL

```sh
dotnet run
```

sample REPL session:

```sh
> \x. x
\x.x
> (\x. x) \y. y
Error: Failed to parse.
> (\x. x \y. y)
y.y
```
