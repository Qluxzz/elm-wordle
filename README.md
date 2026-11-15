# Elm Wordle

A clone of the popular word guessing game Wordle.

## How to play:

Wordle is a word game where you have six attempts
to guess the correct five letter word

For each attempt you will get the result of that attempt:

If a letter is in the correct spot it is marked with a green background\
If a letter appears in the word but is in the wrong place, it is marked with an orange background\
If the letter does not appear in the word, it is marked with a gray background

## Development

- `npm run dev` starts the Vite dev server with the Elm debugger enabled
- `npm run build` builds and minimizes the app into `dist/`
- `npm run test` runs the test suite
- `npm run review` runs elm-review

Made with [Elm](https://elm-lang.org/)

# Elm template

Elm template with the most things I use when creating a new Elm thing

## Effects instead of Cmds

Using effects instead of the built in Cmd type allow you to test your program end to end, simulate Cmds like HTTP calls or getting system time.
Here's more documentation on the benefits: https://github.com/avh4/elm-program-test/blob/main/docs/cmds.md

This template also includes an initial test which tests the initial document structure

## Service worker for offline access

Since most of my Elm apps are client side only, we can also make them work offline by adding a service worker.
The service worker caches the resources used by the template and makes it so it can load even if offline.

## Pipeline

The included pipeline include three steps:

- A test step that runs all tests and reports if all tests succeeded
- A build step that compiles the code and optimizes/minimizes it
- A publishing step that publishes it to Github pages

## Keeping a template created repo up to date

```sh

git remote add upstream https://github.com/Qluxzz/elm-template.git
git fetch upstream
git config rerere.enabled true    # remembers conflict resolutions between merge

# First merge needs --allow-unrelated-histories to allow merging the two repos together
git merge upstream

```
