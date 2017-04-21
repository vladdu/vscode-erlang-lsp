# vscode-erlang-lsp README

This repository provides both an Erlang language server and a VSCode extension talking with that server.

To begin with, this is meant to be a testbed for the server and there will be few (if any) other features (like syntax highlighting or executing code).
There are other extension that provide those. I think that it will be possible to use these other extensions at the same time.

Later, the server will probably get its own home and more clients will be developed (erlide, the Eclipse IDE, will use it).

## Features

> Describe specific features of your extension including screenshots of your extension in action. Image paths are relative to this README file.

> For example if there is an image subfolder under your extension project workspace:

> \!\[feature X\]\(images/feature-x.png\)

> > Tip: Many popular extensions utilize animations. This is an excellent way to show off your extension! We recommend short, focused animations that are easy to follow.

## Requirements

An Erlang VM 19+ must be installed and accessible on $PATH or at `erlang.erlangPath` for the server to run.

## Build

Build as usual, after updating the server (erlide_ide).

For development, run `vsce package` and install from the resulting `.vsix` file.

## Extension Settings

> Include if your extension adds any VS Code settings through the `contributes.configuration` extension point.

> For example:

> This extension contributes the following settings:

> * `myExtension.enable`: enable/disable this extension
> * `myExtension.thing`: set to `blah` to do something

## Known Issues

> Calling out known issues can help limit users opening duplicate issues against your extension.

## Release Notes

Well, no releases yet.
