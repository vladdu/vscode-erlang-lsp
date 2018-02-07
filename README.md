# erlang README

This repository provides a VSCode extension that interfaces with the Erlang language server at https://github.com/erlang/sourcer.

To begin with, this is meant to be a testbed for the server and there will be few (if any) other features (like syntax highlighting or executing code). For these, we recommend the `pgourlain.erlang` extension, that seems to have most functionality at this moment.

## Features

> Describe specific features of your extension including screenshots of your extension in action. Image paths are relative to this README file.

> For example if there is an image subfolder under your extension project workspace:

> \!\[feature X\]\(images/feature-x.png\)

> > Tip: Many popular extensions utilize animations. This is an excellent way to show off your extension! We recommend short, focused animations that are easy to follow.

## Requirements

An Erlang VM 20+ must be installed and accessible on $PATH or at `erlang.erlangPath` for the server to run.

## Build

The build scripts assume that the `sourcer` repository containing the language server is cloned in a sibling directory of this projects'. 

In order to produce a `.vsix` file that can be installed, execute `vsce package`. This will compile and package the language server, copy it here and archive everything.

For development, use vscode and in a terminal run `npm install`, then `./install_server`, then press F5 to launch a vscode instance that includes the current extension.

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
