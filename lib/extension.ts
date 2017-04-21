'use strict';
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as path from 'path';
import * as FS from 'fs';
import * as PortFinder from 'portfinder';
import * as Net from 'net';
import * as ChildProcess from 'child_process';

import { LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions, TransportKind, StreamInfo } from 'vscode-languageclient';

export function activate(context: vscode.ExtensionContext) {

    // we want v19+
    let erlExecutablePath = findErlangExecutable('/home/vlad/erlide_tools/19.3/bin/escript');

    let clientOptions: LanguageClientOptions = {
        // Register the server for erlang documents
        documentSelector: ['erlang'],
        synchronize: {
            // Synchronize the setting section 'erlang' to the server
            configurationSection: 'erlang',
            // Notify the server about relevant file changes in the workspace
            fileEvents: [
                vscode.workspace.createFileSystemWatcher('**/erlang.config.json'),
                vscode.workspace.createFileSystemWatcher('**/*.erl'),
                vscode.workspace.createFileSystemWatcher('**/*.hrl'),
                vscode.workspace.createFileSystemWatcher('**/*.yrl'),
                vscode.workspace.createFileSystemWatcher('**/*.app.src'),
                vscode.workspace.createFileSystemWatcher('**/rebar.config')
            ]
        }
    }

    function createServer(): Promise<StreamInfo> {
        return new Promise((resolve, reject) => {
            let myCwd = context.extensionPath

            let port = 14902;
            PortFinder.getPort(function (err, port) {
                let args = [
                    'erlide_server', '-p', port.toString()
                ];

                console.log('>> erl_lsp:: ' +myCwd + ":: " + erlExecutablePath + ' @ ' + args.join(' '));
                let options = {
                    stdio: 'inherit',
                    env: { "HOME": "/home/vlad" },
                    cwd: myCwd
                };
                // Start the child process
                let erl = ChildProcess.execFile(erlExecutablePath, args, options, (error, stdout, stderr) => {
                    if (error) {
                        throw error;
                    }
                });
                erl.stdout.on('data', (data) => {
                    console.log("$> " + data);
                });
                var waitForSocket = require('socket-retry-connect').waitForSocket;
                waitForSocket({ port: port }, function(err, socket) {
                    resolve({ reader: socket, writer: socket });
                });
 
            });
        });
    }

    // Create the language client and start it.
    let client = new LanguageClient('Erlang Server', createServer, clientOptions);
    client.onReady().then(
        () => console.log('Server ready!'),
        (reason) => vscode.window.showErrorMessage("Could not start Erlang language service: " + reason));
    let aclient = client.start();

    // Push the client to the context's subscriptions so that the 
    // client can be deactivated on extension deactivation
    context.subscriptions.push(aclient);
}

// this method is called when your extension is deactivated
export function deactivate() {
}

function findErlangExecutable(binname: string) {
    binname = correctBinname(binname);

    // Then search PATH parts
    if (process.env['PATH']) {
        let pathparts = process.env['PATH'].split(path.delimiter);
        for (let i = 0; i < pathparts.length; i++) {
            let binpath = path.join(pathparts[i], binname);
            if (FS.existsSync(binpath)) {
                return binpath;
            }
        }
    }

    // Else return the binary name directly (this will likely always fail downstream) 
    return binname;
}

function correctBinname(binname: string) {
    if (process.platform === 'win32')
        return binname + '.exe';
    else
        return binname;
}

