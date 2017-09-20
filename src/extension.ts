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
    let erlExecutablePath = findErlangExecutable('escript');
    if (!erlExecutablePath) {
        vscode.window.showWarningMessage('Could not find Erlang v20 executable; please configure "erlang.runtime.location" or $PATH');
        return;
    }
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

    let erlideChannel = vscode.window.createOutputChannel('Erlang LSP')
    context.subscriptions.push(erlideChannel)
    erlideChannel.clear()

    function createServer(): Promise<StreamInfo> {
        return new Promise((resolve, reject) => {
            let myCwd = context.extensionPath

            PortFinder.getPort({ port: 9000 }, function (err, port) {
                let args = [
                    path.join(myCwd, 'erlang_ls'), '-p', port.toString()
                ];

                console.log(":: " + erlExecutablePath + ' ' + args.join(' '));
                let options = {
                    stdio: 'inherit',
                    env: { "HOME": process.env.HOME },
                    cwd: myCwd,
                    maxBuffer: 1024 * 1024
                };
                // Start the child process
                let erl = ChildProcess.execFile(erlExecutablePath, args, options, (error, stdout, stderr) => {
                    if (error) {
                        console.log("ERROR::: " + error)
                        throw error;
                    }
                });
                erl.stdout.on('data', (data) => {
                    erlideChannel.appendLine(data.toString().trim());
                });
                var waitForSocket = require('socket-retry-connect').waitForSocket;
                waitForSocket({ port: port }, function (err, socket) {
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
    context.subscriptions.push(aclient)
}

// this method is called when your extension is deactivated
export function deactivate() {
}

function findErlangExecutable(binname: string) {
    binname = correctBinname(binname);

    let conf = vscode.workspace.getConfiguration('erlang')['runtime.location'];
    if(conf) {
        let binpath = path.join(conf, 'bin', binname);
        if (FS.existsSync(binpath)) {
            return binpath
        }
    }
    // Then search PATH parts
    if (process.env['PATH']) {
        let pathparts = process.env['PATH'].split(path.delimiter);
        for (let i = 0; i < pathparts.length; i++) {
            let binpath = path.join(pathparts[i], binname);
            if (FS.existsSync(binpath)) {
                if(check_version('20', pathparts[i]))
                    return binpath
            }
        }
    }

    return null;
}

function check_version(vsn, fpath) {
    let fname = path.join(fpath, 'start.script')
    if(FS.existsSync(fname)) {
        let v = FS.readFileSync(fname, 'utf-8')
        let x = v.match("{\"Erlang/OTP\", *\""+vsn+"\"}")
        return x.length!=0
    }
    // what to do here?
    return true
}

function correctBinname(binname: string) {
    if (process.platform === 'win32')
        return binname + '.exe';
    else
        return binname;
}

