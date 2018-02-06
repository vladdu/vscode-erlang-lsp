'use strict'

import * as vscode from 'vscode'
import { LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions, TransportKind, StreamInfo } from 'vscode-languageclient'
import * as path from 'path'
import * as FS from 'fs'
import * as PortFinder from 'portfinder'
import * as Net from 'net'
import * as ChildProcess from 'child_process'
import * as erts from './erts'
const denodeify = require('denodeify')

const downloadUrl = vscode.Uri.parse('https://www.erlang-solutions.com/resources/download.html')

export function activate(context: vscode.ExtensionContext) {
    let channel = vscode.window.createOutputChannel('Erlang LS')
    context.subscriptions.push(channel)
    channel.clear()

    createClient(context.extensionPath, channel).then((client) => {
        context.subscriptions.push(client)
    }).catch(e => {
        vscode.window.showErrorMessage('Could not start Erlang server: ' + e.message,
            'Install Erlang').then((selection) => {
                if (selection === 'Install Erlang') {
                    console.log('OPEN ' + downloadUrl)
                    vscode.commands.executeCommand('vscode.open', downloadUrl)
                }
            })
        throw e;
    })
}

export function deactivate() {
    //
}

//////////////////////////////////////////////////////////////

async function createClient(extensionPath, channel: vscode.OutputChannel) {
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

    let escriptPath = await getEscriptPath()

    // Create the language client and start it.
    let client = new LanguageClient('Erlang Server',
        () => {
            return startServer(extensionPath, escriptPath, channel)
        },
        clientOptions)
    client.registerProposedFeatures();
    client.onReady().then(() => { console.log('Erlang server ready!') },
        (reason) => { throw Error('Erlang server error:  ' + reason) })
    return client.start()
}

async function getEscriptPath() {
    let info
    try {
        info = await erts.getRuntimeInfo()
        let { location: location, ertsVersion: version } = info
        if (!location) {
            throw Error('foobar')
        }
        let escriptPath = path.join(location, 'escript')
        return escriptPath
    } catch (error) {
        throw Error('Erlang v20+ executable not found: ' + error.message)
    }
}

async function startServer(myCwd, escriptPath, channel: vscode.OutputChannel): Promise<StreamInfo> {
    const getPort = denodeify(PortFinder.getPort)
    let port = await getPort({ port: 9000 })
    let args = [
        path.join(myCwd, 'erlang_ls'), '-p', port.toString()
    ]
    channel.appendLine('Launching:: ' + escriptPath + ' ' + args.join(' '))
    let options = {
        stdio: 'inherit',
        cwd: myCwd,
        //shell: true,
        maxBuffer: 2 * 1024 * 1024
    }
    // Start the child process
    let erl = ChildProcess.execFile(escriptPath, args, options)
    erl.on('exit', function (code, signal) {
        console.log('Erlang LS exited with ' +
            `code ${code} and signal ${signal}`)
    })
    erl.stdout.on('data', (data) => {
        channel.appendLine(data.toString().trim())
    })
    erl.stderr.on('data', (data) => {
        channel.appendLine('ERR:' + data.toString().trim())
    })
    const waitForSocket = denodeify(require('socket-retry-connect').waitForSocket)
    let socket = await waitForSocket({ port: port })
    return Promise.resolve({ reader: socket, writer: socket })
}
