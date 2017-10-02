'use strict'
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
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
    client.onReady().then(() => { console.log('Erlang server ready!') },
        (reason) => { throw Error('Erlang server error:  ' + reason) })
    return client.start()
}

async function getEscriptPath() {
    let info
    try {
        info = await erts.getRuntimeInfo()
        let { location: location, ertsVersion: version } = info
        if(!location){
            th
        }
        let escriptPath = path.join(location, 'bin', 'escript')
        return escriptPath
    } catch (error) {
        throw Error('Could\'nt find Erlang v20+ executable, please configure "erlang.runtime.location" or $PATH: ' + error.message)
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

function findErlangExecutable(binname: string) {
    binname = correctBinname(binname)

    let v = vscode.workspace.getConfiguration('erlang')
    let conf = v.get('runtime.location', '')
    if (conf) {
        let binpath = path.join(conf, 'bin', binname)
        if (FS.existsSync(binpath)) {
            if (check_version('20', conf)) {
                return binpath
            }
        }
    }
    // Then search PATH parts
    if (process.env['PATH']) {
        let pathparts = process.env['PATH'].split(path.delimiter)
        for (let i = 0; i < pathparts.length; i++) {
            let binpath = path.join(pathparts[i], binname)
            if (FS.existsSync(binpath)) {
                if (check_version('20', path.join(pathparts[i]))) {
                    return binpath
                }
            }
        }
    }

    return null
}

function check_version(vsn, fpath) {
    return get_version(fpath) === vsn
}

function get_version(home) {
    let fname = path.join(home, 'start.script')
    if (FS.existsSync(fname)) {
        let v = FS.readFileSync(fname, 'utf-8')
        let x = v.match('{"Erlang/OTP", *"([^"]+)"}')
        if (x.length === 2) {
            return x[1]
        }
    }
    return null
}

function correctBinname(binname: string) {
    if (process.platform === 'win32') {
        return binname + '.exe'
    } else {
        return binname
    }
}

