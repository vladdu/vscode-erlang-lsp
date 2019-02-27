import * as vscode from 'vscode'
import { LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions, TransportKind, StreamInfo, Trace } from 'vscode-languageclient'
import * as path from 'path'
import * as FS from 'fs'
import * as PortFinder from 'portfinder'
import * as Net from 'net'
import * as ChildProcess from 'child_process'
import * as erts from './erts'
import * as runtimes from './runtimes'
import { OtpLibsProvider } from './otpLibs';

const denodeify = require('denodeify')

const downloadUrl = vscode.Uri.parse('https://www.erlang-solutions.com/resources/download.html')

export function activate(context: vscode.ExtensionContext, isRestart: boolean = false) {
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

    context.subscriptions.push(vscode.commands.registerCommand('erlang.reloadExtension', (_) => {
        deactivate(true);
        for (const sub of context.subscriptions) {
            try {
                sub.dispose();
            } catch (e) {
                console.error(e);
            }
        }
        activate(context, true);
    }))
    // context.subscriptions.push(vscode.commands.registerCommand('erlang.detect.runtimes', (_) => {
    //     runtimes.detectRuntimes()
    // }))
    // context.subscriptions.push(vscode.commands.registerCommand('erlang.select.runtime', (_) => {
    //     runtimes.selectRuntime()
    // }))
    const rootPath = vscode.workspace.rootPath
    if (rootPath !== undefined) {
        const otpLibsProvider = new OtpLibsProvider(rootPath)
        vscode.window.registerTreeDataProvider('erlangOTPlibs', otpLibsProvider);
    }
}

export function deactivate(isRestart: boolean = false) {
    //
}

//////////////////////////////////////////////////////////////

async function createClient(extensionPath: string, channel: vscode.OutputChannel) {
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
    client.trace = Trace.Verbose;
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

async function startServer(myCwd: string, escriptPath: string, channel: vscode.OutputChannel): Promise<StreamInfo> {
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
        channel.appendLine('<ERR>:' + data.toString().trim())
    })
    const waitForSocket = denodeify(require('socket-retry-connect').waitForSocket)
    let socket = await waitForSocket({ port: port })
    return Promise.resolve({ reader: socket, writer: socket })
}

export async function showErlangRuntimeError(
    search: (path: string[]) => string,
    saveSdkPath: (path: string) => Thenable<void>,
    downloadUrl: string,
    commandToReRun: string = '',
) {
    const locateAction = 'Locate runtime';
    const downloadAction = 'Download runtime';
    let displayMessage = `Could not find an Erlang runtime. ` +
        `Please ensure 'erl' is installed and in your PATH (you may need to restart).`;
    while (true) {
        const selectedItem = await vscode.window.showErrorMessage(displayMessage,
            locateAction,
            downloadAction,
        );
        // TODO: Refactor/reformat/comment this code - it's messy and hard to understand!
        if (selectedItem === locateAction) {
            const selectedFolders =
                await vscode.window.showOpenDialog({ canSelectFolders: true, openLabel: `Set Erlang runtime folder` });
            if (selectedFolders && selectedFolders.length > 0) {
                const matchingSdkFolder = search(selectedFolders.map((f) => f.fsPath));
                if (matchingSdkFolder) {
                    await saveSdkPath(matchingSdkFolder);
                    await reloadExtension();
                    if (commandToReRun) {
                        vscode.commands.executeCommand(commandToReRun);
                    }
                    break;
                } else {
                    displayMessage = `That folder does not appear to be a Erlang runtime.`;
                }
            }
        } else if (selectedItem === downloadAction) {
            openInBrowser(downloadUrl);
            break;
        } else {
            break;
        }
    }
}

export function openInBrowser(url: string) {
    vscode.commands.executeCommand('vscode.open', vscode.Uri.parse(url));
}

export async function reloadExtension(prompt?: string, buttonText?: string) {
    const restartAction = buttonText || 'Restart';
    if (!prompt || await vscode.window.showInformationMessage(prompt, restartAction) === restartAction) {
        vscode.commands.executeCommand('erlang.reloadExtension');
    }
}
