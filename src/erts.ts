'use strict'

import { window, workspace, Uri } from 'vscode'
import * as child_process from 'child_process'
import * as fs from 'fs'
import * as path from 'path'
import * as which from 'which'


const expandHomeDir = require('expand-home-dir')
const denodeify = require('denodeify')
const execFile = require('child-process-promise').execFile;

const isWindows = process.platform.indexOf('win') === 0
const ERL_FILENAME = 'erl' + (isWindows ? '.exe' : '')
const ESCRIPT_FILENAME = 'escript' + (isWindows ? '.exe' : '')

export interface RuntimeInfo {
    location: string
    ertsVersion: number
}

export async function getRuntimeInfo(): Promise<RuntimeInfo> {
    let location = await checkErlangLocation()
    if (location !== undefined) {
        let version = await checkErlangVersion(location)
        return { 'location': location, 'ertsVersion': version }
    }
    return { 'location': undefined, 'ertsVersion': undefined }
}

async function checkErlangLocation(): Promise<string> {
    let location: string = readErlangConfigLocation()
    if (location) {
        location = expandHomeDir(location)
        if (fs.existsSync(path.join(location, ESCRIPT_FILENAME))) {
            return location
        } else {
            if (fs.existsSync(path.join(location, 'bin', ESCRIPT_FILENAME))) {
                return path.join(location, 'bin')
            } else {
                window.showInformationMessage('Please review setting erlang.runtime.location: escript binary not found; trying $PATH')
            }
        }
    }
    return findErlangHome()
}

function readErlangConfigLocation(): string {
    const config = workspace.getConfiguration()
    return config.get<string>('erlang.runtime.location', null)
}

async function checkErlangVersion(erl_home: string): Promise<number> {
    let cp = await execFile(path.join(erl_home, 'erl'), ['-version'], { capture: ['stdout', 'stderr'] })
    let ver = cp.stderr.match('version ([0-9]+).')
    if (ver.length > 1) {
        let vv = parseInt(ver[1])
        if (vv >= 9) {
            return vv
        } else {
            throw Error('Erlang v' + (vv + 11) + ' found (need 20+).')
        }
    }
}

function findErlangHome(): string {
    return path.dirname(which.sync('escript', { nothrow: true }))
}

function getOtpVersion(home) {
    let fname = path.join(home, 'start.script')
    if (fs.existsSync(fname)) {
        let v = fs.readFileSync(fname, 'utf-8')
        let x = v.match('{"Erlang/OTP", *"([^"]+)"}')
        if (x.length === 2) {
            return x[1]
        }
    }
    return null
}
