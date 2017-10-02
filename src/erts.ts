'use strict'

import { workspace, Uri } from 'vscode'
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
    let version = await checkErlangVersion(location)
    return { 'location': location, 'ertsVersion': version }
}

async function checkErlangLocation(): Promise<string> {
    let location: string = readErlangConfigLocation()
    if (location) {
        let msg = 'The vscode setting "erlang.runtime.location"'
        location = expandHomeDir(location)
        if (!fs.existsSync(path.resolve(location, 'bin', ESCRIPT_FILENAME))) {
            throw Error(msg + ' does not point to an Erlang installation.')
        }
        return location
    }
    //No settings, let's try to detect as last resort.
    return findErlangHome()
}

function readErlangConfigLocation(): string {
    const config = workspace.getConfiguration()
    return config.get<string>('erlang.runtime.location', null)
}

async function checkErlangVersion(erl_home: string): Promise<number> {
    let cp = await execFile(erl_home + '/bin/erl', ['-version'], { capture: ['stdout', 'stderr'] })
    let ver = cp.stderr.match('version ([0-9]+).')
    if (ver.length > 1) {
        let vv = parseInt(ver[1])
        if (vv === 9) {
            return vv
        } else {
            throw Error('Erlang v' + (vv + 11) + ' found.')
        }
    }
}

function findErlangHome(): string {
    return which('escript', {nothrow: true})
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
