import * as path from 'path';
import * as vscode from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | null = null;

const channel = vscode.window.createOutputChannel("DoomLSP Client")

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(ctx: vscode.ExtensionContext) {
	channel.appendLine("(DoomLSP) Client initializing...");
	const cfg = vscode.workspace.getConfiguration("doomlsp");

	if (!cfg.get("server.enable") || client !== null) {
		return;
	}

	const ext = process.platform === "win32" ? ".exe" : "";

	// @sync(init-options)
	const initOpts = {};

	const serverOpts: ServerOptions = {
		command: ctx.asAbsolutePath(path.join("out", `doom-lsp${ext}`))
	};

	const clientOpts: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "zscript" }],
		initializationOptions: initOpts,
	};

	client = new LanguageClient("doomlsp", serverOpts, clientOpts);
}

export async function deactivate() {
	channel.appendLine("(DoomLSP) Client shutting down...")

	if (client === null) {
		return;
	}

	await client.stop();
	client = null;
}

function strNone(s: string | undefined): string | null {
	return s === undefined || s === "none" ? null : s;
}
