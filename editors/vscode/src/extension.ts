import * as path from 'path';
import * as vscode from 'vscode';

import * as lc from 'vscode-languageclient';
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from 'vscode-languageclient/node';

export type SymGraphParams = {
	textDocument: lc.TextDocumentIdentifier;
};

export const symGraph = new lc.RequestType<SymGraphParams, null, void>(
    "doomls/symGraph",
);

let client: LanguageClient | null = null;

const channel = vscode.window.createOutputChannel("DoomLS Client")

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(ctx: vscode.ExtensionContext) {
	channel.appendLine("(DoomLS) Client initializing...");

	ctx.subscriptions.push(vscode.commands.registerCommand("doomls.startServer", () => {
		startServer(ctx);
	}));

	ctx.subscriptions.push(vscode.commands.registerCommand("doomls.stopServer", () => {
		stopServer();
	}));

	ctx.subscriptions.push(vscode.commands.registerTextEditorCommand("doomls.symGraph", (textEditor, _edit) => {
		const params = { textDocument: { uri: textEditor.document.uri.toString() } };
		client?.sendRequest(symGraph, params);
	}));

	const cfg = vscode.workspace.getConfiguration("doomls");

	if (!cfg.get("server.enable") || client !== null) {
		return;
	}

	startServer(ctx);
}

async function startServer(ctx: vscode.ExtensionContext) {
	vscode.window.showInformationMessage('(DoomLS) Server starting...');

	if (client !== null) {
		return;
	}

	const ext = process.platform === "win32" ? ".exe" : "";

	// @sync(init-options)
	const initOpts = {};

	const serverOpts: ServerOptions = {
		command: ctx.asAbsolutePath(path.join("out", `doom-ls${ext}`)),
		/*
		For full Rust backtraces:

		options: {
			env: {
				RUST_BACKTRACE: "full"
			}
		}
		*/
	};

	const clientOpts: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "zscript" }],
		initializationOptions: initOpts,
	};

	client = new LanguageClient(
		"doomls",
		"DoomLS",
		serverOpts,
		clientOpts,
	);

	await client.start();
}

export async function deactivate() {
	stopServer();
}

async function stopServer() {
	channel.appendLine("(DoomLS) Client shutting down...")

	if (client === null) {
		return;
	}

	await client.stop();
	client = null;
}
