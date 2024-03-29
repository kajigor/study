"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = void 0;
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require("vscode");
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
function activate(context) {
    // Use the console to output diagnostic information (console.log) and errors (console.error)
    // This line of code will only be executed once when your extension is activated
    console.log('Congratulations, your extension "helloworld-sample" is now active!');
    // The command has been defined in the package.json file
    // Now provide the implementation of the command with registerCommand
    // The commandId parameter must match the command field in package.json
    const hello = vscode.commands.registerCommand('extension.helloWorld', () => {
        // The code you place here will be executed every time your command is executed
        // Display a message box to the user
        vscode.window.setStatusBarMessage('Some kind of message');
        vscode.window.showInformationMessage('Hello VS code!');
    });
    const warning = vscode.commands.registerCommand('extension.warning', () => {
        // The code you place here will be executed every time your command is executed
        // Display a message box to the user
        vscode.window.showWarningMessage('Hello VS code!');
    });
    const statusBar = vscode.commands.registerCommand('extension.status', () => {
        // The code you place here will be executed every time your command is executed
        // Display a message box to the user
        vscode.window.setStatusBarMessage('This is a status bar message!');
    });
    context.subscriptions.push(hello, warning, statusBar);
}
exports.activate = activate;
//# sourceMappingURL=extension.js.map