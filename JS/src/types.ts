
export interface State {
    grammar: string;
    string: string;
    selectedParser: string;
    run: boolean;
    save: boolean;
}

export interface Output {
    grammarParseError: string;
    parsers: string[];
    parserOutput: string;
    warnings: string[];
    saveMessage: string;
}