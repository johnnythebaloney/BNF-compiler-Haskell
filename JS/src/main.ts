import {
    combineLatest,
    debounceTime,
    fromEvent,
    map,
    merge,
    mergeWith,
    pairwise,
    scan,
    share,
    startWith,
    Subject,
    switchMap,
    type Observable,
} from "rxjs";
import { fromFetch } from "rxjs/fetch";
import type { State, Output } from "./types";

const grammarInput = document.getElementById(
    "grammar-input",
) as HTMLTextAreaElement;
const stringInput = document.getElementById(
    "string-input",
) as HTMLTextAreaElement;
const dropDown = document.getElementById("parserSelect") as HTMLSelectElement;
const button = document.getElementById("runParser")!;
const saveButton = document.getElementById("parseSvr")!;

type Action<T> = (_: T) => T;

const resetState: Action<State> = s => ({
    ...s,
    run: false,
});

// Create an Observable for keyboard input events
const input$: Observable<Action<State>> = fromEvent<KeyboardEvent>(
    grammarInput,
    "input",
).pipe(
    debounceTime(1000),
    map(event => (event.target as HTMLInputElement).value),
    map(value => s => resetState({ ...s, grammar: value })),
);

const stringToParse$: Observable<Action<State>> = fromEvent<KeyboardEvent>(
    stringInput,
    "input",
).pipe(
    debounceTime(1000),
    map(event => (event.target as HTMLInputElement).value),
    map(value => s => resetState({ ...s, string: value })),
);

const selectedParser$ = new Subject<string>();
const dropDownStream$: Observable<Action<State>> = merge(
    fromEvent(dropDown, "change").pipe(
        map(event => (event.target as HTMLSelectElement).value),
        mergeWith(selectedParser$),
        map(
            (value): Action<State> =>
                s =>
                    resetState({ ...s, selectedParser: value }),
        ),
    ),
);

const buttonStream$: Observable<Action<State>> = fromEvent(
    button,
    "click",
).pipe(map(() => s => ({ ...resetState(s), run: true })));

// Save button stream
const saveButtonStream$: Observable<Action<State>> = fromEvent(
    saveButton,
    "click",
).pipe(map(() => s => ({ ...s, save: true })));

// State and Output are split up and this only returns an Output to avoid race
// conditions with this function overriding new values of the inputs (e.g.
// grammar, string) in the state]
//Part D : Handle save requests and normal parse requests
function getOutput(s: State): Observable<Action<Output>> {
    // Handle save request
    // if save, we first save the grammer to the server 
    if (s.save) {
        const body = new URLSearchParams();
        //where we set the body to the grammar
        body.set("grammar", s.grammar);
        
        //Then we make a fetch req to the server to get the string and also the filename to save to
        return fromFetch<
            Readonly<
                | { success: string; filename: string }
                | { error: string }
            >
        //Call the save api
        >("/api/save", {
            //using the method post to save the grammar
            method: "POST",
            //Where the header is set to form url encoded
            headers: {
                "Content-Type": "application/x-www-form-urlencoded",
            },
            //Change the type of body to string
            body: body.toString(),
            //Get the json response
            selector: res => res.json(),
        }).pipe(
            //Map the response to either an error or success message
            map(res => {
                if ("error" in res) {
                    return o => ({ ...o, saveMessage: "Error: " + res.error, save: false });
                }
                return o => ({
                    ...o,
                    saveMessage: "Saved to " + res.filename,
                    save: false
                });
            }),
        );
    }

    // Handle normal parse request
    const body = new URLSearchParams();
    body.set("grammar", s.grammar);
    body.set("string", s.run ? s.string : "");
    body.set("selectedParser", s.run ? s.selectedParser : "");

    return fromFetch<
        Readonly<
            | { parsers: string; result: string; warnings: string }
            | { error: string }
        >
    >("/api/generate", {
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: body.toString(),
        selector: res => res.json(),
    }).pipe(
        map(res => {
            if ("error" in res)
                return o => ({ ...o, grammarParseError: res.error });
            const parsers = res.parsers.split(",");
            return o => ({
                ...o,
                grammarParseError: "",
                warnings: res.warnings.split("\n"),
                parserOutput: res.result,
                parsers,
                selectedParser: parsers.includes(s.selectedParser)
                    ? s.selectedParser
                    : (parsers[0] ?? ""),
            });
        }),
    );
}

const initialState: State = {
    grammar: "",
    string: "",
    selectedParser: "",
    run: false,
    save: false,
};

const initialOutput: Output = {
    grammarParseError: "",
    parsers: [],
    parserOutput: "",
    warnings: [],
    saveMessage: "",
};

function main() {
    const selectElement = document.getElementById("parserSelect")!;
    const grammarParseErrorOutput = document.getElementById(
        "grammar-parse-error-output",
    ) as HTMLOutputElement;
    const parserOutput = document.getElementById(
        "parser-output",
    ) as HTMLOutputElement;
    const validateOutput = document.getElementById(
        "validate-output",
    ) as HTMLOutputElement;
    const saveOutput = document.getElementById(
        "save-output",
    ) as HTMLOutputElement;

    // Subscribe to the input Observable to listen for changes
    const state$ = merge(
        input$,
        dropDownStream$,
        stringToParse$,
        buttonStream$,
        saveButtonStream$,
    ).pipe(
        scan((state, action) => action(state), initialState),
        share(),
    );
    const output$ = state$.pipe(
        switchMap(getOutput),
        scan((output, action) => action(output), initialOutput),
    );
    combineLatest([state$, output$])
        .pipe(
            map(([state, output]) => ({ ...state, ...output })),
            startWith({ ...initialState, ...initialOutput }),
            pairwise(),
            map(([s1, s2]) => ({
                ...s2,
                resetParsers:
                    s2.parsers.length !== s1.parsers.length ||
                    s2.parsers.some((x, i) => x !== s1.parsers[i]),
            })),
        )
        .subscribe(state => {
            if (state.resetParsers) {
                selectElement.replaceChildren(
                    ...state.parsers.map(optionText => {
                        const option = document.createElement("option");
                        option.value = optionText;
                        option.text = optionText;
                        return option;
                    }),
                );
                // if the <option> HTML elements are changed, the value of the
                // <select> element will reset to ""
                dropDown.value = state.selectedParser;
                selectedParser$.next(state.selectedParser);
            }

            grammarParseErrorOutput.value = state.grammarParseError;
            parserOutput.value = state.parserOutput;
            validateOutput.value = state.warnings.join("\n");
            
            // Update save output if there's a message
            if (saveOutput && state.saveMessage) {
                saveOutput.value = state.saveMessage;
            }
        });
}
if (typeof window !== "undefined") {
    window.addEventListener("load", main);
}