# ag-ui-protocol

CLOS [AG-UI](https://docs.ag-ui.com/concepts/events) protocol for cl-stack.

Typed events — **not JSON-RPC**. Official HTTP is `POST RunAgentInput` → SSE of
`RUN_*` / `TEXT_MESSAGE_*` / `TOOL_CALL_*` / `REASONING_*` / `STATE_*` /
`ACTIVITY_*` / `SUBAGENT_*` / `MESSAGES_SNAPSHOT`. All 36 upstream event types
decode; `every-upstream-event-type-is-modelled` in the test suite pins that.

`*_CHUNK` events are a producer convenience — `expand-ag-ui-chunks` (or a
`chunk-expander` for incremental use) rewrites them into the explicit
START/CONTENT/END triads so reducers only ever see one shape.

```lisp
(asdf:load-system "ag-ui-protocol")

(let ((agent (ag-ui-protocol:make-ag-ui-agent)))
  (ag-ui-protocol:run-agent
   agent (ag-ui-protocol:make-run-agent-input
          :thread-id "t" :run-id "r"
          :messages (list (ag-ui-protocol:make-ag-ui-message
                           :role "user" :content "hi")))))
```

Incremental handlers call `ag-ui-emit` while `*ag-ui-emit*` is bound by `run-agent`.
If the handler never emits, a returned event list is `mapc`'d onto `:on-event` (`echo-handler`).
`make-ag-ui-app` writes each event to the SSE stream as it is emitted (`force-output`).

Clack app (no HTTP server required to test):

```lisp
(ag-ui-protocol:make-ag-ui-app agent :path "/")
;; POST /  Content-Type: application/json  →  text/event-stream
```

`decode-ag-ui-event` tolerates event types this build does not model — they
decode to `unknown-ag-ui-event`, which keeps the source table and re-encodes
verbatim, so one new event from a newer producer cannot abort a run. Pass
`:strict t` to signal instead. `validate-ag-ui-json` stays strict.

Models are [`schema-protocol`](https://github.com/egao1980/schema-protocol) (`defschema`, `:tag type`).
`ag-ui-event` lists its variants explicitly; a new wire event is registered there.
JSON Schema emit/validate (tool `parameters`, incoming events) is
[`schema-protocol-json`](https://github.com/egao1980/schema-protocol-json).

Bindings: [`ag-ui-backend-sse`](https://github.com/egao1980/ag-ui-backend-sse)
(default), [`ag-ui-backend-protobuf`](https://github.com/egao1980/ag-ui-backend-protobuf).

`:format :protobuf` is JSON-as-WKT (`google.protobuf.Value` via serdes `:wkt`),
length-prefixed under `application/vnd.ag-ui.event+proto`. That is **not** the
official `Event` oneof — unknown event types survive as tables. `make-ag-ui-app`
negotiates `Accept`: proto media type → binary; otherwise SSE. GET on the
agent path returns `AgentCapabilities`.

Part of [cl-stack](https://github.com/egao1980/cl-stack) agent-wire
([brief](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/ag-ui.md)).
Tracks [#187](https://github.com/egao1980/cl-stack/issues/187).

CI: canned [`cl-repository`](https://github.com/egao1980/cl-repository)
(`test-system.yml`). Deps from `ghcr.io/egao1980/cl-systems`.

## License

MIT
