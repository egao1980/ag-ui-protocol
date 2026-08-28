# ag-ui-protocol

CLOS [AG-UI](https://docs.ag-ui.com/concepts/events) protocol for cl-stack (wave-1).

Typed events — **not JSON-RPC**. Official HTTP is `POST RunAgentInput` → SSE of
`RUN_*` / `TEXT_MESSAGE_*` / `TOOL_CALL_*` / `STATE_*` / `MESSAGES_SNAPSHOT`.

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

Models are [`schema-protocol`](https://github.com/egao1980/schema-protocol) (`defschema`, `:tag type`).
JSON Schema emit/validate (tool `parameters`, incoming events) is
[`schema-protocol-json`](https://github.com/egao1980/schema-protocol-json).

Bindings: [`ag-ui-backend-sse`](https://github.com/egao1980/ag-ui-backend-sse)
(default), [`ag-ui-backend-protobuf`](https://github.com/egao1980/ag-ui-backend-protobuf)
(`:format :protobuf` = JSON UTF-8 octets until the official Event proto is compiled).

Part of [cl-stack](https://github.com/egao1980/cl-stack) agent-wire
([brief](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/ag-ui.md)).
Tracks [#187](https://github.com/egao1980/cl-stack/issues/187).

CI: canned [`cl-repository`](https://github.com/egao1980/cl-repository)
(`test-system.yml`). Deps from `ghcr.io/egao1980/cl-systems`.

## License

MIT
