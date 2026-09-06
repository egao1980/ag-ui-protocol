# Vendored `@ag-ui/proto`

Official Event oneof schema from
[`ag-ui-protocol/ag-ui`](https://github.com/ag-ui-protocol/ag-ui)
`sdks/typescript/packages/proto/src/proto/` (main, matches npm `@ag-ui/proto`
0.0.59: 21 oneof members; no REASONING_*, ACTIVITY_*, THINKING_*,
TOOL_CALL_RESULT).

| File | Role |
|------|------|
| `events.proto` | `Event` oneof + per-event messages |
| `types.proto` | `Message`, `Interrupt`, multimodal parts |
| `patch.proto` | `JsonPatchOperation` |
| `*.lisp` | cl-protobufs protoc plugin output (`protoc-gen-cl-pb`) |

Regenerate:

```bash
protoc --proto_path=proto --proto_path=/path/to/cl-protobufs \
  --plugin=protoc-gen-cl-pb=protoc-gen-cl-pb \
  --cl-pb_out=output-file=FILE.lisp:proto \
  --experimental_allow_proto3_optional \
  proto/FILE.proto
```

Do not hand-edit the `.lisp` files.
