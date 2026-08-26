# ai-agent-protocol

Async-first CLOS **agent loop** over [`llm-protocol`](https://github.com/egao1980/llm-protocol). Not blackboard core. Not AG-UI `run-agent` — that's `ag-ui-protocol:run-agent`. Use `run-ai-agent` / `run-ai-agent-async`.

```
AG-UI  ── expose ──┐
A2A    ── expose ──┼──► ai-agent-protocol (async-first)
MCP    ── call  ───┘
                    ├── function-tool (CL handler)
                    ├── nested ai-agent as tool (manager keeps control)
                    ├── handoff (specialist takes the run)
                    └── llm-protocol ──► models
```

**Core has zero MCP/A2A/AG-UI deps.** Optional systems: `ai-agent-protocol/mcp`, `/ag-ui`, `/a2a`.

Primitive = callback + cancel token (http `send-async` shape). No Blackbird in the protocol. Sync `run-ai-agent` awaits on the bound `event-protocol` loop. Sync `generate` / tool handlers run **off-loop** so they must not nest `event:run`.

```lisp
(asdf:load-system "ai-agent-protocol")
(asdf:load-system "event-backend-libuv")

(let* ((eb (event-backend-libuv:make-libuv-backend))
       (el (event-protocol:make-event-loop eb))
       (agent (stack-ai-agent:make-ai-agent
               :name "echo"
               :backend (stack-llm:make-mock-llm-backend)
               :instructions "Be brief.")))
  (event-protocol:with-event-backend (eb)
    (event-protocol:with-event-loop-var (el)
      (stack-ai-agent:agent-run-text
       (stack-ai-agent:run-ai-agent agent "hi")))))
;; ⇒ "echo: hi"
```

CL tools: `function-tool` / `define-agent-tool`. Nested agent in `:tools` = one peer tool. `:handoffs` = specialist replaces `agent-run-agent` and continues the same turns. `defagent` → `defclass` + `:default-initargs`.

MCP sampling (`create-message` → `generate`) is `ai-agent-protocol/mcp:make-mcp-sampling-handler` — **not** in `llm-protocol`.

Part of [cl-stack](https://github.com/egao1980/cl-stack).

## License

MIT — see [LICENSE](LICENSE).
