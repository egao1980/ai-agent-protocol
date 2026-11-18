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

**Core has zero MCP/A2A/AG-UI/task-protocol/telemetry-protocol deps.** Optional systems: `ai-agent-protocol/mcp`, `/ag-ui`, `/a2a`, `/durability`, `/telemetry`.

`on-event` kinds (keywords + llm/agent objects): `:started` `:step` `:part` `:response` `:invocation` `:handoff` `:finished`. `%do-generate` uses `stream-generate` (`:on-part` hops onto the event loop). `run-ai-agent-async` also takes `:on-part`. `ai-agent-protocol/ag-ui` is the streaming encoder to AG-UI events (`*ag-ui-emit*`).

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

Optional [`conversation-protocol`](https://github.com/egao1980/conversation-protocol) `:memory` — `prepare-agent-turns` recalls, terminal finish remembers (`:replace t`). Session is `:session` on the agent or `run-ai-agent`. Approval / deferred pauses do not persist.

Optional [`steer-protocol`](https://github.com/egao1980/steer-protocol) `:steering` — rules / `SKILL.md` skills (not A2A `agent-skill`). Applied after recall; merged into the system turn. `make-skill-tool-source` is a thin tool source that lists `skill-tools` and dispatches `skill-tool-fn` (same shape as `make-mcp-tool-source`).

Optional `task-protocol` `:durability` (`ai-agent-protocol/durability`) — each `generate` and tool invocation is a `with-durable-step` (idempotency key per step). HITL approve/deny journals `wait-input`; `run-ai-agent` with the same journal replays completed steps and continues. Crashes resume mid-run.

`tests/durability-test.lisp` runs in default `test-op` / `:ci :with` (`task-protocol` is on GHCR). `(asdf:test-system "ai-agent-protocol")` includes kill-and-replay coverage. The `ai-agent-protocol/durability-tests` system is a thin alias of the same suite.

```lisp
(asdf:load-system "ai-agent-protocol/durability")
(let ((dur (ai-agent-protocol/durability:make-agent-durability
            :journal (stack-task:make-in-memory-journal)
            :task-id "run-1")))
  (run-ai-agent agent "go" :durability dur))
```

```lisp
(make-ai-agent :name "echo" :backend backend
               :memory (stack-conversation:make-window-memory :window-size 8)
               :session "chat-1"
               :steering (list (stack-steer:make-steer-rule "cite"
                                                           :body "Always cite.")))
```

### Long runs: retries, compaction, max steps, tool failures

Settings `:extra` keys (plist) steer the loop; all are optional.

- **Retry by failure class.** A generate failure no handler recovered is retried when `:retry-policy` (default `llm-protocol:*retry-policy*`) covers its `llm-retry-class` — rate limit, overload, transient 5xx — after the policy's delay, scheduled on the event loop (`event:sleep*`), not by blocking it. Bad requests and auth failures fail at once. A partial streamed turn is discarded before the retry. Events: `:retry` with `(:reason class :attempt n :delay s :condition c)`. Handlers on `agent-generate-error` still win (`retry` / `use-value` / `abort`).
- **Agent-side compaction.** `:compaction` is a conversation memory (e.g. `stack-conversation:make-summary-memory :backend b`). Before a step whose prompt exceeds `:compact-at` tokens — or the model's context window less the completion reserve — `conversation:compact-turns` replaces the history with a templated summary plus the newest turns verbatim. The count is the last response's measured `llm-usage` plus an estimate for the turns since (`anchored-token-count`). On `llm-context-overflow` the run compacts once and retries the step. Event: `:compacted`.
- **API-side compaction** is the backend's job: `llm-protocol-anthropic` `:server-compaction` lets the API summarize in place. The two combine; the agent's threshold then only matters for a backend without it.
- **Max steps wrap-up.** At `max-steps` the run makes one more generate with `tool-choice :none` (tools stay in the request, so the cached prefix holds) after a user turn `+max-steps-prompt+` (override: `:max-steps-prompt`), so it ends with a summary of done / left / next instead of a dangling tool loop. Finish reason stays `:max-steps`. `:max-steps-wrap-up nil` turns it off.
- **Typed tool failures.** A tool handler signals `tool-input-error`, `tool-unavailable` or `tool-timeout` (`tool-failure` subtypes) and the model gets a result that says what to do next ("Fix the arguments…", "Do not call it again…"). `tool-failure-kind` / `tool-failure-text` are generic: specialize them for a source's own errors. An unknown tool name gets the list of tools it may call.

```lisp
(make-ai-agent :name "coder" :backend backend
               :settings (make-agent-settings
                          :max-steps 40
                          :extra (list :compaction (stack-conversation:make-summary-memory
                                                    :backend backend :tail-tokens 15000)
                                       :retry-policy (stack-llm:make-retry-policy :max-attempts 6))))
```

MCP sampling (`create-message` → `generate`) is `ai-agent-protocol/mcp:make-mcp-sampling-handler` — **not** in `llm-protocol`.

Live LM Studio cycle (instructions + user prompt + CL `add` tool):

```bash
set -a && source ../.env && set +a
CL_SOURCE_REGISTRY="$PWD/../:" ros -l scripts/demo.lisp
```

Part of [cl-stack](https://github.com/egao1980/cl-stack).

## License

MIT — see [LICENSE](LICENSE).
