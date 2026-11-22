---
system: ai-agent-protocol
test-system: ai-agent-protocol/tests
brief: ../cl-stack/docs/capabilities/ai-agent.md
---

# ai-agent-protocol 0.15.0 / ai-agent-protocol/mcp 0.2.0 (egao1980/ai-agent-protocol#37)

Order: mcp-protocol 0.4.0 (egao1980/mcp-protocol#14, its own contract branch) is implemented,
merged and published first; this contract's tests load against it.

1. Tool-source combinators, `src/toolsets.lisp` (rules in its header): wrapper, combined
   (with `tool-name-conflict` / `continue`), prefixed, renamed, filtered, prepared,
   approval-required, and `tool-predicate-p` for functions.
2. Rich tool output: `agent-tool-output` through the loop (`agent-invocation-parts`,
   `agent-invocation-structured`, the extra :user turn after the tool results).
3. `ai-agent-protocol/policy` (`src/policy/policy.lisp`): policies as predicates.
4. `ai-agent-protocol/mcp` 0.2.0, `src/mcp/source.lisp`: the cached MCP tool source,
   result mapping, `tool-error-behavior`, `process-tool-call`, timeouts. The old
   `mcp-peer-as-tool-source` test must pass again.

Remove `%toolsets-pending`, `%source-pending`, `%policy-pending` and every call. Given code (classes,
constructors, conditions, slots, exports) stays. Deferred tool loading (a `tool_search` source) is a
later slice (0.16.0), not this one. Prior art: pydantic-ai toolsets and MCPServer.
