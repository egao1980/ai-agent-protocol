(defpackage #:ai-agent-protocol/policy
  (:use #:cl)
  (:local-nicknames (#:agent #:ai-agent-protocol) (#:policy #:policy-protocol))
  (:export #:*tool-policy-context*))

(in-package #:ai-agent-protocol/policy)
