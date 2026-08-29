(defpackage #:ag-ui-protocol/capability
  (:use #:cl)
  (:local-nicknames (#:ag-ui #:ag-ui-protocol)
                    (#:cap #:capability-protocol))
  (:documentation
   "Bridge between capability-protocol catalogues and AG-UI AgentCapabilities.

    The two model different things. capability-protocol is about invocation:
    a capability is registered or it is not, and if it is, its operations can be
    called. AgentCapabilities is a declarative document a UI reads to decide
    which controls to show, carrying tri-state flags and scalars that a registry
    of marker instances cannot express.

    So this is a lossy adapter in both directions, not an alternative
    representation, and it lives in its own system so a wire-protocol consumer
    does not pull in blackboard-protocol. Same shape as llm-protocol/capability.")
  (:export #:capabilities-from-catalogue
           #:register-agent-capabilities
           #:+llm-capability-map+))

(in-package #:ag-ui-protocol/capability)
