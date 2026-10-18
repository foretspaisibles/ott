# Centralised business logic in a decentralized architecture

In a monolithic or centralized, architecture a piece of business logic
can be owned by a specific component and present an artefact that
software engineers and product owners can easily reason about to
explain or understand the behavior of the software. In a microservice
or decentralized architecture, such a code artefact vanishes as the
business logic is diluted in the service mesh. Traces can recreate a
tangible artefact displaying the logic result on a specific run but
they do not carry the same explainability weight than an actual
procedure capturing the business logic, which makes it very difficult
to reason about modifications.

In the talk I illustrate the problem with a lab example from retail,
explains why monads show up naturally in a decentralized setup but
have poor engineering properties and how to extend the compiler to
have best of both worlds (example in Common Lisp).

# Technical onboarding

## Setup Common Lisp MCP

```slime
; SLIME 
CL-USER> (asdf:load-system :cl-mcp)
T
CL-USER> (cl-mcp:start-http-server :port 3000 :token :generate)
MCP Auth Token: TOKEN
```

```bash
claude mcp add --transport http cl-mcp http://127.0.0.1:3000/mcp --header 'Authorization: Bearer TOKEN'
```
