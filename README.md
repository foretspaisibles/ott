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

## Setup the repository secrets

The workflow `.github/workflows/describe-aws-setup.yml` reads three
repository secrets: `AWS_ACCOUNT_ID`, `AWS_REGION` and `AWS_ROLE_ARN`.
Run `OTT/DEVELOPMENT:SETUP-AWS` with the AWS CLI authenticated on the
target account and the environment variables `AWS_ACCOUNT_ID`,
`GITHUB_ORG` and `GITHUB_REPO` set. The function returns the ARN of
the role GitHub Actions assumes. Then run
`OTT/DEVELOPMENT:SETUP-GITHUB-ACTION` with the GitHub CLI
authenticated on the repository: it reads the output of
`OTT/DEVELOPMENT:DESCRIBE-AWS-SETUP` and stores the three secrets
with `gh secret set`. It returns the names of the secrets, and
signals an error without setting anything when the AWS setup does
not exist. You can also set the secrets in the repository settings
under *Secrets and variables*, *Actions*.

The setup expires after one week: extend it with
`OTT/DEVELOPMENT:EXTEND-AWS-LEASE` and reclaim it with
`OTT/DEVELOPMENT:TEAR-DOWN-AWS`.

# Running the demos

The scripts in `demo/` use SBCL and Quicklisp (`~/quicklisp/setup.lisp`
or `~/Library/QuickLisp/setup.lisp`, or the file named by the
environment variable `QUICKLISP_SETUP`).

## Monolith

```bash
demo/run_monolith
```

Loads the system `ott/business-logic` with every micro-service in the
same Lisp image, in the `:MONOLITH` calling convention, checks out the
five demo orders and returns the happy order. Services are plain
function calls and nothing touches AWS.

## Mesh

The mesh needs the infrastructure of the queue `ott-continuation` and
the AWS CLI authenticated on its account. Start each micro-service in
its own terminal:

```bash
demo/start_payment
demo/start_warehouse
demo/start_shipping
demo/start_business-logic
```

Each service prints its name, PID and calling convention (`:MESH`),
checks before anything else that AWS authentication works and that
it can reach the queue (it exits with status 1 otherwise), then handles
the events it owns, tracing each one. Stop it with Control-C: it shuts
down cleanly. Then, in a fifth terminal:

```bash
demo/run_mesh              # the happy order
demo/run_mesh no-credit    # or out-of-stock, bad-address, absent-recipient
```

It sends the checkout of a demo order on the queue and returns at once
with the run identifier. The events travel between the terminals of the
services, and the result of the run shows as a `COMPLETE` line in the
terminal of `demo/start_business-logic`.
