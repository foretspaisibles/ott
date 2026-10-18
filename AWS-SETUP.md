# AWS Setup for `aws configure sso`

How to prepare a brand-new AWS account so that the AWS CLI can authenticate
through IAM Identity Center (SSO).

## 1. Secure the root user (console)

- Sign in as root and enable MFA.
- Do not use root for daily work.

## 2. Enable IAM Identity Center (console)

- Pick your **home region** first. Identity Center lives in one region and is
  hard to move.
- Open IAM Identity Center and click **Enable**.
- Choose **Enable with AWS Organizations**. This creates an organization with
  your account as the management account.

## 3. Create a user

- IAM Identity Center → Users → Add user. Fill in a username and email.
- The user receives an invitation email. They accept it, set a password and
  register MFA.

## 4. Create a permission set

- Permission sets → Create.
- Use the predefined `AdministratorAccess` to start, or `PowerUserAccess` for
  something tighter.
- Set the session duration, for example 8 hours.

## 5. Assign the user to the account

- AWS accounts → select your account → Assign users or groups.
- Choose the user, then the permission set.

## 6. Note the start URL

- It is on the Identity Center dashboard and looks like
  `https://d-xxxxxxxxxx.awsapps.com/start`.
- You can set a friendlier alias there.

## 7. Configure the CLI

Requires AWS CLI v2.

```sh
aws configure sso
```

Prompts:

| Prompt                       | Value                                              |
|------------------------------|----------------------------------------------------|
| SSO session name             | anything, e.g. `obi`                               |
| SSO start URL                | the URL from step 6                                |
| SSO region                   | the Identity Center home region from step 2        |
| SSO registration scopes      | accept the default, `sso:account:access`           |

A browser opens and you approve the login. The CLI then asks you to choose the
account and role, a default region and output format, and a profile name.

## 8. Log in and use it

```sh
aws sso login --profile <profile>
aws sts get-caller-identity --profile <profile>
export AWS_PROFILE=<profile>
```

Tokens expire. Re-run `aws sso login` when they do.
