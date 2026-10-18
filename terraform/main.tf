# main.tf — Infrastructure of the OTT continuation queue

# OTT (https://github.com/foretspaisibles/ott)
# This file is part of OTT.
#
# Copyright © 2026 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# The state lives in the S3 bucket created by OTT/DEVELOPMENT:SETUP-AWS.
# The bucket, key and region are given on the command line by
# OTT/OPERATION:CREATE-INFRASTRUCTURE with `-backend-config'.

terraform {
  required_version = ">= 1.10"

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 6.0"
    }
  }

  backend "s3" {
    encrypt      = true
    use_lockfile = true
  }
}

variable "region" {
  type        = string
  description = "The AWS region hosting the queue."
}

variable "github_organization" {
  type    = string
  default = ""
}

variable "github_repository" {
  type    = string
  default = ""
}

variable "expiry_date" {
  type        = string
  description = "The ExpiryDate label of the resources, ISO 8601 in UTC."
}

provider "aws" {
  region = var.region

  default_tags {
    tags = {
      GitHubOrganization = var.github_organization
      GitHubRepository   = var.github_repository
      ExpiryDate         = var.expiry_date
    }
  }
}

data "aws_caller_identity" "current" {}

resource "aws_sqs_queue" "dlq" {
  name = "ott-continuation-dlq"
}

resource "aws_sqs_queue" "continuation" {
  name                       = "ott-continuation"
  receive_wait_time_seconds  = 20 # long polling by default
  visibility_timeout_seconds = 60 # > worst-case handler time
  redrive_policy = jsonencode({
    deadLetterTargetArn = aws_sqs_queue.dlq.arn
    maxReceiveCount     = 5
  })
}

data "aws_iam_policy_document" "queue" {
  statement {
    actions = [
      "sqs:SendMessage",
      "sqs:ReceiveMessage",
      "sqs:DeleteMessage",
      "sqs:ChangeMessageVisibility",
      "sqs:GetQueueUrl",
      "sqs:GetQueueAttributes",
    ]
    resources = [aws_sqs_queue.continuation.arn]
  }
}

# Principals of the account may assume the role, provided their own
# identity policies allow sts:AssumeRole.
data "aws_iam_policy_document" "trust" {
  statement {
    actions = ["sts:AssumeRole"]
    principals {
      type        = "AWS"
      identifiers = ["arn:aws:iam::${data.aws_caller_identity.current.account_id}:root"]
    }
  }
}

resource "aws_iam_role" "continued_function" {
  name               = "OTTContinuedFunction"
  assume_role_policy = data.aws_iam_policy_document.trust.json
}

# An inline policy rather than a managed policy attachment: the role of
# GitHub Actions may not attach customer managed policies.
resource "aws_iam_role_policy" "queue" {
  name   = "OTTContinuedFunction"
  role   = aws_iam_role.continued_function.id
  policy = data.aws_iam_policy_document.queue.json
}

output "queue_url" { value = aws_sqs_queue.continuation.url }
output "dlq_url" { value = aws_sqs_queue.dlq.url }
output "role_arn" { value = aws_iam_role.continued_function.arn }
