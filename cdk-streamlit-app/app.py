#!/usr/bin/env python3
import os

import aws_cdk as cdk

from cdk.cdk_stack import CdkStack


app = cdk.App()
env = cdk.Environment(account=app.node.try_get_context('account'),
                      region=app.node.try_get_context('region'))
CdkStack(app, "streamlitappstack",env=env)

app.synth()
