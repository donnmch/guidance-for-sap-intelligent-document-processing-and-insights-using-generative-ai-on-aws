import aws_cdk as core
import aws_cdk.assertions as assertions

from cdk_streamlit_app.cdk_streamlit_app_stack import CdkStreamlitAppStack

# example tests. To run these tests, uncomment this file along with the example
# resource in cdk_streamlit_app/cdk_streamlit_app_stack.py
def test_sqs_queue_created():
    app = core.App()
    stack = CdkStreamlitAppStack(app, "cdk-streamlit-app")
    template = assertions.Template.from_stack(stack)

#     template.has_resource_properties("AWS::SQS::Queue", {
#         "VisibilityTimeout": 300
#     })
