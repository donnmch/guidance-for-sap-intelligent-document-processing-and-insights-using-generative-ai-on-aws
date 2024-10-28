## Deploy Streamlit App

This app can be used as a starting point to easily create and deploy a GenAI demo, with web interface and user authentication. It is written in python only, with cdk template to deploy on AWS.

It deploys a basic Streamlit app, and contains the following components:

   - The Streamlit app in ECS/Fargate, behind an ALB and CloudFront
   - A Cognito user pool in which you can manage users

By default, the Streamlit app has the following features:

    - Authentication through Cognito
    - Connection to Bedrock
## Usage

In the docker_app folder, you will find the streamlit app. You can run it locally or with docker.

Note: for the docker version to run, you will need to give appropriate permissions to the container for bedrock access. This is not implemented yet.

In the main folder, you will find a cdk template to deploy the app on ECS / ALB.

Prerequisites:

* python >= 3.8
* docker
* use a Chrome browser for development
* `anthropic.claude-v2` model activated in Amazon Bedrock in your AWS account
* You also need to install the AWS Command Line Interface (CLI), the AWS Cloud Development KIT (CDK), and to configure the AWS CLI on your development environment. One way to configure the AWS CLI is to get your access key through the AWS console, and use the `aws configure` command in your terminal to setup your credentials.

To deploy:

1. Edit `strealit_app/config_file.py`, choose a `STACK_NAME` and a `CUSTOM_HEADER_VALUE`.

2. Update the knowledge base id in the `streamlit_app/main.py`

3. Install dependencies
 
```
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

3. Deploy the cdk template

```
cdk bootstrap
cdk deploy
```

The deployment takes 5 to 10 minutes.

Make a note of the output, in which you will find the CloudFront distribution URL
and the Cognito user pool id.

4. Create a user in the Cognito UserPool that has been created. You can perform this action from your AWS Console. 
5. From your browser, connect to the CloudFront distribution url.
6. Log in to the Streamlit app with the user you have created in Cognito.