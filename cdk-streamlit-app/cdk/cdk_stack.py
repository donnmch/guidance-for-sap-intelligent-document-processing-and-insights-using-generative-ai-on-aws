import platform
import aws_cdk as cdk
from aws_cdk import (
    Stack,
    aws_iam as iam,
    aws_ec2 as ec2,
    aws_ecs_patterns as ecs_patterns,
    aws_ecs as ecs,
    aws_cloudfront as cloudfront,
    aws_cloudfront_origins as origins,
    aws_elasticloadbalancingv2 as elbv2,
    aws_cognito as cognito,
    aws_secretsmanager as secretsmanager,
    SecretValue,
    aws_dynamodb as dynamodb,
    aws_acmpca as acmpca,
    aws_kms as kms,
    aws_lambda as _lambda,
    Duration,
)

from streamlit_app.config_file import Config
from aws_cdk.aws_ecr_assets import DockerImageAsset
from constructs import Construct

class CdkStack(Stack):
    def __init__(self, scope: Construct, construct_id: str, **kwargs) -> None:
        super().__init__(scope, construct_id, **kwargs)
        self.template_options.description = "SAP Intelligent Document Processing and Insights Using Generative AI on AWS (SO9548)"
        platform_mapping = {
            "x86_64": ecs.CpuArchitecture.X86_64,
            "arm64": ecs.CpuArchitecture.ARM64
        }
        # Get architecture from platform (depending the machine that runs CDK)
        architecture = platform_mapping[platform.machine()]
        # The code that defines your stack goes here
        # Build Docker image
        imageAsset = DockerImageAsset(self, "FrontendStreamlitImage",
            directory=("./streamlit_app/")
        )
        # create app execute role
        app_execute_role = iam.Role(self, "AppExecuteRole",
                                    assumed_by=iam.ServicePrincipal("ecs-tasks.amazonaws.com")                     
        )
        
        app_execute_role.add_to_policy(
            iam.PolicyStatement(
                actions=[
                    "ecr:GetAuthorizationToken",
                    "ecr:BatchCheckLayerAvailability",
                    "ecr:GetDownloadUrlForLayer",
                    "ecr:BatchGetImage",
                    "logs:CreateLogStream",
                    "logs:PutLogEvents",
                    "bedrock:Invokemodel",
                    "bedrock:*"
                ],
                resources=["*"]
            )  
        )
        
        vpcstr = self.node.try_get_context('vpc')
        
        vpc = ec2.Vpc.from_lookup(self,"VPC",vpc_id=vpcstr)
        
        # create cognito user pool
        user_pool = cognito.UserPool(self, "AuditBotUserPool",
                                     self_sign_up_enabled=True,
                                     )
        # Create Cognito client
        user_pool_client = cognito.UserPoolClient(self, "AuditBotUserPoolClient",
                                                  user_pool=user_pool,
                                                  generate_secret=True
                                                  )
        
        # Store Cognito parameters in a Secrets Manager secret
        secret = secretsmanager.Secret(self, f"{self.node.get_context('stackname')}ParamCognitoSecret",
                                       secret_object_value={
                                           "pool_id": SecretValue.unsafe_plain_text(user_pool.user_pool_id),
                                           "app_client_id": SecretValue.unsafe_plain_text(user_pool_client.user_pool_client_id),
                                           "app_client_secret": user_pool_client.user_pool_client_secret
                                       },
                                       # This secret name should be identical
                                       # to the one defined in the Streamlit
                                       # container
                                       secret_name=Config.SECRETS_MANAGER_ID
                                       )
        # grant read role for ecs task
        secret.grant_read(app_execute_role)
        
        ecs_cluster = ecs.Cluster(self, 'StreamlitAppCluster', 
                                  vpc=vpc)
        
        # create fargate service
        fargate_service = ecs_patterns.ApplicationLoadBalancedFargateService(self, "StreamlitAppService",
                        cluster=ecs_cluster,
                        runtime_platform = ecs.RuntimePlatform(
                            operating_system_family=ecs.OperatingSystemFamily.LINUX,
                            cpu_architecture=architecture),
                        task_image_options=ecs_patterns.ApplicationLoadBalancedTaskImageOptions(
                            image=ecs.ContainerImage.from_docker_image_asset(imageAsset),
                            container_port=8501,
                            task_role=app_execute_role,
                        ), 
                        task_subnets=ec2.SubnetSelection(subnet_type=ec2.SubnetType.PRIVATE_WITH_EGRESS),
                        public_load_balancer=True,
                )
        
        # Configure health check for ALB
        fargate_service.target_group.configure_health_check(
            path="/healthz"
        )
        
        cdk.CfnOutput(
            self,
            'StreamlitLoadbalancer',
            value=fargate_service.load_balancer.load_balancer_dns_name)    
        
        # Custom header object
        custom_header_name = "X-Verify-Origin"
        custom_header_value = '-'.join((self.stack_name,"StreamLitCloudFrontDistribution"))
        
        # Create a CloudFront distribution
        cloudfront_distribution = cloudfront.Distribution(self, "StreamLitCloudFrontDistribution",
            minimum_protocol_version=cloudfront.SecurityPolicyProtocol.SSL_V3,
            comment="CloudFront distribution for Streamlit frontend application",
            default_behavior=cloudfront.BehaviorOptions(
                origin=origins.LoadBalancerV2Origin(fargate_service.load_balancer, 
                    protocol_policy=cloudfront.OriginProtocolPolicy.HTTP_ONLY, 
                    http_port=80, 
                    origin_path="/", 
                    custom_headers = { custom_header_name : custom_header_value } ),
                viewer_protocol_policy=cloudfront.ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
                allowed_methods=cloudfront.AllowedMethods.ALLOW_ALL,
                cache_policy=cloudfront.CachePolicy.CACHING_DISABLED,
                origin_request_policy=cloudfront.OriginRequestPolicy.ALL_VIEWER_AND_CLOUDFRONT_2022,
                response_headers_policy=cloudfront.ResponseHeadersPolicy.CORS_ALLOW_ALL_ORIGINS,
                compress=False
            ),
        )
    
        # Output the CloudFront distribution URL
        cdk.CfnOutput(self, "Audit Bot Application URL", value=f"https://{cloudfront_distribution.domain_name}")
        cdk.CfnOutput(self, "CognitoPoolId",
                  value=user_pool.user_pool_id)
        
        # Create deny rule for ALB
        # Add a rule to deny traffic if custom header is absent
        elbv2.ApplicationListenerRule(self, "MyApplicationListenerRule",
            listener=fargate_service.listener,
            priority=1,
            conditions=[ elbv2.ListenerCondition.http_header( custom_header_name, [ custom_header_value ]) ],
            action = elbv2.ListenerAction.forward([fargate_service.target_group])
        )
        
        
        elbv2.ApplicationListenerRule(self, "RedirectApplicationListenerRule",
            listener=fargate_service.listener,
            priority=5,
            conditions=[ elbv2.ListenerCondition.path_patterns(["*"]) ],
            action = elbv2.ListenerAction.redirect(host=cloudfront_distribution.domain_name, permanent=True,protocol="HTTPS",port="443")
        )



        
 
 


