## SAP Intelligent Document Processing and Insights Using Generative AI on AWS
This Guidance demonstrates how to automate unstructured document processing and subsequent auditing and analysis using AWS AI/ML and generative AI Services

## Table of Contents  
1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Prerequisites](#prerequisites)
4. [Deployment Steps](#deployment-steps)
5. [Validation](#4-testing-the-audit-bot)
6. [AWS SDK ABAP code Samples](#5-aws-sdk-for-abap-code-examples)
7. [Costs](#costs)
7. [Notices](#notices)


## Overview
SAP customers often face the challenge of manually processing documents and extracting audit insights, which can lead to reduced productivity and increased costs. This repository provides a solution for automating document processing and deriving audit insights using AWS services, helping organizations seamlessly extract, classify, and process documents in alignment with SAP's Clean Core Methodology. By integrating with AWS AI/ML and Generative AI services, this solution enables SAP customers to extend business processes both on-stack and side-by-side, driving efficiencies through automated document processing and the generation of insights with the help of audit assistants.

##  In this repository, you will find the code and guidance to:

1. **Automate Paper-to-Post Invoice Management**
Learn how to automate the invoice management process, transforming paper-based documents into digital formats and integrating with SAP using the AWS SDK for SAP ABAP.

2. **Build Smart Audit Assistants**
Leverage Amazon Bedrock's generative AI service to build intelligent audit assistants, enabling you to derive actionable insights from invoices and other documents.

# Repository contents
This repository includes the following components to help you automate document processing and derive insights using SAP and AWS services:

**ABAP Integration Code**
The provided ABAP code integrates with AWS services such as Amazon Textract, Amazon S3, Amazon Translate, and Amazon SNS using the AWS SDK for SAP ABAP. This code includes an OData service that allows a frontend application to orchestrate the document processing scenario.

**Frontend Fiori Application**
A Fiori application built using the UI5 framework is included to provide a user-friendly interface for orchestrating and managing the document processing flow.

**CDK for Streamlit App Deployment**
The AWS Cloud Development Kit (CDK) configuration is included to deploy a Streamlit application. This app interacts with the Amazon Bedrock knowledge base to derive insights from documents, providing intelligent audit assistant functionality.

*This project is intended to be sample code only. **Not for use in production.***

## Architecture
![Architecture](/assets/images/document-processing-architecture.png)

## Prerequisites
To use the solution in this repository, you need the following:

1. **AWS Account** Ensure you have an AWS account with the necessary IAM permissions to:
    - Create and manage S3 buckets
    - Access Amazon Textract, Amazon Translate, and Amazon SNS
    - Access Amazon Bedrock service with access to Antrhopic claude and Amazon titan embedding models
    - Deploy infrastructure using the AWS Cloud Development Kit (CDK) for deploying the streamlit applicaiton on a fargate container

2. Create the following S3 buckets with the following name pattern. These buckets will be used for storing the documents and will be used for the Knowledge base creation later.
    ```
    invoicekb-<sapusername>
    invoice-images-sapinsider-<sapusername>
    ```

3. Use a **SAP ECC or S/4HANA** system that is compatible with SAP NetWeaver version 7.40 or above. If you are deploying the Fiori application on the S/4 HANA system, the **SAP_UI version should be 758**; for hub deployment, use SAP Front End Server with the **SAP_UI version of 758.**

4. The [AWS SDK for SAP ABAP](https://aws.amazon.com/sdk-for-sap-abap/) is installed on the SAP ERP or S/4HANA system. For instructions on installing and configuring your SAP system with the AWS SDK for SAP ABAP, please refer to the guide [here](https://docs.aws.amazon.com/sdk-for-sapabap/latest/developer-guide/getting-started.html).

5. Ensure that ABAP Git is installed for importing the ABAP artifacts into your SAP system. For detailed installation instructions, refer to the [ABAP Git documentation](https://docs.abapgit.org/user-guide/getting-started/install.html).


6. Knowledge of SAP ABAP programming.
7. Knowledge of SAP UI5 and Fiori.

## Deployment Steps
Before deploying the ABAP and UI5 artifacts, ensure that the AWS SDK for SAP ABAP is installed and the AWS SDK for SAP ABAP profiles are configured, as detailed in the installation guide
## 1. Importing ABAP and UI5 application artifacts using abapGit  
1. Start abapGit using the program 
``` ZABAPGIT_STANDALONE ```
 ![abapGit](/assets/images/abapgit-standalone.png)
2. In the abapGit interface, click on **"New Online"** to add a new repository.
    ![abapGit-import](/assets/images/abap-git-import.png)
3. In the URL field, enter the URL of this Git repository along with the package details where you want to group those repository objects.
    ![abapGit-url](/assets/images/abap-git-step3.png)
4. After cloning the repository, click "Pull" to download the ABAP artifacts into your selected package.
    ![abapGit-pull](/assets/images/abapgit-repo-pull.png)
5. Once the pull operation is complete, you should see the list of ABAP objects.
    ![abapGit-pullrequest](/assets/images/abap-git-pull-request.png)
6. After importing the artifacts, go to the Object Navigator (SE80) and find the imported package. The package should contain SAP Gateway services, a UI5 application, and all the helper classes for integrating with AWS services such as Amazon Textract, Amazon S3, and Amazon SNS, leveraging the AWS SDK for SAP ABAP to orchestrate the document processing scenario

7. Activate the gateway service ```ZINVOICE_APP_SRV``` using transaction ```IWFND/MAINT_SERVICE```
![catalogservice](/assets/images/catalog-service.png)
8. Activate the UI5 application service in SICF.
![sicf](/assets/images/sicf-invoice-service.png)
9. Configure this application in your Fiori Launchpad according to the [SAP Fiori Launchpad guide](https://help.sap.com/doc/289ec1eb1a9b4efab8cb1bf60f6f8e03/202210.002/en-US/bde12a271f0647e799b338574cda0808.pdf).

10. With all these steps completed, you have now deployed the ABAP and UI5 application artifacts which can be used for orchestrating the document processing scenario. You can test the application from your Fiori Launchpad by accessing it through a browser.
![fioriapp](/assets/images/fiori-app.png)


## 2. Create knowledgebase using Amazon Bedrock

So far, we have seen how to deploy the Fiori app for orchestrating the document processing to intelligently process invoice documents using AWS AI/ML services. Now we will shift gears and infuse Generative AI capabilities to get insights on the SAP invoice data. One of the most common applications of Generative AI and Foundation Models (FMs) in an enterprise environment is answering questions based on the enterprise’s knowledge corpus.

Pre-trained foundation models (FMs) perform well at natural language understanding (NLU) tasks such as summarization, text generation, and question answering on a wide variety of topics. However, these models often struggle to provide accurate (without hallucinations) answers or completely fail at answering questions about content they haven’t encountered in their training data. Moreover, FMs are trained on a point-in-time snapshot of data and lack the ability to access fresh data during inference, which can result in potentially incorrect or inadequate responses.

A commonly used approach to address this limitation is the Retrieval Augmented Generation (RAG) technique. In a RAG-based approach, the user's question is converted into vector embeddings using a Foundation Model, and then a similarity search is conducted against a pre-populated vector database containing embeddings of the enterprise's knowledge corpus.

In Amazon Bedrock, there is a feature called Knowledge Base that facilitates the creation of a complete RAG workflow with just a few clicks. This simplifies the process of building a RAG solution by managing the heavy lifting. In this part of the workshop, we will use the SAP invoice data ingested into Amazon S3 to create a knowledge base using Amazon Bedrock, which will automatically create a vector database with embeddings for the data in S3.

![rag-architecture](/assets/images/rag_architecture.png)

Once the knowledge base is created, we will test it by asking questions in plain English to see the RAG process in action. We will use the Foundation Models available through Amazon Bedrock for this, specifically:

- Amazon Titan Embeddings G1 - Text for embedding the data
- Anthropic Claude v2 for generating text and responses

For creating the knowledge base refer to the the workshop guide [here](https://catalog.us-east-1.prod.workshops.aws/workshops/6bfb401c-b349-4fcd-b82d-e94f76bec635/en-US/aa4-part2genai).

## 3. Deploy the streamlit application for accessing the knowledge base

For deploying the streamlit application on a fargate navigate to the ```cdk-streamlit-app``` folder of this repository and following the instructions.

![streamlit-app](/assets/images/streamlit-cdk-architecture.png)

## 4. Testing the audit bot
Once the streamlit applicaiton is deployed, access the application using the cloud front URL. 

![audit-bot](/assets/images/audit-bot.png)


## 5. AWS SDK for ABAP code examples

Interacting with AWS services using the SDK for SAP ABAP requires only a few lines of code, but it provides a great developer experience that encapsulates all the architectural complexities. 

Create an S3 bucket using the AWS SDK for SAP ABAP

```
Data(lo_session) = /aws1/cl_rt_session_aws=>create(
                   Iv_profile_id = co_profile ).

Data(lo_s3) = /aws1/cl_s3_factory=>create( lo_session ).

lo_s3_client->createbucket(
    EXPORTING
    iv_bucket = p_bucket
    RECEIVING
    oo_output = DATA(lo_bucket)
).
```
Translate texts using Amazon Translate using automatic language detection

```
DATA(lo_xl8)     = /aws1/cl_xl8_factory=>create( lo_session ).

  CALL METHOD lo_xl8->translatetext
    EXPORTING
      iv_text               = pv_desc
      iv_sourcelanguagecode = 'auto'    " will use comprehend to do lang detection
      iv_targetlanguagecode = 'de'
    RECEIVING
      oo_output             = DATA(lo_output).

 DATA(lv_trans_desc) = lo_output->get_translatedtext( ).


```
You can use the Amazon Textract helper utility for analyzing documents asynchronously, which leverages the AWS SDK for SAP ABAP.

```
* call the Analyzedocument API in Asynchronous mode
 DATA(lo_textract) = NEW zcl_aws_textract_helper( iv_profile = 'demo').

  TRY.
      DATA(lv_jobid) = lo_textract->analyze_document_asynchronous(
          iv_bucket = p_bucket
          iv_key    = p_key
      ).

      MESSAGE i016(rp) WITH |Document Analysis started| |Mode Asynchronous|.

    CATCH /aws1/cx_texaccessdeniedex.
    CATCH /aws1/cx_texbaddocumentex.
    CATCH /aws1/cx_texdocumenttoolargeex.
    CATCH /aws1/cx_texidempotentprmmis00.
    CATCH /aws1/cx_texinternalservererr.
    CATCH /aws1/cx_texinvalidkmskeyex.
    CATCH /aws1/cx_texinvalids3objectex.
    CATCH /aws1/cx_texlimitexceededex.
    CATCH /aws1/cx_texprovthruputexcdex.
    CATCH /aws1/cx_texthrottlingex.
    CATCH /aws1/cx_texunsupporteddocex.
    CATCH /aws1/cx_texclientexc.
    CATCH /aws1/cx_texserverexc.
    CATCH /aws1/cx_rt_technical_generic.
    CATCH /aws1/cx_rt_service_generic.
  ENDTRY.

```

Get the response of the AnalyzeDocument API results and its status asynchronously

```
DATA(lv_status) = lo_textract->get_document_results_asynch( im_jobid = lv_jobid ).
```
Retrieve the key value pairs from the document 

```
data(lt_key_value_pair) = lo_textract->get_form_data( )
```
Get table data with row index, column index and cell values

```
data(lt_tables) = lo_textract->get_table_data( )

```
Retrieve the words or lines of the document

```
data(lt_words) = lo_textract->get_words( )

data(lt_lines) = lo_textract->get_lines( )

```

## Costs
_We recommend creating a [Budget](https://docs.aws.amazon.com/cost-management/latest/userguide/budgets-managing-costs.html) through [AWS Cost Explorer](https://aws.amazon.com/aws-cost-management/aws-cost-explorer/) to help manage costs. Prices are subject to change. For full details, refer to the pricing webpage for each AWS service used in this Guidance._

### Sample Cost breakdown

The following table provides a sample cost breakdown for deploying this Guidance with the default parameters in the US East (N. Virginia) Region for one month.


|AWS service	|Dimensions	|Cost [USD]	|
|---	|---	|---	|
|Amazon Textract	| 50000 Pages with invoices and receipts (Analyze Expense API)	| 500.00	|
|Amazon Translate	| Standard Real-Time Translation cost for 10 million characters | 150.00	|
|Amazon S3	| S3 Standard Cost for 50GB	| 1.20	|
|Amazon Bedrock | 1000 input/output tokens	|18.00	|
|Amazon Fargate	| 8vCPU |108.20	|
|Amazon Cloud Front|50GB Data transfer in and out	|54.00	|
|Amazon Cognito|1000 users|50.75|
|	|	|	|

### Notices
Customers are responsible for making their own independent assessment of the information in this Guidance. This Guidance: (a) is for informational purposes only, (b) represents AWS current product offerings and practices, which are subject to change without notice, and (c) does not create any commitments or assurances from AWS and its affiliates, suppliers or licensors. AWS products or services are provided “as is” without warranties, representations, or conditions of any kind, whether express or implied. AWS responsibilities and liabilities to its customers are controlled by AWS agreements, and this Guidance is not part of, nor does it modify, any agreement between AWS and its customers.


