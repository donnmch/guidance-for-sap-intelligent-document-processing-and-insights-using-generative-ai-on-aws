import boto3
from langchain_core.prompts import ChatPromptTemplate
from langchain_core.runnables import RunnablePassthrough, RunnableParallel
from langchain_core.output_parsers import StrOutputParser
from langchain_community.chat_models import BedrockChat
from langchain_community.retrievers import AmazonKnowledgeBasesRetriever

from utils.auth import Auth
from config_file import Config

# ID of Secrets Manager containing cognito parameters
secrets_manager_id = Config.SECRETS_MANAGER_ID

# Initialise CognitoAuthenticator
authenticator = Auth.get_authenticator(secrets_manager_id)
# -----------------------------------------------------------------------
# Amazon Bedrock - settings

bedrock_runtime = boto3.client(
    service_name="bedrock-runtime",
    region_name="us-east-1",
)




model_id = "anthropic.claude-3-haiku-20240307-v1:0"

model_kwargs = {
    "max_tokens": 2048,
    "temperature": 0.0,
    "top_k": 250,
    "top_p": 1,
    "stop_sequences": ["\n\nHuman"],
}
# -------------------------------------------------------------------------
# LangChain - RAG chain with citations

template = """Answer the question based only on the following context:
{context}

Question: {question}"""

prompt = ChatPromptTemplate.from_template(template)

# Amazon Bedrock - KnowledgeBase Retriever
retriever = AmazonKnowledgeBasesRetriever(
    knowledge_base_id="",  # 👈 Set your Knowledge base ID
    retrieval_config={"vectorSearchConfiguration": {"numberOfResults": 4}},
)

model = BedrockChat(
    client=bedrock_runtime,
    model_id=model_id,
    model_kwargs=model_kwargs,
)

chain = (
    RunnableParallel({"context": retriever, "question": RunnablePassthrough()})
    .assign(response=prompt | model | StrOutputParser())
    .pick(["response", "context"])
)

import streamlit as st

#
#st.set_page_config(
#    page_title="Financial Audit assistant", page_icon="assets/images.jpg"
#)

is_logged_in = authenticator.login()

if not is_logged_in:
    st.stop()

def logout():
    authenticator.logout()


st.header("Finance Audit assistant", divider="rainbow")


# Clear Chat History fuction
def clear_screen():
    st.session_state.messages = [
        {
            "role": "assistant",
            "content": "I am your audit assistant. How may I assist you today?",
        }
    ]
    
def list_kb():
    client = boto3.client('bedrock-agent')
    response = client.list_knowledge_bases(maxResults=10)
    knowledge_bases = response.get('knowledgeBaseSummaries', [])
    return {kb['name']: kb['knowledgeBaseId'] for kb in knowledge_bases}

knowledge_bases = list_kb()

with st.sidebar:

    st.logo("assets/images.jpg")
    st.button("Logout", on_click=logout)
    st.title("💬 Accounts Payable Invoice Audit Assistant")
    st.caption("Powered by Amazon Bedrock")
    streaming_on = st.toggle("Streaming")
    st.button("Clear Chat", on_click=clear_screen)
    #selected_knowledge_base = st.selectbox('Select a Knowledge Base', list(knowledge_bases.keys()))
    #selected_knowledge_base_id = knowledge_bases[selected_knowledge_base]
   

# Store LLM generated responses
if "messages" not in st.session_state.keys():
    st.session_state.messages = [
        {
            "role": "assistant",
            "content": "I am your audit assistant. How may I assist you today?",
        }
    ]

# Display or clear chat messages
for message in st.session_state.messages:
    with st.chat_message(message["role"]):
        st.write(message["content"])

# Chat Input - User Prompt
if prompt := st.chat_input():
    st.session_state.messages.append({"role": "user", "content": prompt})
    with st.chat_message("user"):
        st.write(prompt)

    if streaming_on:
        # Chain - Stream
        with st.chat_message("assistant"):
            placeholder = st.empty()
            full_response = ""
            # Chain Stream
            for chunk in chain.stream(prompt):
                if "response" in chunk:
                    full_response += chunk["response"]
                    placeholder.markdown(full_response)
                else:
                    full_context = chunk
            placeholder.markdown(full_response)
            with st.expander("Show source details >"):
                st.write(full_context)
            st.session_state.messages.append(
                {"role": "assistant", "content": full_response}
            )
    else:
        # Chain - Invoke
        with st.chat_message("assistant"):
            response = chain.invoke(prompt)
            st.write(response["response"])
            with st.expander("Show source details >"):
                st.write(response["context"])
            st.session_state.messages.append(
                {"role": "assistant", "content": response["response"]}
            )
