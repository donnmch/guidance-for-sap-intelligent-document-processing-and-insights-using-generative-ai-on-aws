sap.ui.define([
    'aws/fin/ap/controllers/BaseController',
    'sap/ui/model/json/JSONModel',
    'sap/m/MessageToast',
    'sap/m/MessageBox',
    'sap/ui/core/BusyIndicator',
    "sap/ui/core/library",
    'sap/ui/core/Fragment',
    "sap/ui/model/BindingMode"
], function (BaseController,
    JSONModel,
    MessageToast,
    MessageBox,
    BusyIndicator,
    library,
    Fragment,
    BindingMode) {
    'use strict';

    var MessageType = library.MessageType;

    return BaseController.extend("aws.fin.ap.controllers.Master", {

        onInit: function () {
            var oViewModel;
            oViewModel = new JSONModel({
                busy: false,
                delay: 0,
                showCustomListItem: false,
                showheaderToolbar: false
            });

            this.oList = this.byId("idInvoicesList");
            this.setModel(oViewModel, "masterView");

            var oModel = new JSONModel({
                email: ""
            })
            
            this.setModel(oModel, "subscription")
            this.getModel("subscription").setDefaultBindingMode(BindingMode.TwoWay)

            this.messageManager = sap.ui.getCore().getMessageManager();
            this.messageManager.registerObject(this.getView(), true)

            this.oRouter = this.getOwnerComponent().getRouter();
            this.oRouter.getRoute("start").attachMatched(this.onMasterRouteMatched, this);
            this.oRouter.getRoute("detail").attachMatched(this.onDetailRouteMatched, this);

            this.toggleUploadButtons();
        },

        toggleUploadButtons: function () {
            var oModel = this.getOwnerComponent().getModel();
            oModel.read("/Invoices/$count", {
                success: function (count) {
                    if (count > 0) {
                        this.getModel("masterView").setProperty("/showCustomListItem", false);
                        this.getModel("masterView").setProperty("/showheaderToolbar", true);
                    } else {
                        this.getModel("masterView").setProperty("/showCustomListItem", true);
                    }
                }.bind(this),
                error: function (err) {

                }
            });
        },

        onDetailRouteMatched: function (oEvent) {
            var sKey = oEvent.getParameter('arguments').invoiceId;

            this.getModel().metadataLoaded().then(
                function () {
                    this.sDesiredItemPath = this.getModel().createKey("Invoices", {
                        invoiceNumber: sKey
                    });

                    if (this.oList.getItems().length) {
                        this.setSelectedItem(this.sDesiredItemPath);
                    } else {
                        this.onListLoaded().then(function () {
                            this.setSelectedItem(this.sDesiredItemPath)
                        }.bind(this)
                        );
                    }
                }.bind(this)
            );
        },

        setSelectedItem: function (sPath) {
            var sBindingPath = "/" + sPath;
            var oSelectedItem = this.oList.getSelectedItem();
            // skip update if the current selection is already matching the object path
            if (oSelectedItem && oSelectedItem.getBindingContext().getPath() === sBindingPath) {
                return;
            }
            this.oList.getItems().some(function (oItem) {
                if (oItem.getBindingContext() && oItem.getBindingContext().getPath() === sBindingPath) {
                    this.oList.setSelectedItem(oItem);
                    return;
                }
            }.bind(this));

        },

        onListLoaded: function () {

            return new Promise(function (resolve, reject) {
                this.oList.getBinding('items').attachEventOnce("dataReceived",
                    function () {
                        if (this.oList.getItems().length) {
                            resolve(true)
                        } else {
                            reject(false)
                        }
                    }.bind(this)
                )
            }.bind(this)
            )
        },

        onMasterRouteMatched: function (oEvent) {

            this.oList.removeSelections(true);
            this.toggleLayout()
        },

        onInvoicesListSelectionChange: function (oEvent) {
            var oRouter = this.getOwnerComponent().getRouter();
            var oList = oEvent.getSource();
            var oCtx = oList.getSelectedItem().getBindingContext();
            oRouter.navTo("detail", {
                invoiceId: oCtx.getProperty("invoiceNumber")
            });
        },
        onFileUploaderChange: function (oEvent) {
            var oUploader = oEvent.getSource();
            var oFileArr = oEvent.getParameter("files");

            if (oFileArr.length > 0) {
                var oFile = oFileArr[oFileArr.length - 1];
            }
            oUploader.removeAllHeaderParameters();
            oUploader.setUseMultipart(false);
            oUploader.setSendXHR(true);
            // csrf token
            var sToken = this.getOwnerComponent().getModel().getSecurityToken()
            oUploader.addHeaderParameter(new sap.ui.unified.FileUploaderParameter(
                {
                    name: "x-csrf-token",
                    value: sToken
                }
            ));
            oUploader.addHeaderParameter(new sap.ui.unified.FileUploaderParameter(
                {
                    name: "slug",
                    value: oFile.name
                }
            ));
            // Read the file content and upload to SAP
            var that = this;

            BusyIndicator.show()
            oUploader.checkFileReadable().then(function () {
                oUploader.upload();
                oUploader.destroyHeaderParameters();
            }, function (error) {
                MessageToast.show("File cannot be read");
                BusyIndicator.hide()
            }).then(function () {
                oUploader.clear();
            }
            );
        },
        onFileUploaderUploadComplete: function (oEvent) {
            var sResponse = oEvent.getParameter("responseRaw");
            var sStatus = oEvent.getParameter("status")
            var parser = new DOMParser()

            if (sStatus === 200 || sStatus === 201) {
                BusyIndicator.hide()
                MessageToast.show("Document was uploaded and extracted sucessfully");
                this.getModel().refresh()

            } else {
                switch (sStatus) {
                    case 400:
                        var msgxml = parser.parseFromString(sResponse, "text/xml")
                        MessageBox.error(msgxml.getElementsByTagName('message')[0].innerHTML )
                        break;
                    case 500:
                        MessageBox.error("Error occured while uploading Invoice - contact Administrator")
                    default:
                        break;
                }
                BusyIndicator.hide()
            }

            this.toggleUploadButtons();
        },
        /**
         * 
         * @param {Boolean} sBusy 
         */
        setAppBusy: function (sBusy) {
            this.getModel("masterView").setProperty("/busy", sBusy);
        },
        onSubscribeToAlertsButtonPress: function (oEvent) {
            var oButton = oEvent.getSource(),
                oView = this.getView();

            // create popover
            if (!this._pPopover) {
                this._pPopover = Fragment.load({
                    id: oView.getId(),
                    name: "aws.fin.ap.fragments.popover",
                    type: "XML",
                    controller: this
                }).then(function (oPopover) {
                    oView.addDependent(oPopover);
                    return oPopover;
                });
            }
            this._pPopover.then(function (oPopover) {
                this.getModel("subscription").setProperty("/email", "");
                oPopover.openBy(oButton);
            }.bind(this)
            );

        },
        onSubscribeButtonPress: function (oEvent) {
            var oEmail = this.byId('idEmailInput');

            this.validateEmail(oEmail);
            if (!this.messageManager.getMessageModel().getData().length > 0) {
        
            this.getModel().callFunction("/SubscribetoAlerts", {
                error: function (oMsg) {
                    MessageBox.error(this.getResourceBundle().getText("msgsubscriptionerror"))
                    BusyIndicator.hide()
                },
                success: function (oMsg) {
                    BusyIndicator.hide()
                    MessageToast.show(
                        this.getResourceBundle().getText("msgsubscription"), {
                            duration: 4000
                        }
                    );
                }.bind(this),
                method: 'POST',
                urlParameters: { email: oEmail.getValue() }
              }
            );
            }
        },
        onEmailInputChange: function (oEvent) {
            var oInput = oEvent.getSource()
            this.validateEmail(oInput)
        },
        removeMessagefromTarget: function (sTarget) {
            this.messageManager.getMessageModel().getData().forEach(function (oMessage) {
                if (oMessage.target === sTarget) {
                    this.messageManager.removeMessages(oMessage)
                }
            }.bind(this)
            );
        },
        validateEmail: function (oInput) {
            var sTarget = oInput.getBindingPath("value");
            var oBinding = oInput.getBinding("value");
            var oProcessor = this.getModel("subscription");

            this.removeMessagefromTarget(sTarget);

            try {
                oBinding.getType().validateValue(oInput.getValue());

            } catch (oException) {
                this.messageManager.addMessages(new Message(
                    {
                        message: oException.message,
                        type: MessageType.Error,
                        target: sTarget,
                        processor: oProcessor
                    }
                ))
            }
        }

    });
});