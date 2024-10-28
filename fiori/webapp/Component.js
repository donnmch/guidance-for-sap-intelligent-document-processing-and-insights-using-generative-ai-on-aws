sap.ui.define([
    'sap/ui/core/UIComponent',
    'aws/fin/ap/model/models',
    'sap/ui/Device'
], function (UIComponent,models,Device) {
    'use strict';
    return UIComponent.extend("aws.fin.ap.Component", {
        metadata: {
            manifest: "json"
        },
        init: function () {
            // set the device model
            this.setModel(models.createDeviceModel(), "device");
            // call base class constructor
            UIComponent.prototype.init.apply(this);

            var oRootPath =  jQuery.sap.getModulePath("aws.fin.ap");; // your resource root
		
            var oImageModel = new sap.ui.model.json.JSONModel({
                path : oRootPath,
            });

            this.setModel(oImageModel, "localresource");

            var oRouter = this.getRouter();
            oRouter.initialize();
        },
        
        getContentDensityClass : function() {
			if (this._sContentDensityClass === undefined) {
				// check whether FLP has already set the content density class; do nothing in this case
				// eslint-disable-next-line sap-no-proprietary-browser-api
				if (document.body.classList.contains("sapUiSizeCozy") || document.body.classList.contains("sapUiSizeCompact")) {
					this._sContentDensityClass = "";
				} else if (!Device.support.touch) { // apply "compact" mode if touch is not supported
					this._sContentDensityClass = "sapUiSizeCompact";
				} else {
					// "cozy" in case of touch support; default for most sap.m controls, but needed for desktop-first controls like sap.ui.table.Table
					this._sContentDensityClass = "sapUiSizeCozy";
				}
			}
			return this._sContentDensityClass;
		},

        destroy: function () {

            UIComponent.prototype.destroy.apply(this, arguments);
        }
    });
});