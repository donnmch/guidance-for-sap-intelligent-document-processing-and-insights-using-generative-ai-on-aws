sap.ui.define([
	"sap/ui/model/json/JSONModel",
	"sap/ui/Device"
], function (JSONModel, Device) {
	"use strict";

	return {
		createDeviceModel : function () {
			var oModel = new JSONModel(Device);
			oModel.setDefaultBindingMode("OneWay");
			return oModel;
		},
		createCodeModel : function (sPath) {
            var oModel = new JSONModel;
			
			oModel.loadData(sPath+'/samples/code.json');
			return oModel;
		}
	};

});