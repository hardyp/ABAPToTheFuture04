sap.ui.define(["sap/ui/test/Opa5"],function(e){"use strict";var s="MosterList";e.createPageObjects({onTheAppPage:{actions:{},assertions:{iShouldSeeTheApp:function(){return this.waitFor({id:"app",viewName:s,success:function(){e.assert.ok(true,"The "+s+" v+
iew is displayed")},errorMessage:"Did not find the "+s+" view"})}}}})});                                                                                                                                                                                       