
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"endogenous","title":"endogenous","type":"Variables","suggested":["continuous","ordinal"],"permitted":["numeric"],"default":null,"description":{"R":"a vector of strings naming the mediators from `data`"}},{"name":"factors","title":"Exogenous Factors","type":"Variables","suggested":["nominal","ordinal"],"permitted":["factor"],"default":null,"description":{"R":"a vector of strings naming the fixed factors from `data`"}},{"name":"covs","title":"Exogenous Covariates","type":"Variables","suggested":["continuous","ordinal"],"permitted":["numeric"],"default":null,"description":{"R":"a vector of strings naming the covariates from `data`"}},{"name":"ciType","title":"Confidence Intervals","type":"List","options":[{"name":"standard","title":"Standard"},{"name":"bca","title":"Bootstrap (BC)"},{"name":"perc","title":"Bootstrap (Percent)"},{"name":"norm","title":"Bootstrap (Normal)"},{"name":"none","title":"None"}],"default":"standard","description":{"R":"Choose the confidence interval type\n"}},{"name":"ciWidth","title":"Confidence level","type":"Number","min":50,"max":99.9,"default":95,"description":{"R":"a number between 50 and 99.9 (default: 95) specifying the confidence interval width for the parameter estimates\n"}},{"name":"bootN","title":"Bootstrap Rep.","type":"Number","min":50,"default":1000,"description":{"R":"number of bootstrap samples for estimating confidence intervals\n"}},{"name":"contrasts","title":"Factors Coding","type":"Array","items":"(factors)","default":null,"template":{"type":"Group","elements":[{"name":"var","type":"Variable","content":"$key"},{"name":"type","type":"List","options":["simple","deviation","dummy","difference","helmert","repeated","polynomial"],"default":"simple"}]},"description":{"R":"a list of lists specifying the factor and type of contrast to use, one of `'deviation'`, `'simple'`, `'difference'`, `'helmert'`, `'repeated'` or `'polynomial'`\n"}},{"name":"showRealNames","title":"Names in estimates table","type":"Bool","default":true,"description":{"R":"`TRUE` or `FALSE` (default), provide raw names of the contrasts variables\n"}},{"name":"showContrastCode","title":"Contrast Coefficients tables","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide contrast coefficients tables\n"}},{"name":"endogenousTerms","title":"Models for Endogenous Vars","type":"Array","default":[[]],"template":{"type":"Terms"},"description":{"R":"a list of lists specifying the models for with the mediators as dependent variables.          \n"}}];

const view = function() {
    
    

    View.extend({
        jus: "2.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Path Analysis",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			name: "variablesupplier",
			suggested: ["continuous","nominal","ordinal"],
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "endogenous",
							height: "small",
							isTarget: true,
							events: [
								{ execute: require('./pathj.events').onChange_endogenous }
							]
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "factors",
							height: "small",
							isTarget: true,
							events: [
								{ execute: require('./pathj.events').onChange_factors }
							]
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "covs",
							height: "small",
							isTarget: true,
							events: [
								{ execute: require('./pathj.events').onChange_covariates }
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Endogenous Models",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.Supplier,
							typeName: 'Supplier',
							name: "endogenousSupplier",
							format: FormatDef.term,
							persistentItems: true,
							stretchFactor: 1,
							events: [
								{ onEvent: 'update', execute: require('./pathj.events').onUpdate_endogenousSupplier },
								{ execute: require('./pathj.events').onChange_endogenousSupplier }
							],
							controls: [
								{
									type: DefaultControls.TargetLayoutBox,
									typeName: 'TargetLayoutBox',
									transferAction: "interactions",
									controls: [
										{
											type: DefaultControls.ListBox,
											typeName: 'ListBox',
											name: "endogenousTerms",
											height: "large",
											events: [
												{ onEvent: 'listItemAdded', execute: require('./pathj.events').onEvent_nothing },
												{ onEvent: 'listItemRemoved', execute: require('./pathj.events').onEvent_nothing }
											],
											selectable: true,
											templateName: "linreg-block-template",
											template:
											{
												type: DefaultControls.LayoutBox,
												typeName: 'LayoutBox',
												margin: "normal",
												targetArea: true,
												controls: [
													{
														type: DefaultControls.Label,
														typeName: 'Label',
														label: "empty",
														name: "blockName",
														stretchFactor: 1,
														margin: "normal"
													},
													{
														type: DefaultControls.ListBox,
														typeName: 'ListBox',
														enable: "(endogenous)",
														name: "blockList",
														isTarget: true,
														valueFilter: "unique",
														height: "auto",
														ghostText: "drag variables here",
														events: [
															{ execute: require('./pathj.events').onEvent_endogenousToTerms }
														],
														template:
														{
															type: DefaultControls.TermLabel,
															typeName: 'TermLabel'
														}														
													}
												]
											}											
										}
									]
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Factors Coding",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.ListBox,
					typeName: 'ListBox',
					name: "contrasts",
					stretchFactor: 1,
					showColumnHeaders: false,
					columns: [
						{
							name: "var",
							label: null,
							selectable: false,
							stretchFactor: 1,
							maxWidth: 300,
							template:
							{
								type: DefaultControls.VariableLabel,
								typeName: 'VariableLabel'
							}							
						},
						{
							name: "type",
							label: null,
							selectable: false,
							stretchFactor: 0.5,
							template:
							{
								type: DefaultControls.ComboBox,
								typeName: 'ComboBox'
							}							
						}
					]
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "showRealNames"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "showContrastCode"
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Options",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					style: "inline",
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Confidence Intervals",
							margin: "large",
							style: "list",
							controls: [
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "ciType_standard",
									optionName: "ciType",
									optionPart: "standard"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "ciType_bootperc",
									optionName: "ciType",
									optionPart: "perc"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "ciType_bootbca",
									optionName: "ciType",
									optionPart: "bca"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "ciType_bootnorm",
									optionName: "ciType",
									optionPart: "norm"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "ciType_none",
									optionName: "ciType",
									optionPart: "none"
								},
								{
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									name: "ciWidth",
									label: "Interval",
									suffix: "%",
									format: FormatDef.number
								},
								{
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									name: "bootN",
									format: FormatDef.number
								}
							]
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
