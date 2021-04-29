const DEBUG=true;

const events = {
    update: function(ui) {
      console.log("general update");
      initializeAll(ui, this);
    },

    onChange_factors: function(ui) {
         updateSuppliers(ui,this);
         updateContrasts(ui,this);
    },

    onChange_endogenous: function(ui) {
       prepareEndogenousTerms(ui,this);
       updateSuppliers(ui,this);
       updateScaling(ui,this);

    },

    onChange_covariates: function(ui) {
        updateSuppliers(ui,this);
        updateScaling(ui,this)
        
    },


    onChange_endogenousSupplier: function(ui) {
       log("endogenousSupplier has changed");
       cleanEndogenousTerms(ui, this);
//       fromSupplierToEndogenousTerms(ui, this);

    },

    onUpdate_endogenousSupplier: function(ui) {
            log("endogenousSupplier update");
            let factorsList = this.cloneArray(ui.factors.value(), []);
            let covariatesList = this.cloneArray(ui.covs.value(), []);
            var variablesList = factorsList.concat(covariatesList);
            ui.endogenousSupplier.setValue(this.valuesToItems(variablesList, FormatDef.variable));

    },

     onChange_endogenousTerms: function(ui) {
      cleanRecursiveTerms(ui,this);
    },


     onEvent_nothing: function(ui) {
      console.log("I did not do anything");
    },

     onChange_nothing: function(ui) {
      console.log("I did not do anything");
    },
    onChange_varcovSupplier: function(ui) {
      console.log("varcovsup change");
       let values = this.itemsToValues(ui.varcovSupplier.value());
        this.checkPairsValue(ui.varcov, values);
      
    },
    onUpdate_varcovSupplier: function(ui) {
      console.log("varcovsup update");
      
    },


};



var initializeAll = function(ui, context) {
    
    updateSuppliers(ui,context);
    prepareEndogenousTerms(ui,context);

};




var updateSuppliers= function(ui,context) {

   // here we transfer all variables in the supplier for the endogenous models
   
    var factorsList = context.cloneArray(ui.factors.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);
    var indList = factorsList.concat(covariatesList);
    var endogenousList = context.cloneArray(ui.endogenous.value(), []);
    var allList = factorsList.concat(covariatesList).concat(endogenousList);
    ui.endogenousSupplier.setValue(context.valuesToItems(allList, FormatDef.variable));
    ui.varcovSupplier.setValue(context.valuesToItems(allList, FormatDef.variable));
    context.workspace.endogenousSupplierList=allList;
    context.workspace.varcovSupplierList=allList;
    

};

var prepareEndogenousTerms= function(ui,context) {

     // here we prepare a list of lists, one of each endogenous variable.
     // we also want to put a label to show which dependent variable the user is working on

     console.log("prepareEndogenousTerms");
     var endogenous = context.cloneArray(ui.endogenous.value(),[]);
     var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);
     
 
     // we make sure that there are enough arrays in the array list, each for each endogeneous
     var okList= [];
     for (var i = 0; i < endogenous.length; i++) {
         var aList = endogenousTerms[i] === undefined ? [] : endogenousTerms[i] ;
             okList.push(aList);
     }
      console.log(okList);
      ui.endogenousTerms.setValue(okList);    
      
     // we give a label for each endogeneous model
     labelize(ui.endogenousTerms,endogenous, "Endogenous");  
     storeComponent("endogenousTerms",endogenousTerms,context);
  
  
};


var cleanRecursiveTerms= function(ui,context) {

    console.log("cleanRecursiveTerms");
    var endogenous = context.cloneArray(ui.endogenous.value(), []);
    var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);

    for (var i = 0; i < endogenous.length; i++)
        endogenousTerms[i]=removeFromList(endogenous[i],endogenousTerms[i],context,1);
        
    ui.endogenousTerms.setValue(endogenousTerms);
    storeComponent("endogenousTerms",endogenousTerms,context);

};


var cleanEndogenousTerms= function(ui,context) {

    console.log("cleanEndogenousTerms");
    var endogenous = context.cloneArray(ui.endogenous.value(), []);
    var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);

    for (var i = 0; i < endogenous.length; i++)
        endogenousTerms[i]=removeFromList(endogenous[i],endogenousTerms[i],context,1);

    var endogenousSupplierList = context.cloneArray(context.itemsToValues(ui.endogenousSupplier.value()),[]);
    var diff = context.findChanges("endogenousSupplierList",endogenousSupplierList,context);
    if (diff.hasChanged) {
      for (var i = 0; i < endogenous.length; i++) 
           for (var j = 0; j < diff.removed.length; j++) {
                console.log(diff.removed[j]);
                endogenousTerms[i]=removeFromList(diff.removed[j],endogenousTerms[i],context,1);
           }
           
    }
    ui.endogenousTerms.setValue(endogenousTerms);
    storeComponent("endogenousTerms",endogenousTerms,context);

};






var updateScaling = function(ui,context) {
    log("updateScaling");
    var currentList = context.cloneArray(ui.scaling.value(), []);
    var variableList1 = context.cloneArray(ui.covs.value(), []);
    var variableList2 = context.cloneArray(ui.endogenous.value(), []);
    var variableList = variableList2.concat(variableList1);

    var list3 = [];
    for (let i = 0; i < variableList.length; i++) {
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j].var === variableList[i]) {
                found = currentList[j];
                break;
            }
        }
        if (found === null)
            list3.push({ var: variableList[i], type: "none" });
        else
            list3.push(found);
    }

    ui.scaling.setValue(list3);
};

var updateContrasts = function(ui, context) {
    var currentList = context.cloneArray(ui.contrasts.value(), []);
    var variableList = context.cloneArray(ui.factors.value(), [])
    var list3 = [];
    for (let i = 0; i < variableList.length; i++) {
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j].var === variableList[i]) {
                found = currentList[j];
                break;
            }
        }
        if (found === null)
            list3.push({ var: variableList[i], type: "simple" });
        else
            list3.push(found);
    }

    ui.contrasts.setValue(list3);
};



// helper functions

// add an item or a list of items (quantum) to a list (cosmos).
// it tries to understand what kind of input it has, but it is not clear
// when it works
var addToList = function(quantum, cosmos, context) {
  
    cosmos = normalize(context.cloneArray(cosmos));
    quantum = normalize(context.cloneArray(quantum));
    
    for (var i = 0; i < quantum.length; i++) {
          if (dim(quantum[i])===0)
              cosmos.push([quantum[i]]);
          if (dim(quantum[i])===1)
              cosmos.push(quantum[i]);
          }
    return unique(cosmos);
};


var removeFromMultiList = function(quantum, cosmos, context, strict = 1) {

    var cosmos = context.cloneArray(cosmos);
    var dimq = dim(quantum);
        for (var j = 0; j < cosmos.length; j++) 
           cosmos[j]=removeFromList(quantum,cosmos[j],context, strict);
    return(cosmos);
};



// remove a list or a item from list
// order=0 remove only if term and target term are equal
// order>0 remove if term length>=order 
// for instance, order=1 remove any matching interaction with terms, keeps main effects
// order=2 remove from 3-way interaction on (keep up to 2-way interactions)

var removeFromList = function(quantum, cosmos, context, order = 1) {

     cosmos=normalize(cosmos);
     quantum=normalize(quantum);
     if (cosmos===undefined)
        return([]);
     var cosmos = context.cloneArray(cosmos);
       for (var i = 0; i < cosmos.length; i++) {
          if (cosmos[i]===undefined)
             break;
          var aCosmos = context.cloneArray(cosmos[i]);
           for (var k = 0; k < quantum.length; k++) {
             var  test = order === 0 ? FormatDef.term.isEqual(aCosmos,quantum[k]) : FormatDef.term.contains(aCosmos,quantum[k]);
                 if (test && (aCosmos.length >= order)) {
                        cosmos.splice(i, 1);
                        i -= 1;
                    break;    
                    }
          }
            
       }
  
    return(cosmos);
};




var unique=function(arr) {
    var u = {}, a = [];
    for(var i = 0, l = arr.length; i < l; ++i){
        var prop=ssort(JSON.stringify(arr[i]));
        if(!u.hasOwnProperty(prop) && arr[i].length>0) {
            a.push(arr[i]);
            u[prop] = 1;
        }
    }
    return a;
};

var ssort= function(str){
  str = str.replace(/[`\[\]"\\\/]/gi, '');
  var arr = str.split(',');
  var sorted = arr.sort();
  return sorted.join('');
}

var labelize = function(widget, labels, prefix) {

     widget.applyToItems(0, (item, index) => {
           item.controls[0].setPropertyValue("label",prefix +" = "+labels[index]);
        });
};


var flatMulti = function(cosmos,context) {
  var light = []
  for (var i=0 ; i < cosmos.length; i++) {
    light=addToList(light,cosmos[i],context);
  }
  return unique(light);
};

var combineOne = function(values, mod, context) {
        if (mod===undefined)
            return(values);
        var list = unique(values.concat([mod]));
        for (var i = 0; i < values.length; i++) {
            var newValue = context.clone(mod);
            var value = values[i];
            if (context.listContains(value,newValue[0],FormatDef.term)===false)
                   if (FormatDef.term.isEqual(newValue,value)===false) {
                      if (Array.isArray(value)) 
                          newValue = newValue.concat(value);
                      else
                         newValue.push(value);   
            list.push(newValue);
            }
        }
        return unique(list);
};

var getInteractions = function(aList,context,order=2) {
  
  var iList = context.getCombinations(aList);
  for (var i = 0; i < iList.length; i++ )
          if (iList[i].length===1 || iList[i].length>order) {
              iList.splice(i, 1);
               i -= 1;
          }
 return(iList);
  
};

var normalize = function(cosmos) {

  if (cosmos===undefined)
          return [];
  if (dim(cosmos)===0)
          cosmos=[cosmos]
          
        for (var i = 0; i < cosmos.length; i++) {
            var aValue = cosmos[i];
            var newValue=dim(aValue)>0 ? aValue : [aValue];
            cosmos[i]=newValue
        }
        return cosmos;
}

// get interaction between two lists
// order==0 include main effects
// 1  only interaction


var combine = function(cosmos1, cosmos2 , context, order=2) {
        if (cosmos1===cosmos2)
               return;
        if (cosmos1===undefined)
               return order===0 ? cosmos2 : [] ;
        if (cosmos2===undefined)
               return order===0 ? cosmos1 : [] ;
        cosmos1 = normalize(cosmos1)
        cosmos2 = normalize(cosmos2)        

        var light=[];
        for (var i = 0; i < cosmos1.length; i++) {
            var aValue1 = context.clone(cosmos1[i]);
            for (var j = 0 ; j < cosmos2.length; j ++) {
            var aValue2 = context.clone(cosmos2[j]);
            var join = aValue1.concat(aValue2);
            if (join.length===unique(join).length)
                   light.push(join);
              }
            }

        if (order===0)
              light = cosmos1.concat(cosmos2).concat(light);
        return unique(light);
};


var dim = function(aList) {

    if (!Array.isArray(aList))
           return(0);
    if (!Array.isArray(aList[0]))
           return(1);
    if (!Array.isArray(aList[0][0]))
           return(2);
    if (!Array.isArray(aList[0][0][0]))
           return(3);
    if (!Array.isArray(aList[0][0][0][0]))
           return(4);

  
    return(value);
};

var findChangesMulti= function(id,cosmos,context,save=true) {

  var old = context.workspace[id];
 if (old===undefined)
        old=[];
      
  var light = [];
  var len = Math.max(cosmos.length,old.length);
  var changeIndex = -1;
  for (var i = 0; i < len; i++) {
    var photon=[];
    photon.added = removeFromList(old[i], cosmos[i], context, 0);
    photon.removed = removeFromList(cosmos[i],old[i], context, 0);
    if  (photon.added.length > 0 || photon.removed.length > 0) 
          changeIndex = i
    light[i] = photon;
}
  light = { changes: light, index: changeIndex };
  if (save)
        storeComponent(id,cosmos,context);
  return(light);
};

var storeComponent = function(id,cosmos,context) {
                    context.workspace[id]=cosmos;
};

var log=function(obj) {
    if (DEBUG)
      console.log(obj);
};

var dlog=function(obj) {
    if (DEBUG)
      console.log(obj);
};

var flashMGridOptionListControl = function(widget,note=false) {
        var i =widget.getSelectedRowIndices();
        if (i.length==0) i=0;
        widget.controls[i].controls[1].$el[0].style.backgroundColor="#ffcccc";
        widget.controls[i].controls[1].$el[0].style.borderColor="red";
        if (note!==false)
           note.$el[0].style.visibility="visible";
        

};

var unflashMGridOptionListControl = function(widget,note=false) {
        var i =widget.getSelectedRowIndices();
        if (i.length==0) i=0;
        widget.controls[i].controls[1].$el[0].style.backgroundColor="white";
        widget.controls[i].controls[1].$el[0].style.borderColor="rgb(46,138,199)";
        if (note!==false)
           note.$el[0].style.visibility="hidden";
};

var   cleanInteractions= function(quantum,cosmos,context){
  if (quantum===undefined)
    return(cosmos);
    
  var deny= context.getCombinations(quantum)  
  for (var i=0; i < deny.length; i++) {
    if (deny[i].length==1) {
      deny.splice(i, 1);
      i -= 1;
    }
  }
  cosmos=context.cloneArray(cosmos)
  for (var j=0; j< cosmos.length; j++) {
    for (var i=0; i < deny.length; i++) {
      if (FormatDef.term.contains(cosmos[j],deny[i])) {
        cosmos.splice(j,1);
        j -= 1;
        break;
      }
  }
}
 return cosmos;
  
};


module.exports = events;

