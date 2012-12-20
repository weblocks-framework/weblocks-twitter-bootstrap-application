
// These styles need to be added dynamically because we only want them
// when JS is turned on
addCss(".datagrid td.drilldown, .datagrid th.drilldown { display: none; }");

// We should only invoke datagrid actions when selection is empty
function initiateActionOnEmptySelection(actionCode, sessionString) {
    if(selectionEmpty()) {
	initiateAction(actionCode, sessionString);
	return false;
    }
}
