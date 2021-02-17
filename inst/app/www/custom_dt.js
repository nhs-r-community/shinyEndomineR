$( document ).ready(function() {
  var ncols = table.columns().count();
  var tbl = table.table().node();
  var tblID = $(tbl).closest('.datatables').attr('id');
  table.on('click', 'tbody td', function(){
    // if the column is selected, deselect it:
    if(table.column(this, {selected: true}).length){
      table.column(this).deselect();
      // otherwise, select the column unless it's among the last two columns:"
      } else if([ncols-1, ncols-2].indexOf(table.column(this).index()) === -1){
        table.column(this).select();
      }
      // send selected columns to Shiny
      var indexes = table.columns({selected:true}).indexes();
      var indices = Array(indexes.length);
      for(var i = 0; i < indices.length; ++i){
        indices[i] = indexes[i];
      }
      Shiny.setInputValue(tblID + '_columns_selected', indices);
      var checkboxes = document.getElementsByName('row_selected');
      var checkboxesChecked = [];
      for (var i=0; i<checkboxes.length; i++) {
        if (checkboxes[i].checked) {
          checkboxesChecked.push(checkboxes[i].value);
          }
        }
        Shiny.onInputChange('checked_rows',checkboxesChecked);
  });
});
