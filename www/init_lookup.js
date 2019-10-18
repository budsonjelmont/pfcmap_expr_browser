// initialize lookups
function processData(allText) {
    var record_num = 1;  // or however many elements there are in each row
    var allTextLines = allText.split(/\r\n|\n/);
    // ***** begin CSV parse ****
    // var entries = allTextLines[0].split(',');
    // var lines = [];
    // var headings = entries.splice(0,record_num);
    // while (entries.length>0) {
    //     var tarr = [];
    //     for (var j=0; j<record_num; j++) {
    //         tarr.push(headings[j]+":"+entries.shift());
    //     }
    //     lines.push(tarr);
    // }
    // console.log(lines);
    // ****** end CSV parse *****
    var lines = allTextLines;

    acopts = $.map(lines, function(gene,row){
     console.log(gene);
     return {
      label: gene,
      value: gene
     }
    });

    $( '#genequery' ).autocomplete({
      source: acopts,
      minLength: 2
    });

    $.widget( 'ui.autocomplete', $.ui.autocomplete, {
   _renderItem: function( ul, item ) {
       return $( '<li>' )
         // .attr( 'data-value', item.value )
        .addClass('autocomplete-ul')
        .appendTo( ul );
   },

   _renderMenu: function( ul, items ) {
      var that = this;
      $.each( items, function( index, item ) {
         that._renderItemData( ul, item );
      });
      $( ul )
         .addClass('autocomplete-ul')
         .attr( 'tabindex', -1 )
         .find( 'li:odd' ).addClass( 'row-alt' );
   },
 });
}

$(function() {
//$(document).on('shiny:connected', function(event) {

 $.ajax({
  type : 'get',
  //url : 'https://pintolab03.mssm.edu/testdb/php/allgenequery.php',
  //dataType : 'json',
  url: './data/genes.csv',
  dataType: 'text',
  success: function(data) {
    processData(data);
  }

/*  success : function(availableTags){
   // reformat query JSON
   acopts = $.map(availableTags, function(gene,row){
    console.log(gene);
    return {
     label: gene['ensgid'],
     value: gene['id']
    }
   })
   $( '#genesearch' ).autocomplete({
    source: acopts,
    minLength: 2
   });
  //
    $.widget( 'ui.autocomplete', $.ui.autocomplete, {
   _renderItem: function( ul, item ) {
       return $( '<li>' )
         // .attr( 'data-value', item.value )
        .addClass('autocomplete-ul')
        .appendTo( ul );
   },
  //
   _renderMenu: function( ul, items ) {
      var that = this;
      $.each( items, function( index, item ) {
         that._renderItemData( ul, item );
      });
      $( ul )
         .addClass('autocomplete-ul')
         .attr( 'tabindex', -1 )
         .find( 'li:odd' ).addClass( 'row-alt' );

   },
 });

}*/
 });
});
// End AJAX call
