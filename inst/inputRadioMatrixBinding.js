var radioMatrixInputBinding = new Shiny.InputBinding();

$.extend(radioMatrixInputBinding, {
  find: function (scope) {
    return $(scope).find('.shiny-radiomatrix-container');
  },

  getValue: function (el) {
    // find rows in the matrix
    var $rows = $(el).find("tr.shiny-radiomatrix-row");
    var values = {};

    $rows.each(function () {
      // Get all checked checkboxes in the row
      var checkedValues = $(this).find("input:checkbox:checked").map(function () {
        return $(this).val();
      }).get(); // .get() converts jQuery object to plain array

      if (checkedValues.length === 0) { // return null or an empty array for rows with no checked boxes
        values[$(this).attr("name")] = null; // or [] for empty array
      } else {
        values[$(this).attr("name")] = checkedValues; // assign array of checked values
      }

    });

    return (values);
  },

  setValue: function (el, value) {
    // Assuming 'value' is an object where keys are row names and values are arrays of selected items
    var $rows = $(el).find("tr.shiny-radiomatrix-row");
    $rows.find("input").prop("checked", false);  // reset all checkboxes

    $rows.each(function () {
      var rowName = $(this).attr("name");
      if(value[rowName]) {
        value[rowName].forEach(function(val) {
          $(this).find("input[value='" + val + "']").prop("checked", true);
        }.bind(this)); // ensure 'this' context is correct in forEach callback
      }
    });

  },

  receiveMessage: function (el, data) {
    // TODO: implement for updating the widget from the server
  },

  subscribe: function (el, callback) {
    $(el).on('change.radioMatrixInputBinding', "input:checkbox", function (event) {
      callback();
    });
  },

  unsubscribe: function (el) {
    $(el).off('.radioMatrixInputBinding');
  },

});

Shiny.inputBindings.register(radioMatrixInputBinding, 'shiny.radioMatrixInput');
