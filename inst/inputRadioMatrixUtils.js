// Toggle checkboxes by clicking on the cell they are included in
$('td').click(function() {
    var $cell = $(this);
    var $checkbox = $cell.children('input[type="checkbox"]');
    // Toggle the checkbox within the clicked cell
    $checkbox.prop("checked", !$checkbox.prop("checked"));
    
    // Trigger the change event manually to ensure Shiny detects the change
    $checkbox.change();
});
